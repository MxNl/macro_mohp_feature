drop_disconnected_river_networks <-
  function(river_networks, studyarea, table_name_read, depends_on = NULL) {
    test <- FALSE
    if (test) {
      river_networks <- tar_read(river_networks_clean)
      studyarea <- tar_read(studyarea_outline)
      table_name <- LINES_CLEAN
    }

    length(depends_on)

    get_table_from_postgress(table_name_read) %>%
      query_result_as_sf() %>%
      filter(st_intersects(., st_cast(studyarea, "MULTILINESTRING"), sparse = FALSE)[, 1]) %>%
      select(-connected_id) %>%
      st_intersection(river_networks) %>%
      add_feature_index_column()
  }


run_query_connected <-
  function(table_name_read, table_name_destination) {
    query <- glue::glue("
      CREATE TABLE {table_name_destination} AS (
	      WITH endpoints AS (
	        SELECT
	          ST_Collect(ST_StartPoint(geometry),
	          ST_EndPoint(geometry)) AS geometry
	        FROM {table_name_read}
	      ), clusters AS (
	        SELECT
	          unnest(ST_ClusterWithin(geometry, 1e-8)) AS geometry
	        FROM endpoints
	      ), clusters_with_ids AS (
	        SELECT
	          row_number() OVER () AS {table_name_destination},
	          ST_CollectionHomogenize(geometry) AS geometry
	        FROM clusters
	      )
      	SELECT
		      {table_name_destination},
		      ST_Collect({table_name_read}.geometry) AS geometry
	      FROM
	        {table_name_read}
	        LEFT JOIN
	        clusters_with_ids
	        ON ST_Intersects({table_name_read}.geometry, clusters_with_ids.geometry)
	      GROUP BY {table_name_destination}
      )
    ")
    create_table(query, table_name_destination)
  }

write_connected_but_merged_river_networks <-
  function(table_name_read, table_name_destination, depends_on) {
    length(depends_on)
    run_query_connected(
      table_name_read, 
      table_name_destination
      )
  }

merge_same_strahler_segments <-
  function(depends_on) {

    length(depends_on)

    ###### Test
    # sf_lines <- tar_read(river_networks_dissolved_junctions_after)
    # query <- tar_read(linemerge_query)
    ###
    connect_to_database() %>%
      run_query_linemerge_by_streamorder() %>%
      prepare_lines()
  }

nearest_neighbours_between <- function(
  table_name_destination,
  left_table,
  left_columns,
  right_table,
  right_columns = NULL,
  stream_order_id = NULL,
  right_table_is_long_format = FALSE,
  n = 1,
  depends_on
) {
  length(depends_on)
  connection <- connect_to_database()

  format_select <- function(table, columns) {
    validate(table, columns)
    select_statements <- paste0(table, '.', columns, ' AS ', table, '_', columns)
    paste0(select_statements, collapse = ', ')
  }

  left_select <- format_select(left_table, left_columns) %>% 
    str_replace("grid_grid_id", "grid_id") #TODO how can we make this more elegant?
  right_select <- NULL
  if (!is.null(right_columns)) {
    right_select <- format_select(right_table, right_columns)
  }  
  select <- list(left_select, right_select) %>% compact() %>% str_c(collapse=', ')
  
  left_geometry <- geometry_of(left_table)
  right_geometry <- geometry_of(right_table)
  by_streamorder <- ifelse(is.null(stream_order_id), FALSE, TRUE)

  distance <- glue::glue('ST_Distance({left_geometry}, {right_geometry}) AS distance_meters')
  by_streamorder_clause <- ifelse(by_streamorder & right_table_is_long_format, glue::glue("AND {right_table}.stream_order_id = {stream_order_id}"), '')
  table_destination <- composite_name(table_name_destination, stream_order_id)
  
  query <- glue::glue("
    CREATE TABLE {table_destination} AS (
      SELECT
        {select},
        {distance}
      FROM
        {left_table}
        CROSS JOIN LATERAL (
          SELECT
            *
          FROM
            {right_table}
          WHERE
            True {by_streamorder_clause}
          ORDER BY
            {left_geometry} <-> {right_geometry}
          LIMIT
            {n}
        ) AS {right_table}
     );
  ")
  create_table(query, table_destination)
  set_index(connection, table_destination, "grid_id")
  Sys.time()
}

composite_name <- 
  function(table_name, stream_order_id) {
  ifelse(is.null(stream_order_id), table_name, glue::glue('{table_name}_id_{stream_order_id}'))
}

geometry_of <- 
  function(table) {
    paste0(table, '.geometry')
  }

make_thiessen_catchments <- function(stream_order_id, depends_on) {
  length(depends_on)
  connection <- connect_to_database()

  table_name_destination <- composite_name(THIESSEN_CATCHMENTS_TABLE, stream_order_id)
  nearest_neighbours <- composite_name(NN_GRID_RIVERS_TABLE, stream_order_id)
  
  query <- glue::glue("
    CREATE TABLE {table_name_destination} AS (
    	SELECT 
	  		nn.{LINES_BY_STREAMORDER}_feature_id,
	  		ST_ExteriorRing(ST_Union(grid.geometry)) AS geometry
	  	FROM
			  {GRID_POLYGONS_TABLE} AS grid
	  		INNER JOIN {nearest_neighbours} AS nn
	  		USING(grid_id)
	  	GROUP BY 1
    );
  ")
  create_table(query, table_name_destination)
  set_geo_index(connection, table_name_destination)
  Sys.time()
}

validate <- function(table, columns) {
  
  connection <- connect_to_database()
  
  if (!DBI::dbExistsTable(connection, table)) { stop(paste("Table", table, "not in database")) }
  
  present_columns <- DBI::dbListFields(connection, name = table)
  if (!('geometry' %in% present_columns)) { stop(paste("Table", table, "does not have column 'geometry'")) }
  
  missing_columns <- setdiff(columns, present_columns)
  
  if (length(missing_columns) != 0) {
    message <- paste('Invalid columns selected:', paste(missing_columns, collapse = ', '), 'not in', table)
    stop(message)
    
  }
}

calculate_lateral_position_stream_divide_distance <- 
  function(stream_order_id, 
           depends_on = NULL) {
  length(depends_on)
  
  connection <- connect_to_database()
  
  catchments_table <- composite_name(NN_GRID_CATCHMENTS_TABLE, stream_order_id)
  rivers_table <- composite_name(NN_GRID_RIVERS_TABLE, stream_order_id)
  table_name_destination <- composite_name(MOHP_FEATURES_TABLE, stream_order_id)
  query <- glue::glue("
    CREATE TABLE {table_name_destination} AS (
      SELECT
        grid_polygons.grid_id,
        grid_polygons.geometry,
        rivers.distance_meters / (rivers.distance_meters + catchments.distance_meters) AS lateral_position,
        rivers.distance_meters + catchments.distance_meters AS divide_stream_distance
      FROM
        {catchments_table} AS catchments
        INNER JOIN {rivers_table} AS rivers USING(grid_id)
        INNER JOIN {GRID_POLYGONS_TABLE} USING(grid_id)
    );
  ")
  create_table(query, table_name_destination)
  Sys.time()
}


set_geo_indices <- function(table_names, index_columns = NULL, depends_on) {
  length(depends_on)

  if (is.null(index_columns)) {
    index_columns <- rep("geometry", length(table_names))
  }

  connection <- connect_to_database()
  map2(table_names, index_columns, ~ set_geo_index(connection, .x, .y))
}

set_geo_index <- function(connection, table_name, index_column = "geometry") {
  DBI::dbExecute(connection, glue::glue("DROP INDEX IF EXISTS {table_name}_geometry_idx;"))
  DBI::dbExecute(connection, glue::glue("CREATE INDEX {table_name}_geometry_idx ON {table_name} USING GIST ({index_column});"))
}

set_index <- function(connection, table_name, column) {
  DBI::dbExecute(connection, glue::glue("DROP INDEX IF EXISTS {column}_idx;"))
  DBI::dbExecute(connection, glue::glue("CREATE UNIQUE INDEX {column}_idx ON {table_name} ({column});"))
}


read_lateral_position_stream_divide_distance_from_db <- 
  function(table_name_source_prefix, streamorder, depends_on = NULL) {
    
    length(depends_on)
    
    composite_name(
      table_name_source_prefix, 
      streamorder) %>% 
      get_table_from_postgress() %>% 
      query_result_as_sf()
  }