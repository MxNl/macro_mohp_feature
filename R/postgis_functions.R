drop_disconnected_river_networks <-
  function(river_networks, studyarea, table_name_source, depends_on = NULL) {
    test <- FALSE
    if (test) {
      river_networks <- tar_read(river_networks_clean)
      studyarea <- tar_read(studyarea_outline)
      table_name <- LINES_CLEAN
    }
    ####
    
    length(depends_on)
    
    get_table_from_postgress(table_name_source) %>% 
      query_result_as_sf() %>%
      filter(st_intersects(., st_cast(studyarea, "LINESTRING"), sparse = FALSE)[,1]) %>% 
      select(-connected_id) %>%
      st_intersection(river_networks) %>%
      add_feature_index_column()
  }



run_query_connected <- 
  function(table_name_source, table_name_create) {
    database <- connect_to_database()
    DBI::dbExecute(database, glue::glue("DROP TABLE IF EXISTS {table_name_create}"))
    DBI::dbExecute(database, glue::glue("
      CREATE TABLE {table_name_create} AS (
	      WITH endpoints AS (
	        SELECT
	          ST_Collect(ST_StartPoint(geometry),
	          ST_EndPoint(geometry)) AS geometry
	        FROM {table_name_source}
	      ), clusters AS (
	        SELECT
	          unnest(ST_ClusterWithin(geometry, 1e-8)) AS geometry
	        FROM endpoints
	      ), clusters_with_ids AS (
	        SELECT
	          row_number() OVER () AS {table_name_create},
	          ST_CollectionHomogenize(geometry) AS geometry
	        FROM clusters
	      )
      	SELECT
		      {table_name_create},
		      ST_Collect({table_name_source}.geometry) AS geometry
	      FROM
	        {table_name_source}
	        LEFT JOIN
	        clusters_with_ids
	        ON ST_Intersects({table_name_source}.geometry, clusters_with_ids.geometry)
	      GROUP BY {table_name_create}
      )
    "))
  }

write_connected_but_merged_river_networks <- 
  function(table_name_source, table_name_create, depends_on){
    length(depends_on)
    run_query_connected(table_name_source, table_name_create)
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
  left_table,
  left_columns,
  right_table,
  right_columns,
  stream_order_id,
  n = 1,
  as_wkt = TRUE,
  depends_on
) {
  length(depends_on)
  connection <- connect_to_database()
  if (!("PqConnection" %in% class(connection))) { stop("Supply a connection of class 'PqConnection'") }
  if (!(class(n) %in% c('numeric', 'integer'))) { stop('n must be numeric or integer') }
  if (n %% 1 != 0) { stop(paste('n cannot be interpreted as integer, got:', n)) }
  
  number_of_nearest_neighbours <- n
  
  validate <- function(table, columns) {
    if (!DBI::dbExistsTable(connection, table)) { stop(paste("Table", table, "not in database")) }
    
    present_columns <- DBI::dbListFields(connection, name = table)
    if (!('geometry' %in% present_columns)) { stop(paste("Table", table, "does not have column 'geometry'")) }
    
    missing_columns <- setdiff(columns, present_columns)
    
    if (length(missing_columns) != 0) {
      message <- paste('Invalid columns selected:', paste(missing_columns, collapse = ', '), 'not in', table)
      stop(message)
    }
  }
  
  format_select <- function(table, columns) {
    validate(table, columns)
    select_statements <- paste0(table, '.', columns, ' AS ', table, '_', columns)
    paste0(select_statements, collapse = ', ')
  }
  
  geometry_of <- function(table) { paste0(table, '.geometry') }
  
  left_select <- format_select(left_table, left_columns)
  right_select <- format_select(right_table, right_columns)
  left_geometry <- geometry_of(left_table)
  right_geometry <- geometry_of(right_table)
  
  distance <- glue::glue('ST_Distance({left_geometry}, {right_geometry}) AS distance_meters')
  
  query <- glue::glue("
  CREATE TABLE nearest_neighbours_streamorder_id_{stream_order_id} AS (
    SELECT
      {left_select},
      {right_select},
      {distance}
    FROM
      {left_table}
      CROSS JOIN LATERAL (
        SELECT
          *
        FROM
          {right_table}
        WHERE
          {right_table}.stream_order_id = {stream_order_id}
        ORDER BY
          {left_geometry} <-> {right_geometry}
        LIMIT
          {number_of_nearest_neighbours}
      ) AS {right_table}
    );
  ")
  print(query)
  DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS nearest_neighbours_streamorder_id_{stream_order_id};"))
  result <- tibble::as_tibble(DBI::dbGetQuery(connection, query))
  
  if (!as_wkt) { return(result) }
  
  is_pq_geometry <- function(column) { class(column) == "pq_geometry" }
  from_wkb <- purrr::partial(sf::st_as_sfc, EWKB = TRUE)
  dplyr::mutate(result, dplyr::across(where(is_pq_geometry), from_wkb))
}

make_thiessen_catchments <- function(
  left_table,
  stream_order_id,
  as_wkt = TRUE,
  depends_on
) {
  length(depends_on)
  connection <- connect_to_database()

  query <- glue::glue("
    CREATE TABLE thiessen_catchments_{stream_order_id} AS (
  	  WITH right_table AS (
  		  SELECT
  			  river_network_by_streamorder_feature_id,
  			  grid_id
  		  FROM nearest_neighbours_streamorder_id_{stream_order_id}
    	), joined_table AS (
  		  SELECT
  		  	*
  	  	FROM {left_table}
  	  		LEFT JOIN right_table
  	  		ON grid_id = id
    	), catchments AS (
  	  	SELECT 
  	  		river_network_by_streamorder_feature_id,
  	  		ST_Union(geometry) AS geometry
  	  	FROM joined_table
  	  		GROUP BY 
  	  			river_network_by_streamorder_feature_id
    	), catchment_boundary AS(
      	SELECT
      		river_network_by_streamorder_feature_id,
      		ST_ExteriorRing(geometry) AS geometry
      		FROM catchments
      	)
      	SELECT
      		*
      	FROM catchment_boundary
      )
  ")
  print(query)
  DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS thiessen_catchments_{stream_order_id};"))
  result <- tibble::as_tibble(DBI::dbGetQuery(connection, query))
  
  if (!as_wkt) { return(result) }
  
  is_pq_geometry <- function(column) { class(column) == "pq_geometry" }
  from_wkb <- purrr::partial(sf::st_as_sfc, EWKB = TRUE)
  dplyr::mutate(result, dplyr::across(where(is_pq_geometry), from_wkb))
}


set_geo_indices <- function(table_names, index_columns = NULL, depends_on) {
  length(depends_on)
  
  if(is.null(index_columns)) {
    index_columns <- rep("geometry", length(table_names))
    }
  
  connection <- connect_to_database()
  map2(table_names, index_columns,  ~ {
    DBI::dbExecute(connection, glue::glue("DROP INDEX IF EXISTS {.x}_geometry_idx;"))
    DBI::dbExecute(connection, glue::glue("CREATE INDEX {.x}_geometry_idx ON {.x} USING GIST ({.y});"))
  })
}

