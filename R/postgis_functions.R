make_grid_polygons_and_write_to_db <- 
  function(reference_raster, table_name_destination, studyarea, depends_on = NULL) {
    
    length(depends_on)
    
    reference_raster %>% 
      st_as_stars() %>% 
      st_transform(CRS_REFERENCE) %>%
      st_as_sf() %>% 
      select(-layer) %>% 
      write_to_table(table_name_destination)
    
      add_row_id_in_db_table(table_name_destination)
    
    Sys.time()
  }

add_row_id_in_db_table <- 
  function(table_name, column_name = "grid_id") {
    query <- glue::glue(
      "
      ALTER TABLE {table_name}
      ADD grid_id serial;
      "
      )
    connection <- connect_to_database()
    
    DBI::dbExecute(connection, query)
    
    if (!is.null(column_name)) {
      set_index(table_name, column_name, connection)
    }
    DBI::dbDisconnect(connection)
      
  } 


run_query_connected <- function(table_name_read, table_name_destination, table_name_studyarea) {
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
	          row_number() OVER () AS connected_id,
	          ST_CollectionHomogenize(geometry) AS geometry
	        FROM clusters
	      ), connected_merged AS (
      	SELECT
		      connected_id,
		      ST_Collect({table_name_read}.geometry) AS geometry
	      FROM
	        {table_name_read}
	        LEFT JOIN
	        clusters_with_ids
	        ON ST_Intersects({table_name_read}.geometry, clusters_with_ids.geometry)
	      GROUP BY connected_id
        ), only_connected AS (
	        SELECT
		        a.geometry
	        FROM connected_merged a, {table_name_studyarea} b
	    	  WHERE ST_Intersects(a.geometry, ST_ExteriorRing(b.geometry))
        )
	      SELECT
		      feature_id,
	      	strahler,
		      a.geometry AS geometry
	      FROM {table_name_read} a, only_connected b
		    WHERE ST_Within(a.geometry, b.geometry)
      )
    ")
  # print(query)
  create_table(query, table_name_destination)
}

write_connected_river_networks <- function(table_name_read,
                                           table_name_destination,
                                           table_name_studyarea,
                                           depends_on) {
  length(depends_on)
  run_query_connected(
    table_name_read,
    table_name_destination,
    table_name_studyarea
  )
  Sys.time()
}

merge_same_strahler_segments <- function(table_name_destination, table_source, river_basin_name, depends_on = NULL) {

  length(depends_on)
  
  table_name_destination <-
    table_name_destination %>%
    str_c(river_basin_name, sep = "_")

  query <- 
    glue::glue("
    CREATE TABLE {table_name_destination} AS (
    WITH filtered AS(
      SELECT
        *
      FROM {table_source}
      WHERE river_basin_name = '{river_basin_name}'
    ),
    collected AS (
    SELECT strahler,
        ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry
    FROM filtered
    GROUP BY strahler
),
-- base table
clusters AS (
    SELECT row_number() OVER (
            ORDER BY strahler
        ) AS cluster_id,
        strahler,
        CASE
            WHEN GeometryType(ST_LineMerge(geometry)) = 'MULTILINESTRING' THEN true
            ELSE false
        END AS is_multilinestring,
        geometry
    FROM collected
),
-- processing for clusters that are multilinestrings and have splitpoints
cluster_lines_without_id AS (
    SELECT cluster_id,
        (ST_Dump(geometry)).geom AS geometry,
        strahler,
        is_multilinestring
    FROM clusters
),
cluster_lines as (
    SELECT row_number() OVER (
            ORDER BY is_multilinestring
        ) AS line_id,
        *
    FROM cluster_lines_without_id
),
split_points_single AS (
    SELECT DISTINCT l.cluster_id AS cluster_id,
        ST_Intersection(l.geometry, r.geometry) AS geometry
    FROM cluster_lines AS l
        CROSS JOIN cluster_lines AS r
    WHERE (
            l.strahler < r.strahler
            AND ST_Intersects(l.geometry, r.geometry)
            AND l.cluster_id != r.cluster_id
        )
),
--Test for multiple splitpoints per cluster id
split_points AS (
    SELECT
        cluster_id,
        ST_CollectionHomogenize(ST_Collect(geometry)) AS geometry
    FROM split_points_single
    GROUP BY cluster_id
),
lines_touching_splitpoints AS (
    SELECT cluster_lines.line_id,
        cluster_lines.cluster_id,
        cluster_lines.geometry
    FROM cluster_lines
        LEFT JOIN split_points USING (cluster_id)
    WHERE ST_Intersects(cluster_lines.geometry, split_points.geometry)
        AND is_multilinestring
),
lines_not_touching_splitpoints AS (
    SELECT l.line_id,
        l.cluster_id,
        l.geometry AS geometry,
        r.line_id as rid,
        r.cluster_id as r_cl
    FROM cluster_lines AS l
        LEFT JOIN lines_touching_splitpoints AS r ON l.line_id = r.line_id
    WHERE l.cluster_id IN (
            SELECT cluster_id
            FROM lines_touching_splitpoints
        )
        AND r.line_id IS NULL
),
lines_not_touching_splitpoints_clusters AS (
    SELECT cluster_id,
        ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry
    FROM lines_not_touching_splitpoints
    GROUP BY cluster_id
),
-- clusters that are multilinestrings and have splitpoints
multinelinestring_clusters_splitted AS (
    SELECT l.cluster_id,
        ST_CollectionHomogenize(ST_Collect(l.geometry, r.geometry)) AS geometry
    FROM lines_touching_splitpoints l
        INNER JOIN lines_not_touching_splitpoints_clusters r USING(cluster_id)
    WHERE ST_Intersects(l.geometry, r.geometry)
),
-- clusters that are single linestrings
linestrings_clusters_splitted AS (
    SELECT l.cluster_id,
        (ST_Dump(ST_Split(ST_LineMerge(l.geometry), r.geometry))).geom AS geometry
    FROM clusters l
        INNER JOIN split_points r USING(cluster_id)
    WHERE NOT l.is_multilinestring
),
-- clusters that are multilinestrings but do not have splitpoints
multinelinestring_clusters_without_splitpoints AS (
    select cluster_id,
        ST_Multi(ST_CollectionHomogenize(geometry)) AS geometry
    FROM clusters
    WHERE is_multilinestring
        AND cluster_id NOT IN (
            SELECT DISTINCT cluster_id
            FROM multinelinestring_clusters_splitted
        )
),
unioned_without_strahler AS (
    SELECT cluster_id,
        geometry
    FROM multinelinestring_clusters_splitted
    UNION ALL
    SELECT cluster_id,
        geometry
    FROM linestrings_clusters_splitted
    UNION ALL
    SELECT cluster_id,
        geometry
    FROM multinelinestring_clusters_without_splitpoints
),
unioned_without_feature_id AS (
SELECT
	l.cluster_id,
    r.strahler,
	l.geometry
FROM unioned_without_strahler l
    INNER JOIN (
        SELECT DISTINCT ON (cluster_id)
            cluster_id,
            strahler
        FROM clusters
        ORDER BY cluster_id
    ) r USING(cluster_id)
	),
	unioned_without_strahler_unique_cluster_ids AS (
		SELECT
			DISTINCT cluster_id
		FROM unioned_without_strahler
	),
	linestrings_without_splitpoint AS (
	SELECT
		l.cluster_id,
		strahler,
		geometry
	FROM clusters l
		LEFT JOIN unioned_without_strahler_unique_cluster_ids r ON l.cluster_id = r.cluster_id
    	WHERE r.cluster_id IS NULL
	), 
	linestrings_without_splitpoint_inserted AS (
	SELECT
		*
    FROM linestrings_without_splitpoint
    UNION ALL
    SELECT
		*
    FROM unioned_without_feature_id
	),
	linestrings_without_splitpoint_inserted_with_feature_id AS (
		SELECT
			row_number() OVER (
            	ORDER BY strahler
        	) AS feature_id,
			*
		FROM linestrings_without_splitpoint_inserted
	),
	-- next steps are needed for union of overlapping lines that occur due to multipoint splitting
	lines_overlapping AS (
		SELECT
			l.feature_id,
			l.cluster_id
		FROM linestrings_without_splitpoint_inserted_with_feature_id l
		LEFT JOIN linestrings_without_splitpoint_inserted_with_feature_id r
		ON ST_Overlaps(l.geometry, r.geometry)
		WHERE l.cluster_id = r.cluster_id
	), 
	overlap_column AS (
		SELECT 
			*,
			CASE
				WHEN feature_id IN (SELECT feature_id FROM lines_overlapping) THEN true
				ELSE false
			END AS lines_overlap
		FROM linestrings_without_splitpoint_inserted_with_feature_id
	),
	overlapping_lines_merged AS (
		SELECT
			cluster_id,
			strahler,
			ST_Union(geometry) AS geometry
		FROM overlap_column
		WHERE lines_overlap = true
		GROUP BY cluster_id, strahler
	),
	lines_non_overlapping_reunion AS (
		SELECT
			*
		FROM overlapping_lines_merged
		UNION ALL
		(SELECT 
			cluster_id,
			strahler,
			geometry
		FROM overlap_column
		WHERE lines_overlap = false)
	)
	SELECT
		row_number() OVER (
            ORDER BY strahler
        ) AS feature_id,
		strahler,
		ST_Multi(ST_CollectionHomogenize(geometry)) AS geometry
	FROM lines_non_overlapping_reunion
    )
    ")
  create_table(query, table_name_destination)
  Sys.time()
}

union_per_basin_merge <- function(table_name, river_basin_names, depends_on = NULL) {
  ########## Test
  # table_name <- LINES_MERGED
  # river_basin_names <- tar_read(river_basin_names)
  ####
  
  length(depends_on)
  
  table_name_source <- 
    table_name %>% 
    str_c(river_basin_names, sep = "_")
  
  union_query <- 
    str_glue("SELECT * FROM {table_name_source} UNION ALL") %>% 
    str_c(collapse = " ") %>% 
    str_sub(end = -11)
  
  query <- 
    str_glue("CREATE TABLE {table_name} AS ({union_query})")
  
  create_table(query, table_name)
  Sys.time()
}

filter_rivers_in_studyarea <- function(table_name, 
                                       table_name_source,
                                       table_name_studyarea, 
                                       geo_index_column = NULL,
                                       depends_on = NULL) {
  ########## Test
  # table_name <- LINES_STUDYAREA
  # table_name_source <- LINES_MERGED
  # table_name_studyarea <- SELECTED_STUDYAREA_TABLE
  ####
  
  length(depends_on)
  
  query <- 
    str_glue("
    CREATE TABLE {table_name} AS ( 
      SELECT
  row_number() OVER (
            	ORDER BY strahler
        	) AS feature_id,
  left_table.strahler,
   left_table.geometry
      FROM {table_name_source} AS left_table, {table_name_studyarea} AS right_table
      WHERE ST_Intersects(left_table.geometry, right_table.geometry))")
  
  create_table(query, table_name, geo_index_column = geo_index_column)
  Sys.time()
}

join_streamorder_to_inland_waters <-
  function(table_name_destination, table_name_inland_waters, table_name_lines, depends_on = NULL) {
    length(depends_on)
    
    query <-
      str_glue("
        CREATE TABLE {table_name_destination} AS (
          SELECT
  	        DISTINCT ON (inspire_id)
  	        feature_id,
  	        strahler,
  	        l.geometry
          FROM {table_name_inland_waters} l
          INNER JOIN {table_name_lines} r
          ON ST_Intersects(l.geometry, r.geometry)
          ORDER BY 
          inspire_id, strahler DESC
        )")
    
    create_table(query, table_name_destination)
    Sys.time()
  }

get_unique_streamorders <- 
  function(table_name_source, depends_on = NULL) {
    
    length(depends_on)
    
    query <- glue::glue(
      "
      SELECT
        DISTINCT strahler
      FROM {table_name_source}
      ORDER BY strahler
      "
    )
    connection <- connect_to_database()
    unique_streamorders <- DBI::dbGetQuery(connection, query) %>% 
      as_vector() %>%
      as.integer()
    DBI::dbDisconnect(connection)
    unique_streamorders
  }

streamorder_filter <-
  function(table_name_prefix_destination,
           table_name_source,
           streamorder,
           depends_on = NULL) {
    
    length(depends_on)
    
    table <- composite_name(table_name_prefix_destination, streamorder)
    query <- glue::glue(
      "
      CREATE TABLE {table} AS (
          SELECT
            *
          FROM {table_name_source}
          WHERE strahler >= {streamorder}
      )
      "
    )
    create_table(query, table, geo_index_column = "geometry")
    Sys.time()
  }
  
is_pq_geometry <- 
  function(column) { 
    class(column) == "pq_geometry" | class(column) == "pq_NA"
    }

from_wkb <- purrr::partial(sf::st_as_sfc, EWKB = TRUE)

convert_geometry <- 
  function(x) {
    x %>% 
      as_tibble() %>% 
      mutate(across(where(is_pq_geometry), from_wkb))
  }

write_selected_studyarea <- function(x, table_name_destination, index_column = NULL) {
  write_to_table(
    x,
    table_name_destination,
    index_column = index_column
  )
  Sys.time()
}

write_inland_waters <- function(x, table_name_destination, index_column = NULL, geo_index_column = NULL) {
  
  write_to_table(
    x,
    table_name_destination,
    index_column = index_column
  )
  
  connection <- connect_to_database()
 
  if (!is.null(geo_index_column)) {
    set_geo_index(table_name_destination, geo_index_column, connection)
  }
  DBI::dbDisconnect(connection)
  
  Sys.time()
}

make_grid_centroids_in_db <- function(
  table_name_centroids_basis,
  table_name_destination,
  index_column = NULL,
  geo_index_column = NULL,
  depends_on = NULL
) {

  length(depends_on)

  query <-
    glue::glue("
        CREATE TABLE {table_name_destination} AS (
          SELECT 
	         grid_id,
	         ST_Centroid(geometry) AS geometry
         FROM {table_name_centroids_basis}
        )
        ")

  create_table(query, table_name_destination, index_column, geo_index_column)
  Sys.time()
}


nearest_neighbours_between <- function(
  table_name_destination,
  left_table,
  left_columns,
  right_table,
  right_columns = NULL,
  stream_order_id = NULL,
  n = 1,
  depends_on
) {
  length(depends_on)
  connection <- connect_to_database()

  if (length(intersect(left_columns, right_columns)) > 0) {
    stop('Ambiguous column names provided.')
  }

  format_select <- function(table, columns) {
    validate(table, columns)
    select_statements <- paste0(table, '.', columns)
    paste0(select_statements, collapse = ', ')
  }

  left_select <- format_select(left_table, left_columns) %>%
    str_replace("grid_grid_id", "grid_id") #TODO how can we make this more elegant?
  right_select <- NULL
  if (!is.null(right_columns)) {
    right_select <- format_select(right_table, right_columns)
  }
  select <- list(left_select, right_select) %>%
    compact() %>%
    str_c(collapse = ', ')

  left_geometry <- geometry_of(left_table)
  right_geometry <- geometry_of(right_table)
  by_streamorder <- ifelse(is.null(stream_order_id), FALSE, TRUE)

  distance <- glue::glue('ST_Distance({left_geometry}, {right_geometry}) AS distance_meters')
  #by_streamorder_clause <- glue::glue("AND {right_table}.stream_order_id = {stream_order_id}")
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
          ORDER BY
            {left_geometry} <-> {right_geometry}
          LIMIT
            {n}
        ) AS {right_table}
     );
  ")
  # print(query)
  create_table(query, table_destination, index_column = c("feature_id", "grid_id"))
  DBI::dbDisconnect(connection)
  Sys.time()
}

composite_name <- function(table_name, stream_order_id) {
  if(is.null(stream_order_id)){
    table_name
  } else {
    glue::glue('{table_name}_id_{stream_order_id}')
  }
}

geometry_of <- function(table) { paste0(table, '.geometry') }

grid_catchment_distance <- function(nn_grid, catchment, table, stream_order_id, depends_on = NULL) {
  length(depends_on)
  table <- composite_name(table, stream_order_id)
  query <- glue::glue("
    CREATE TABLE {table} AS (
      SELECT
        grid.grid_id,
        ST_Distance(grid.geometry, catchment.geometry) AS distance_meters
      FROM
        {composite_name(nn_grid, stream_order_id)} AS grid
        INNER JOIN
        {composite_name(catchment, stream_order_id)} AS catchment
        USING(feature_id)
    );
  ")
  create_table(query, table, "grid_id")
  Sys.time()
}

make_thiessen_catchments <- function(stream_order_id, depends_on) {
  length(depends_on)
  connection <- connect_to_database()

  table_name_destination <- composite_name(THIESSEN_CATCHMENTS_TABLE, stream_order_id)
  nearest_neighbours <- composite_name(NN_GRID_RIVERS_TABLE, stream_order_id)

  query <- glue::glue("
    CREATE TABLE {table_name_destination} AS (
    	SELECT 
	  		nn.feature_id,
	  		ST_Boundary(ST_Union(grid.geometry)) AS geometry
	  	FROM
			{GRID_POLYGONS_TABLE} AS grid
	  		INNER JOIN {nearest_neighbours} AS nn
	  		USING(grid_id)
	  	GROUP BY 1
    );
  ")
  # print(query)
  create_table(query, table_name_destination, index_column = 'feature_id')
  DBI::dbDisconnect(connection)
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

calculate_lateral_position_stream_divide_distance <- function(stream_order_id, depends_on = NULL) {
  length(depends_on)

  catchments_table <- composite_name(NN_GRID_CATCHMENTS_TABLE, stream_order_id)
  rivers_table <- composite_name(NN_GRID_RIVERS_TABLE, stream_order_id)
  table <- composite_name(MOHP_FEATURES_TABLE, stream_order_id)
  query <- glue::glue("
    CREATE TABLE {table} AS (
      SELECT
        {GRID_POLYGONS_TABLE}.grid_id,
        {GRID_POLYGONS_TABLE}.geometry,
        ROUND(rivers.distance_meters / (rivers.distance_meters + catchments.distance_meters) * 10000)::INTEGER AS lateral_position,
        ROUND(rivers.distance_meters + catchments.distance_meters)::INTEGER AS divide_stream_distance
      FROM
        {catchments_table} AS catchments
        INNER JOIN {rivers_table} AS rivers USING(grid_id)
        INNER JOIN {GRID_POLYGONS_TABLE} USING(grid_id)
    );
  ")
  create_table(query, table)
  Sys.time()
}

# TODO: delete when last one is in its own target
set_geo_indices <- function(table_names, index_columns = NULL, depends_on) {
  length(depends_on)

  if (is.null(index_columns)) {
    index_columns <- rep("geometry", length(table_names))
  }

  map2(table_names, index_columns, ~set_geo_index(.x, .y))
}

set_geo_index <- function(table, index_column = "geometry", connection = connect_to_database()) {
  db_execute(glue::glue("DROP INDEX IF EXISTS {table}_geometry_idx;"), connection = connection)
  db_execute(glue::glue("CREATE INDEX {table}_geometry_idx ON {table} USING GIST ({index_column});"), connection = connection)
}

db_execute <- function(query, connection = connect_to_database()) {
  #print(query)
  DBI::dbExecute(connection, query)
}

read_lateral_position_stream_divide_distance_from_db <- function(table_name_source_prefix, streamorder, depends_on = NULL) {
  length(depends_on)

  composite_name(
    table_name_source_prefix,
    streamorder) %>%
    get_table_from_postgress() %>%
    query_result_as_sf()
}