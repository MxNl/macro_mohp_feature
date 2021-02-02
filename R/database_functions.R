write_to_table <- 
  function(data, table_name_create, append = FALSE) {
    st_write(data, dsn = connect_to_database(), layer = table_name_create, 
             append = append)
  }

connect_to_database <- 
  function() {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      user = "postgres",
      # password = "1Bg1bheYJIHnTmM6Gw",
      host = "localhost",
      dbname = "postgis"
    )
  }

initiate_database <- 
  function(river_networks, table_name) {
    #### Test
    # river_networks <- tar_read(river_networks_clip)
    # table_name <- "testi"
    # connection <- connect_to_database()
    ####
    connection <- connect_to_database()
    
    DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS {table_name}"))
    
    river_networks %>% 
      write_to_table(
        connection = connection,
        table_name = table_name
      )
    
    return(river_networks)
  }

run_query_create_table_brackets <- 
  function(query, table_name_create) {
    connection <- connect_to_database()
    DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS {table_name_create}"))
    DBI::dbExecute(connection, query)
  }

run_query_linemerge_by_streamorder <- 
  function(con) {
    DBI::dbGetQuery(con, glue::glue("
      WITH collected AS (
      	SELECT strahler, ST_Collect(geometry) AS geometry
      	FROM {LINES_RAW} GROUP BY strahler
      ), local_linestrings AS (
      	SELECT strahler, (ST_Dump(ST_LineMerge(geometry))).geom AS geometry FROM collected
      ), local_linestrings_with_id AS (
      	SELECT row_number() OVER (ORDER BY strahler) AS id, * FROM local_linestrings
      ), local_linestrings_with_splitpoints AS (
      	SELECT
      		l.id AS id,
      		l.strahler AS strahler,
      		ST_AsText(l.geometry) AS geometry,
      		r.id AS r_id,
      		r.strahler AS r_strahler,
      		ST_AsText(r.geometry) AS r_geometry,
      		ST_AsText(ST_Intersection(l.geometry, r.geometry)) AS common_geometry
      	FROM local_linestrings_with_id AS l
      	CROSS JOIN local_linestrings_with_id AS r
      	WHERE 
      		(
      			l.strahler < r.strahler
      			AND
      			ST_Touches(l.geometry, r.geometry)
      			AND
      			l.id != r.id
      		)
      ), local_linestrings_splitted AS (
      	SELECT 
      		id AS old_id, 
      		strahler,
      		(ST_Dump(
      			CASE WHEN ST_Equals(geometry, common_geometry) THEN 
      				ST_ForceCollection(geometry)
      			ELSE
      				ST_Split(geometry, common_geometry)
      			END
      		)).geom
      	AS geometry
      	FROM local_linestrings_with_splitpoints
      ), local_linestrings_without_splitpoints AS (
      	SELECT * FROM local_linestrings_with_id
      	WHERE
      		NOT EXISTS (
      		SELECT 
      			1
      		FROM
      			local_linestrings_with_splitpoints
      		WHERE 
      			local_linestrings_with_splitpoints.id = local_linestrings_with_id.id)
      )
      SELECT * FROM local_linestrings_splitted
      UNION
      SELECT * FROM local_linestrings_without_splitpoints
    "))
  }

prepare_lines <- 
  function(x) {
    # x <- tar_read(line_merge_by_streamorder_raw)
    
    x %>% 
      query_result_as_sf() %>% 
      select(-old_id) %>% 
      add_feature_index_column()
    
  }

query_result_as_sf <- 
  function(x) {
    x %>% 
      as_tibble() %>% 
      mutate(geometry = convert_pq_geomentry(geometry)) %>%
      st_as_sf()
  }

convert_pq_geomentry <- 
  function(x) {
    x %>%
      st_as_sfc() %>%
      st_sf() %>% 
      as_tibble() %>% 
      pull(geometry)
  }

# add_feature_id <- 
#   function(x) {
#     x %>% 
#       mutate(feature_id = 1:n(),
#              .before = 1) %>% 
#       mutate(feature_id = as.character(feature_id))
#   }

get_table_from_postgress <-
  function(table_name) {
    DBI::dbGetQuery(connect_to_database(), glue::glue("SELECT * FROM {table_name}"))
  }

hash_of_table <- 
  function(table_name) {
    DB %>% 
      get_table_from_postgress(table_name) %>% 
      digest::digest()
  }


write_to_db_and_return_hash <- 
  function(sf_lines, table_name){
    sf_lines %>% 
      st_cast("LINESTRING") %>%
      write_to_table(table_name = table_name)
    
    hash_of_db(table_name)
  }

write_as_lines_to_db <- 
  function(sf_lines, table_name){
    sf_lines %>% 
      st_cast("LINESTRING") %>%
      write_to_table(table_name = table_name)
  }

hash_of_db <- 
  function(table_name) {
    get_table_from_postgress(table_name) %>% 
      fastdigest::fastdigest()
  }

is_db_hash_outdated <- 
  function(last_hash, table_name_for_current_hash) {
    current_hash <- 
      table_name_for_current_hash %>% 
      hash_of_db()
    
    current_hash != last_hash
  }

table_doesnt_exist <- 
  function(table_name_source) {
    !DBI::dbExistsTable(connect_to_database(), table_name_source)
  }