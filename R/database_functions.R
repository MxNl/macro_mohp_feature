connect_to_database <- function() {
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    user = "postgres",
    host = "localhost",
    dbname = DATABASENAME
  )
}

create_table <- function(query, table, index_column = NULL, geo_index_column = NULL) {
  connection <- connect_to_database()
  db_execute(glue::glue("DROP TABLE IF EXISTS {table}"), connection = connection)
  db_execute(query, connection = connection)

  if (!is.null(index_column)) {
    index_column %>% 
      map(
        ~ set_index(table, .x, connection)
      )
  }
  if (!is.null(geo_index_column)) {
    set_geo_index(table, geo_index_column, connection)
  }
  DBI::dbDisconnect(connection)
}

set_index <- function(table, column, connection) {
  db_execute(glue::glue("DROP INDEX IF EXISTS {table}_{column}_idx;"), connection = connection)
  db_execute(glue::glue("CREATE INDEX {table}_{column}_idx ON {table} ({column});"), connection = connection)
}

write_to_table <- function(data, table, append = FALSE, index_column = NULL, geo_index_column = NULL) {
  connection <- connect_to_database()
  db_execute("CREATE EXTENSION IF NOT EXISTS postgis;", connection = connection)
  db_execute(glue::glue("DROP TABLE IF EXISTS {table};"), connection = connection)
  st_write(data, dsn = connection, layer = table, append = append)

  if (!is.null(index_column)) {
    set_index(table, index_column, connection)
  }
  if (!is.null(geo_index_column)) {
    set_geo_index(table, geo_index_column, connection)
  }
  DBI::dbDisconnect(connection)
}

prepare_lines <- function(x) {
  # x <- tar_read(line_merge_by_streamorder_raw)

  x %>%
    convert_geometry() %>% 
    st_as_sf() %>% 
    st_cast("MULTILINESTRING") %>%
    mutate(feature_id = as.integer(feature_id))
}

query_result_as_sf <- function(x) {
  x %>%
    as_tibble() %>%
    mutate(geometry = convert_pq_geomentry(geometry)) %>%
    st_as_sf()
}

convert_pq_geomentry <- function(x) {
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

get_table_from_postgress <- function(table_name_read) {
  connection <- connect_to_database()
  query_result <- DBI::dbGetQuery(connection, glue::glue("SELECT * FROM {table_name_read}"))
  DBI::dbDisconnect(connection)
  query_result
}

write_as_lines_to_db <- function(sf_lines, table_name_destination) {
  
  # sf_lines <- tar_read(river_networks_clean)
  # river_basin_name <- tar_read(river_basin_names) %>% chuck(2)
  # table_name_destination <- LINES_CLEAN
  
  sf_lines %>%
    st_cast("LINESTRING") %>%
    write_to_table(table_name_destination)
  Sys.time()
}

write_as_is_to_db <- function(sf_lines, table_name, geo_index_column = NULL) {
  
  # sf_lines <- tar_read(river_networks_grouped)
  # river_basin_name <- tar_read(river_basin_names) %>% chuck(2)
  # table_name <- LINES_MERGED
  
  streamorder <- 
    sf_lines %>% 
    slice(1) %>% 
    pull(streamorder)
  
  table_name <- 
    table_name %>% 
    composite_name(streamorder)
  
  sf_lines %>%
    write_to_table(table_name, geo_index_column = geo_index_column)
  Sys.time()
}

hash_of_db <- function(table_name) {
  get_table_from_postgress(table_name) %>%
    fastdigest::fastdigest()
}

exists_table <- function(table_name_source) {
  !DBI::dbExistsTable(connect_to_database(), table_name_source)
}

exist_tables_with_prefix <- function(table_name_source_prefix, streamorder) {
  table_names <- composite_name(table_name_source_prefix, streamorder)
  table_names %>%
    map_lgl(~!DBI::dbExistsTable(connect_to_database(), .x)) %>% 
    any()
}

drop_all_tables <- function() {
  RESERVED_OBJECTS <- c(
    'geometry_columns',
    'geography_columns',
    'spatial_ref_sys',
    'raster_columns',
    'raster_overviews'
  )
  connection <- connect_to_database()
  DBI::dbListTables(connection) %>%
    discard(. %in% RESERVED_OBJECTS) %>%
    walk(~DBI::dbExecute(connection, glue::glue("DROP TABLE {.};")))
}

postgres_config_path <- function() {
  DBI::dbGetQuery(connect_to_database(), 'SHOW config_file') %>% pull(config_file) %>% pluck(1)
}
