connect_to_database <- function() {
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    user = "postgres",
    host = "localhost",
    dbname = "postgis_test"
  )
}

create_table <- function(query, table, index_column = NULL) {
  connection <- connect_to_database()
  db_execute(glue::glue("DROP TABLE IF EXISTS {table}"), connection = connection)
  db_execute(query, connection = connection)

  if (!is.null(index_column)) {
    set_index(table, index_column, connection)
  }
}

write_to_table <- function(data, table, append = FALSE, index_column = NULL) {
  connection <- connect_to_database()
  db_execute(glue::glue("DROP TABLE IF EXISTS {table};"), connection = connection)
  st_write(data, dsn = connection, layer = table, append = append)

  if (!is.null(index_column)) {
    set_index(table, index_column, connection)
  }
  # TODO: add geoindex here if provided
}

prepare_lines <- function(x) {
  # x <- tar_read(line_merge_by_streamorder_raw)

  x %>%
    query_result_as_sf() %>%
    select(-old_id) %>%
    add_feature_index_column()

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
  DBI::dbGetQuery(connect_to_database(), glue::glue("SELECT * FROM {table_name_read}"))
}


write_as_lines_to_db <- function(sf_lines, table_name_destination) {
  sf_lines %>%
    st_cast("LINESTRING") %>%
    write_to_table(table_name_destination)
  Sys.time()
}

hash_of_db <- function(table_name) {
  get_table_from_postgress(table_name) %>%
    fastdigest::fastdigest()
}

exists_table <- function(table_name_source) {
  !DBI::dbExistsTable(connect_to_database(), table_name_source)
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
