write_to_table <- 
  function(data, connection, table_name, append = FALSE) {
    st_write(data, dsn = connection, layer = table_name, 
             append = append)
  }

connect_to_database <- 
  function() {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      user = "postgres",
      password = "1Bg1bheYJIHnTmM6Gw",
      host = "localhost",
      dbname = "postgis"
    )
  }

initiate_database <- 
  function(river_networks, connection, table_name) {
    DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS {table_name}"))
    
    river_networks %>% 
      write_to_table(
        connection = connection,
        table_name = table_name
      )
    
    return(river_networks)
  }

run_query_create_table_brackets <- 
  function(con, query) {
    
    DBI::dbExecute(con, "DROP TABLE IF EXISTS brackets_to_drop")
    DBI::dbExecute(con, query)
  }

run_query_linemerge_by_streamorder <- 
  function(con, query_as_string) {
    DBI::dbGetQuery(con, query_as_string)
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
  function(connection, table_name) {
    DBI::dbGetQuery(connection, glue::glue("SELECT * FROM {table_name}"))
  }

hash_of_table <- 
  function(connection,
           table_name) {
    connection %>% 
      get_table_from_postgress(table_name) %>% 
      digest::digest()
  }
