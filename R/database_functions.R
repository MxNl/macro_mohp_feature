
connect_to_database <- 
  function() {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      user = "postgres",
      host = "localhost",
      dbname = "postgis_test"
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

create_table <- 
  function(query, table_name_destination, index_column = NULL) {
    connection <- connect_to_database()
    DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS {table_name_destination}"))
    DBI::dbExecute(connection, query)
    
    if(!is.null(index_column)){
      set_index(connection, table_name_destination, index_column)
    }
  }

write_to_table <- 
  function(data, table_name_destination, append = FALSE, index_column = NULL) {
    connection <- connect_to_database()
    DBI::dbExecute(connection, glue::glue("DROP TABLE IF EXISTS {table_name_destination}"))
    st_write(data, dsn = connection, layer = table_name_destination, 
             append = append)
    
    if(!is.null(index_column)){
      set_index(connection, table_name_destination, index_column)
    }
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
  function(table_name_read) {
    DBI::dbGetQuery(connect_to_database(), glue::glue("SELECT * FROM {table_name_read}"))
  }


write_as_lines_to_db <- 
  function(sf_lines, table_name_destination){
    sf_lines %>% 
      st_cast("LINESTRING") %>%
      write_to_table(table_name_destination)
    Sys.time()
  }

hash_of_db <- 
  function(table_name) {
    get_table_from_postgress(table_name) %>% 
      fastdigest::fastdigest()
  }

table_doesnt_exist <- 
  function(table_name_source) {
    !DBI::dbExistsTable(connect_to_database(), table_name_source)
  }