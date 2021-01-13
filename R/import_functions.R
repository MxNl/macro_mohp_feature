read_studyarea <-
  function(filepath) {
    filepath %>%
      st_read() %>%
      st_transform(crs = CRS_REFERENCE)
  }

read_river_networks <-
  function(filepath) {
    ##### Test
    # filepath <- tar_read(filepath_river_networks)
    ####

    files <-
      filepath %>%
      list.files(recursive = TRUE) %>%
      keep(str_detect(., ".gpkg$")) %>%
      keep(!str_detect(., "public|Documentation"))


    river_networks <-
      filepath %>%
      str_c(files) %>%
      map(
        ~ map(
          STREAM_TYPE_TO_INCLUDE,
          ~ read_sf(.y, layer = .x),
          .y = .x
        )
      ) %>%
      reduce(bind_rows) %>% 
      st_zm() %>% 
      rename(geometry = Shape) %>% 
      janitor::clean_names() %>%
      st_transform(crs = CRS_REFERENCE)

    return(river_networks)
  }

read_coastline <-
  function(filepath) {

    coastline <- 
      filepath %>% 
      read_sf() %>% 
      summarise() %>% 
      st_transform(CRS_REFERENCE)

    return(coastline)
  }


get_feature_ids_to_reclassify <- 
  function(filepath) {
    filepath %>% 
      list.files() %>% 
      magrittr::extract(str_detect(., ".shp$")) %>% 
      str_c(filepath, "/", .) %>% 
      map_df(st_read) %>% 
      # imap_dfr(add_source_id) %>% 
      # reduce(bind_rows) %>% 
      as_tibble() %>% 
      janitor::clean_names() %>% 
      verify(nrow(.) == length(unique(.$inspire_id))) %>% 
      pull(inspire_id) %>% 
      as.character()
  }
