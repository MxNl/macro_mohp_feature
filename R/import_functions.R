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
    # files <-
    #   filepath %>%
    #   list.files(recursive = TRUE) %>%
    #   keep(str_detect(., ".gdb$")) %>%
    #   keep(!str_detect(., "public|Documentation")) %>%
    #   str_replace("/gdb", "")

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