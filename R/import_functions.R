read_studyarea <-
  function(filepath) {
    filepath %>%
      st_read() %>%
      st_transform(crs = CRS_REFERENCE)
  }

list_river_network_files <-
  function(directory) {
    directory %>%
      list.files(recursive = TRUE) %>%
      keep(str_detect(., ".shp$")) %>%
      fs::path(directory, .)
  }

list_river_basin_files <-
  function(directory) {
    directory %>%
      list.files(recursive = TRUE) %>%
      keep(str_detect(., ".gpkg$")) %>%
      keep(str_detect(., "public")) %>%
      str_c(directory, .)
  }

read_river_networks <-
  function(file) {
    file %>%
      map(read_sf) %>%
      reduce(bind_rows) %>%
      st_zm() %>%
      #rename(geometry = Shape) %>%
      janitor::clean_names() %>%
      select(dfdd, inspire_id, strahler) %>%
      st_transform(up_to_here, crs = CRS_REFERENCE)
  }

read_river_basins <-
  function(file) {
    ##### Test
    # filepath <- "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data/"
    ####
    layer <-
      tar_read(river_basins_files)[1] %>%
        str_replace(".*(?=drainage)", "") %>%
        str_replace("(?=_public).*", "") %>%
        str_replace("drainage_network_", "") %>%
        str_c("_eudem2_basins")

    tar_read(river_basins_files)[1] %>%
      read_sf(layer) %>%
      rename(geometry = Shape) %>%
      janitor::clean_names() %>%
      select(namebasin) %>%
      summarize(namebasin = first(namebasin)) %>%
      st_transform(crs = CRS_REFERENCE)
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
