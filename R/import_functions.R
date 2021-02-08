read_studyarea <-
  function(filepath) {
    filepath %>%
      st_read() %>%
      transform_crs_if_required()
  }

list_river_network_files <-
  function(directory) {
    directory %>%
      list.files(recursive = TRUE) %>%
      keep(str_detect(., ".gpkg$")) %>%
      discard(str_detect(., "public")) %>%
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
    river_basin_name <-
      file %>% 
      str_replace(".*(?=GPKG/euhydro_)", "") %>% 
      str_replace("(?=_v0).*", "") %>% 
      str_replace("GPKG/euhydro_", "")
    
    file %>% 
      map(
        STREAM_TYPE_TO_INCLUDE,
        read_sf,
        dsn = .
      ) %>%
      reduce(bind_rows) %>%
      st_zm() %>%
      rename(geometry = Shape) %>% 
      clean_names() %>%
      select(dfdd, inspire_id, strahler) %>% 
      mutate(river_basin_name = river_basin_name) %>% 
      transform_crs_if_required()
  }

transform_crs_if_required <- 
  function(x) {
    if(st_crs(x) != CRS_REFERENCE) {
      x %>% 
        st_transform(crs = CRS_REFERENCE)
    } else {
      x
    }
    
  }

read_river_basins <-
  function(file) {
    river_basin_name <-
      file %>% 
      str_replace(".*(?=GPKG/euhydro_)", "") %>% 
      str_replace("(?=_v0).*", "") %>% 
      str_replace("GPKG/euhydro_", "")
    
    file %>% 
      read_sf("RiverBasins") %>%
      st_zm() %>%
      rename(geometry = Shape) %>% 
      select(geometry) %>%
      mutate(river_basin_name = river_basin_name) %>%
      transform_crs_if_required()
  }

read_coastline <-
  function(filepath) {
    filepath %>%
      read_sf() %>%
      st_zm() %>% 
      summarise() %>%
      transform_crs_if_required()
  }


get_feature_ids_to_reclassify <-
  function(filepath) {
    filepath %>%
      list.files() %>%
      magrittr::extract(str_detect(., ".shp$")) %>%
      str_c(filepath, "/", .) %>%
      map_dfr(st_read) %>%
      as_tibble() %>%
      clean_names() %>%
      distinct(inspire_id, .keep_all = TRUE) %>% 
      verify(nrow(.) == length(unique(.$inspire_id))) %>%
      pull(inspire_id) %>%
      as.character()
  }
