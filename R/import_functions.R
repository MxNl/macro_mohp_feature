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
      select(dfdd, inspire_id, strahler, hyp) %>% 
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

generate_lines <-
  function() {
    coordinates <- tribble(
      ~x, ~y, ~feature_id, ~strahler,
      # 6,0,1,3,
      7, 1, 1, 3,
      7, 3, 1, 3,
      7, 3, 17, 3,#bracket
      9, 4, 17, 3,#bracket
      7, 5, 17, 3,#bracket
      7, 3, 2, 3,
      7, 5, 2, 3,
      7, 5, 3, 2,
      5, 5, 3, 2,
      5, 5, 4, 2,
      3, 5, 4, 2,
      3, 5, 5, 1,
      1, 5, 5, 1,
      3, 5, 6, 1,
      3, 7, 6, 1,
      7, 5, 7, 3,
      7, 7, 7, 3,
      7, 7, 8, 2,
      9, 7, 8, 2,
      9, 7, 9, 2,
      11, 7, 9, 2,
      11, 7, 10, 1,
      13, 7, 10, 1,
      11, 7, 11, 1,
      11, 9, 11, 1,
      7, 7, 12, 2,
      7, 9, 12, 2,
      7, 9, 131, 1,
      6, 9, 131, 1,
      6, 9, 132, 1,
      5, 9, 132, 1,
      5, 9, 133, 1,
      3, 9, 133, 1,
      7, 9, 14, 2,
      7, 11, 14, 2,
      7, 11, 15, 1,
      9, 11, 15, 1,
      7, 11, 16, 1,
      7, 13, 16, 1,
    )
    
    # coordinates <- 
    # coordinates %>% 
    #   mutate(feature_id = rep(1:(nrow(coordinates)/2)))
    
    coordinates <-
      coordinates %>%
      mutate(type = "river") %>%
      mutate(type = replace(type, feature_id == 14, "canal"))
    
    sf_lines <- 
      coordinates %>% 
      mutate(point = str_c(x, " ", y)) %>% 
      group_by(feature_id, strahler) %>% 
      summarise(point_collection = str_c(point, collapse = ", "), .groups = "drop") %>% 
      mutate(geometry = str_c("LINESTRING (", point_collection, ")")) %>% 
      select(-point_collection) %>% 
      st_as_sf(wkt = "geometry") %>% 
      mutate(across(all_of(c("feature_id", "strahler")), as.integer))
    
    st_crs(sf_lines) <- CRS_REFERENCE
    
    return(sf_lines)
  }

