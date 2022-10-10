read_studyarea <-
  function(filepath) {
    filepath %>%
      st_read() %>%
      mutate(region_name = str_c("testarea", row_number()), .before = geometry) %>% 
      transform_crs_if_required() %>% 
      mutate(id = 1L)
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
      keep(str_detect(., "drainage")) %>%
      fs::path(directory, .)
  }

read_river_networks_parallel <- 
  function(file) {
    plan(multisession)
    
    data <- file %>% 
      future_map_dfr(read_river_networks)
    
    plan(sequential)
    data
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
      select(dfdd, inspire_id, strahler, hyp, longpath, nextdownid, object_id) %>% 
      mutate(river_basin_name = river_basin_name) %>% 
      transform_crs_if_required()
  }

read_inland_waters <- 
  function(file) {
    river_basin_name <-
      file %>% 
      str_replace(".*(?=GPKG/euhydro_)", "") %>% 
      str_replace("(?=_v0).*", "") %>% 
      str_replace("GPKG/euhydro_", "")
    
    file %>% 
      read_sf(layer = "InlandWater") %>%
      st_zm() %>%
      rename(geometry = Shape) %>% 
      clean_names() %>%
      select(inspire_id, area) %>% 
      filter(area >= CELLSIZE^2*4) %>% 
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
  function(river_basins_files, river_basin_name) {
    
    if(river_basin_name == "fr_islands") {
      gpkg_layer <- glue::glue("{river_basin_name}_eudem2_basins")
    } else {
      gpkg_layer <- glue::glue("{river_basin_name}_eudem2_basins_h1")
    }
    
    basin <- 
      river_basins_files %>% 
      magrittr::extract(str_detect(., river_basin_name)) %>% 
      read_sf(gpkg_layer) %>%
      st_zm() %>%
      rename(geometry = Shape)
    
    if(river_basin_name == "danube") {
      basin <- 
        basin %>% 
        filter(H1_ID != "EU4H100296")
    }
    
    basin %>% 
      select(geometry) %>%
      transform_crs_if_required() %>% 
      mutate(river_basin_name = river_basin_name)
  }

read_coastline <-
  function(filepath) {
    filepath %>%
      read_sf() %>%
      st_zm() %>% 
      clean_names() %>% 
      # select(geometry) %>% 
      # st_make_valid() %>%
      # as_Spatial() %>%
      # gUnaryUnion() %>%
      # st_as_sf() %>%
      transform_crs_if_required()
  }

return_path_to_data_descriptor_tex_file <- 
  function(depends_on = NULL) {
    length(depends_on)
    here::here("data_descriptor", "tex", "data_descriptor.tex")
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

read_mohp_starsproxy <- function(directory) {
  spatial_extent <- filename_placeholders_values[names(filename_placeholders_values) == "region_name_spatcov"]
  
  eumohp_starsproxy <- eumohp_clip(directory,
                                    region_name_spatcov = spatial_extent,
                                    # hydrologic_order = c(5),
                                    # abbreviation_measure = "dsd",
                                    eumohp_version = "v013.1.1"
  )
  
  eumohp_starsproxy %>% 
    purrr::set_names(str_remove(names(eumohp_starsproxy), "europemainland-finland-norway-sweden-france-greece-iceland-italy1-italy2-turkey-unitedkingdom-unitedkingdom-ireland_")) %>%
    map(stars::st_downsample, n =500)#100
}
