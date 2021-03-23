initiate_grass_db <- 
  function(reference_raster, studyarea) {
    initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.8",
              addon_base = "C:/Noelscher.M/Anwendungsdaten/GRASS7/addons",
              # home parameter necessary under UNIX-based systems
              home = tempdir(),
              gisDbase = tempdir(), 
              location = "test", 
              mapset = "PERMANENT", 
              override = TRUE)
    
    execGRASS("g.proj", flags = c("c", "quiet"), 
              proj4 = st_crs(studyarea)$proj4string)
    b_box = st_bbox(studyarea) 
    execGRASS("g.region", flags = c("quiet"), 
              n = as.character(b_box["ymax"]), s = as.character(b_box["ymin"]), 
              e = as.character(b_box["xmax"]), w = as.character(b_box["xmin"]), 
              res = as.character(CELLSIZE))
  }

write_objects_to_grassdb <- 
  function(table_name, reference_raster, studyarea, streamorder, depends_on = NULL) {
    
    length(depends_on)
    
    # fs::dir_delete(GRASS_DIRECTORY)
    # fs::dir_create(GRASS_DIRECTORY)
    # 
    # grass_streamorder_directory <-
    #   GRASS_STREAMORDER_DIRECTORY %>%
    #   composite_name(streamorder) %>%
    #   str_c(GRASS_DIRECTORY, ., sep = "/")
    # 
    # if (!fs::dir_exists(GRASS_DIRECTORY)) {
    #   fs::dir_create(GRASS_DIRECTORY)
    # }
    
    initiate_grass_db(reference_raster, studyarea)
    
    use_sf()
    table_name %>% 
      composite_name(streamorder) %>% 
      get_table_from_postgress() %>% 
      query_result_as_sf() %>% 
      add_feature_index_column() %>% 
      mutate(feature_id = as.integer(feature_id)) %>% 
      st_transform(crs(reference_raster)) %>% 
      writeVECT("river_network", 
                v.in.ogr_flags = c("overwrite"))
    message("river_network")
    use_sp()
    reference_raster %>% 
      as("SpatialGridDataFrame") %>% 
      writeRAST("reference_raster",
                overwrite = TRUE)
    message("reference_raster")
    execGRASS("r.mask",
              raster = "reference_raster")
    message("r.mask")
    execGRASS("v.to.rast", 
              input = "river_network", 
              output = "river_network_raster", 
              use = "attr",
              attribute_column = "feature_id",
              flags = c("overwrite"))
    message("v.to.rast")
    execGRASS("r.mapcalc",
              expression = "river_network_raster = round(river_network_raster)",
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = "river_network_raster")
    
    execGRASS("r.thin",
              input = "river_network_raster",
              output = "river_network_value_raster_thin",
              flags = c("overwrite"))
    message("r.thin")
    execGRASS("r.grow.distance",
              input = "river_network_value_raster_thin",
              distance = "river_network_distance_raster", 
              value = "river_network_value_raster", 
              flags = c("overwrite"))
    message("r.grow.distance")
    execGRASS("r.to.vect",
              input = "river_network_value_raster",
              output = "thiessen_catchments", 
              type = "area", 
              flags = c("overwrite"))
    
    use_sf()
    readVECT("thiessen_catchments") %>% 
      sf::st_buffer(dist = 0) %>% 
      st_intersection(st_transform(studyarea, crs=st_crs(.))) %>%
      st_cast("MULTILINESTRING") %>% 
      writeVECT("thiessen_catchments_lines", v.in.ogr_flags = c("overwrite"))
    
    execGRASS("v.to.rast", 
              input = "thiessen_catchments_lines", 
              output = "thiessen_catchments_lines_raster", 
              use = "attr",
              attribute_column = "value",
              flags = c("overwrite"),
              memory = GRASS_MAX_MEMORY)
    
    execGRASS("r.mapcalc",
              expression = "thiessen_catchments_lines_raster = round(thiessen_catchments_lines_raster)",
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = "thiessen_catchments_lines_raster")
    
    execGRASS("r.thin",
              input = "thiessen_catchments_lines_raster",
              output = "thiessen_catchments_lines_raster_thin",
              flags = c("overwrite"))
    
    execGRASS("r.grow.distance",
              input = "thiessen_catchments_lines_raster_thin",
              distance = "thiessen_catchments_distance_raster",
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[1]} = round(river_network_distance_raster + thiessen_catchments_distance_raster)"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[2]} = round((river_network_distance_raster/(river_network_distance_raster + thiessen_catchments_distance_raster))*10000)"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[3]} = round(river_network_distance_raster)"),
              flags = c("overwrite"))
    
    FEATURE_NAMES %>%
      map(write_raster_mohp_features, streamorder)
  }