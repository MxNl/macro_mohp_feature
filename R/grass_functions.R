
write_objects_to_grassdb <- 
  function(table_name, reference_raster, studyarea, streamorder, depends_on = NULL) {
    
    length(depends_on)
    
    link2GI::linkGRASS7(reference_raster,
                        default_GRASS7 = c(
                          "C:\\Program Files\\GRASS GIS 7.8",
                          "GRASS GIS 7.8",
                          "NSIS"
                        )
    )
    
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
    
    use_sp()
    reference_raster %>% 
      as("SpatialGridDataFrame") %>% 
      writeRAST("reference_raster")
    
    execGRASS("v.to.rast", 
              input = "river_network", 
              output = "river_network_raster", 
              use = "attr",
              attribute_column = "feature_id",
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = "river_network_raster = round(river_network_raster)",
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = "river_network_raster")
    
    execGRASS("r.thin",
              input = "river_network_raster",
              output = "river_network_value_raster_thin",
              flags = c("overwrite"))
    
    execGRASS("r.grow.distance",
              input = "river_network_value_raster_thin",
              distance = "river_network_distance_raster", 
              value = "river_network_value_raster", 
              flags = c("overwrite"))
    
    execGRASS("r.to.vect",
              input = "river_network_value_raster",
              output = "thiessen_catchments", 
              type = "area", 
              flags = c("overwrite"))
    
    use_sf()
    readVECT("thiessen_catchments") %>% 
      st_intersection(st_transform(studyarea, crs=st_crs(.))) %>%
      st_cast("MULTILINESTRING") %>% 
      writeVECT("thiessen_catchments_lines", v.in.ogr_flags = c("overwrite"))
    
    execGRASS("v.to.rast", 
              input = "thiessen_catchments_lines", 
              output = "thiessen_catchments_lines_raster", 
              use = "attr",
              attribute_column = "value",
              flags = c("overwrite"))
    
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
              expression = glue::glue("{FEATURE_NAMES[1]} = river_network_distance_raster + thiessen_catchments_distance_raster"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[2]} = river_network_distance_raster/{FEATURE_NAMES[1]}"),
              flags = c("overwrite"))
    
    FEATURE_NAMES %>%
      map(write_raster_mohp_features, streamorder)
    
    rgrass7::use_sp()
    rgrass7::readRAST("lateral_position") %>% 
      # raster() %>% 
      # raster::mask(raster) %>% 
      plot()
  }