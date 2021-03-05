write_raster_mohp_features <-
  function(lateral_position_stream_divide_distance, streamorder, feature_name, reference_raster) {

    if (feature_name == "lateral_position") {
      filepath_prefix_feature_name <- "lp"
      directory <- "output_data/lateral_position/"
    } else if (feature_name == "divide_stream_distance") {
      filepath_prefix_feature_name <- "dsd"
      directory <- "output_data/divide_stream_distance/"
    } else {
      stop("Provide valid value for the argument feature_name")
    }

    filepath_prefix_streamorder <- streamorder
    filepath_prefix_spatial_resolution <- CELLSIZE

    filepath <-
      str_c(
        FILEPATH_PREFIX_SPATIAL_COVERAGE,
        "_",
        filepath_prefix_feature_name,
        "_",
        "streamorder",
        filepath_prefix_streamorder,
        "_",
        filepath_prefix_spatial_resolution,
        "m",
        ".tiff"
      ) %>%
      fs::path(directory, .)
    
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)  
    }
    
    lateral_position_stream_divide_distance %>%
      pluck(streamorder) %>%
      select(all_of(feature_name)) %>% 
      fasterize::fasterize(reference_raster, field = feature_name) %>% 
      writeRaster(filepath, overwrite = TRUE)
      # gdalUtils::gdal_rasterize("output_data/test.tiff", output_Raster = TRUE, verbose = TRUE)
  }