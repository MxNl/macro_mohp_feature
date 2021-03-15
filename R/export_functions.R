write_raster_mohp_features <-
  function(feature_name, streamorder) {

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
    
    execGRASS("r.out.gdal",
              input = feature_name,
              output = filepath)
  }