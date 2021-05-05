export_targets <- 
  list(
    tar_target(
      feature_names,
      FEATURE_NAMES
    ),
    
    tar_target(
      write_results,
      write_raster_mohp_features(
        lateral_position_stream_divide_distance,
        streamorders,
        feature_names,
        reference_raster
      ),
      pattern = cross(streamorders, feature_names),
      deployment = "main"
    )
  )