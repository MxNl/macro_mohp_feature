export_targets <- 
  list(
  
    # tar_target(
    #   directory_output_data,
    #   create_directory_and_return_path("output_data/"),
    # ),
    # tar_target(
    #   directory_lateral_position,
    #   create_directory_and_return_path(directory_output_data, "lateral_position/")
    # ),
    # tar_target(
    #   directory_stream_divide_distance,
    #   create_directory_and_return_path(directory_output_data, "stream_divide_distance/")
    # ),
    tar_target(
      feature_names,
      FEATURE_NAMES
    ),
    
    tar_target(
      write_results,
      write_raster_mohp_features(
        lateral_position_stream_divide_distance,
        streamorders,
        feature_names
      ),
      pattern = cross(streamorders, feature_names)
    )
  )