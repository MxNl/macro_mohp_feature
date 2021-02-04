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
    # tar_target(
    #   grid_lateral_position,
    #   future_pmap_chr(
    #     list(
    #       centroids_stream_distance,
    #       centroids_divide_distance,
    #       streamorders  
    #     ),
    #     calculate_lateral_position_grid,
    #     grid = base_grid,
    #     field_name = "lateral_position",
    #     directory = directory_lateral_position
    #   ),
    #   format = "file"
    # ),
    # tar_target(
    #   grid_stream_divide_distance,
    #   future_pmap_chr(
    #     list(
    #       centroids_stream_distance,
    #       centroids_divide_distance,
    #       streamorders  
    #     ),
    #     calculate_stream_divide_distance_grid,
    #     grid = base_grid,
    #     field_name = "distance_stream_divide",
    #     directory = directory_stream_divide_distance
    #   ),
    #   format = "file"
    # )
  )