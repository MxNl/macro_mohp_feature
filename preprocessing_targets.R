preprocessing_targets <- 
  list(
    tar_target(
      studyarea_outline,
      determine_studyarea_outline(studyarea_subset_plots, coastline)
    ),
    tar_target(
      river_networks_clip,
      clip_river_networks(river_networks, studyarea_outline)
    ),
    tar_target(
      river_networks_only_rivers,
      reclassify_relevant_canals_and_ditches_and_drop_others(
        river_networks_clip,
        features_ids_to_reclassify
      )
    ),
    tar_target(
      river_networks_valid_strahler,
      impute_line_features_with_invalid_strahler_value(river_networks_only_rivers)
    ),
    tar_target(
      river_networks_clean,
      clean_river_networks(river_networks_valid_strahler)
    ),
    tar_target(
      db_river_networks_clean,
      write_to_db(river_networks_clean, LINES_CLEAN)
    ),
    tar_target(
      river_networks_only_connected,
      drop_disconnected_river_networks(
        river_networks_clean, 
        studyarea_outline,
        LINES_CLEAN
      )
    ),
    tar_target(
      river_networks_dissolved_junctions,
      dissolve_line_features_between_junctions(river_networks_only_connected)
    ),
    tar_target(
      river_networks_without_brackets,
      drop_shorter_bracket_line_features(river_networks_dissolved_junctions)
    ),
    tar_target(
      river_networks_dissolved_junctions_after,
      dissolve_line_features_between_junctions(river_networks_without_brackets)
    ),
    tar_target(
      db_river_networks_dissolved_junctions_after,
      write_to_db(river_networks_dissolved_junctions_after, LINES_RAW)
    ),
    tar_target(
      river_networks_strahler_merge,
      merge_same_strahler_segments(river_networks_dissolved_junctions_after)
    ),
    tar_target(
      streamorders,
      river_networks_strahler_merge %>% 
        as_tibble() %>% 
        distinct(strahler) %>% 
        pull(strahler) %>% 
        as.numeric() %>%
        sort()
    ),
    tar_target(
      river_network_by_streamorder,
      streamorders %>% 
        as.vector() %>% 
        as.numeric() %>% 
        future_map(
          ~stream_order_filter(
            river_network = river_networks_strahler_merge,
            stream_order = .x
          )
        ) %>% 
        bind_rows()
    ),
    tar_target(
      db_river_network_by_streamorder,
      write_to_table(river_network_by_streamorder, "river_network_by_streamorder")
    ),
    tar_target(
      base_grid,
      make_grid(river_network_by_streamorder)
    ),
    tar_target(
      base_grid_centroids,
      make_grid_centroids(base_grid)
    ),
    # tar_target(
    #   thiessen_catchments_centroids,
    #   river_network_by_streamorder %>% 
    #     future_map(
    #       ~make_thiessen_catchments_centroids(
    #         .x,
    #         base_grid,
    #         base_grid_centroids
    #       )
    #     )
    # ),
    tar_target(
      db_grid,
      write_to_table(base_grid_centroids, "grid")
    ),
    tar_target(
      db_grid_polygons,
      write_to_table(base_grid, "grid_polygons")
    ),
    tar_target(
      db_geo_indices,
      set_geo_indices(c("grid", "river_network_by_streamorder"))
    ),
    tar_target(
      nearest_neighbours,
      nearest_neighbours_between(
        left_table =  "grid",
        right_table = "river_network_by_streamorder",
        left_columns = c("id", "geometry"),
        right_columns = c("feature_id", "strahler", "stream_order_id", "geometry"),
        stream_order_id = 6,
        left_target = db_river_network_by_streamorder,
        right_target = db_grid
      )
    )#,
    
    # tar_target(
    #   thiessen_catchments,
    #   thiessen_catchments_centroids %>% 
    #     future_map(
    #       ~make_thiessen_catchments(
    #         base_grid,
    #         .x
    #       )
    #     )
    # ),
    # tar_target(
    #   centroids_stream_distance,
    #   future_map2(
    #     thiessen_catchments_centroids,
    #     river_network_by_streamorder,
    #     calculate_stream_distance_centroids
    #   )
    # ),
    # tar_target(
    #   centroids_divide_distance,
    #   future_map2(
    #     thiessen_catchments_centroids,
    #     thiessen_catchments,
    #     calculate_divide_distance_centroids
    #   )
    # ),
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