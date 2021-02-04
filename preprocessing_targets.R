preprocessing_targets <-
  list(
    
    tar_target(
      studyarea_outline_level_germany,
      determine_studyarea_outline_level_germany(studyarea_germany, coastline)
    ),
    
    # tar_target(
    #   studyarea_outline_level_europe,
    #   determine_studyarea_outline_level_europe(
    #     river_basins, 
    #     coastline)
    # ),
    
    tar_target(
      studyarea_outline_level_pipelinetest,
      studyarea_pipelinetest
    ),
    
    tar_target(
      selected_studyarea,
      studyarea_outline_level_pipelinetest
      ),
    
    # tar_target(
    #   river_networks_clip,
    #   clip_river_networks(
    #     river_networks, 
    #     river_basins, 
    #     selected_studyarea)
    # ),
    
    tar_target(
      river_networks_only_rivers,
      reclassify_relevant_canals_and_ditches_and_drop_others(
        river_network_pipeline_test, # TODO use river_networks_clip again,
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
      write_as_lines_to_db(
        river_networks_clean,
        LINES_CLEAN
      )
    ),
    
    tar_target(
      db_connected_but_merged_river_networks,
      write_connected_but_merged_river_networks(
        LINES_CLEAN,
        LINES_CONNECTED_ID,
        depends_on = list(
          db_river_networks_clean
        )
      )
    ),
    
    tar_target(
      river_networks_only_connected,
      drop_disconnected_river_networks(
        river_networks_clean,
        selected_studyarea,
        LINES_CONNECTED_ID,
        depends_on = list(
          db_river_networks_clean,
          db_connected_but_merged_river_networks
        )
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
      write_as_lines_to_db(
        river_networks_dissolved_junctions_after,
        LINES_RAW)
    ),
    
    tar_target(
      river_networks_strahler_merge,
      merge_same_strahler_segments(
        depends_on = list(
          db_river_networks_dissolved_junctions_after
        )
      )
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
      write_to_table(
        river_network_by_streamorder,
        LINES_BY_STREAMORDER
      )
    ),
    
    tar_target(
      base_grid,
      make_grid(selected_studyarea)
    ),
    
    tar_target(
      base_grid_centroids,
      make_grid_centroids(base_grid)
    ),
    
    tar_target(
      db_grid,
      write_to_table(
        base_grid_centroids,
        GRID_CENTROIDS
      )
    ),
    
    tar_target(
      db_grid_polygons,
      write_to_table(
        base_grid,
        GRID_POLYGONS)
    ),
    
    tar_target(
      db_geo_indices,
      set_geo_indices(
        c(GRID_CENTROIDS, LINES_BY_STREAMORDER),
        depends_on = list(
          db_grid,
          db_river_network_by_streamorder
        )
      )
    )
  )