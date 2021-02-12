preprocessing_targets <-
  c(
    pipeline_for(AREA),
    
    tar_target(
      db_selected_studyarea,
      write_selected_studyarea(
        selected_studyarea,
        SELECTED_STUDYAREA_TABLE
      )
    ),

    tar_target(
      river_networks_clip,
      clip_river_networks(
        river_networks,
        #river_basins,
        selected_studyarea
      )
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
    
    # tar_target(
    #   river_network_by_streamorder,
    #     stream_order_filter(
    #       river_networks_strahler_merge,
    #       streamorders
    #       ),
    #   pattern = map(streamorders)
    # ),
    
    tar_target(
      db_river_network_by_streamorder,
      write_to_table(
        river_network_by_streamorder,
        LINES_BY_STREAMORDER
      )
    ),
    
    tar_force(
      cellsize,
      CELLSIZE,
      force = TRUE
    ),
    
    tar_target( #TODO ST_AsRaster parameter touched=true setzen?! damit grid das polygon überlappt
      db_grid_polygons,
      make_grid_polygons_in_db(
        SELECTED_STUDYAREA_TABLE,
        GRID_POLYGONS_TABLE, #"db_grid_polygons_test",
        index_column = "grid_id",
        depends_on = list(db_selected_studyarea,
                          cellsize)
        )
    ),

    tar_target(
      db_grid,
      make_grid_centroids_in_db(
        GRID_POLYGONS_TABLE,
        GRID_CENTROIDS,#"db_grid_polygons_test"
        index_column = "grid_id",
        depends_on = list(db_grid_polygons))
    ),
    
    # tar_target(
    #   base_grid,
    #   make_grid(selected_studyarea)
    # ),
    # 
    # tar_target(
    #   base_grid_centroids,
    #   make_grid_centroids(base_grid)
    # ),
    # 
    # tar_target(
    #   db_grid,
    #   write_to_table(
    #     base_grid_centroids,
    #     GRID_CENTROIDS,
    #     index_column = "grid_id"
    #   )
    # ),
    # 
    # tar_target(
    #   db_grid_polygons,
    #   write_to_table(
    #     base_grid,
    #     GRID_POLYGONS_TABLE,
    #     index_column = "grid_id"
    #   )
    # ),
    
    tar_target(
      db_geo_indices,
      set_geo_indices(
        c(GRID_CENTROIDS,
          # "db_grid_test", 
          LINES_BY_STREAMORDER),
        depends_on = list(
          db_grid,
          # db_grid_test,
          db_river_network_by_streamorder
        )
      )
    )
  )