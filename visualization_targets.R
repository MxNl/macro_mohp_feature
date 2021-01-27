visualization_targets <- 
  list(
    
    tar_target(
      plot_subset_all_steps,
      list(
        river_networks_clip,
        river_networks_only_rivers,
        river_networks_valid_strahler,
        river_networks_clean,
        river_networks_only_connected,
        river_networks_dissolved_junctions,
        river_networks_without_brackets,
        river_networks_dissolved_junctions_after,
        river_networks_strahler_merge
      ) %>% 
        map(plot_lines_coloured_by_categorical_attribute, feature_id) %>% 
        cowplot::plot_grid(plotlist = .)
    ),
    
    
    tar_target(
      test_processed_river_network_plot,
      plot_test_processed_river_network(river_networks_strahler_merge,
                                        studyarea_subset_plots)
    ),
    
    tar_target(
      test_catchments_plot,
      plot_test_catchments(river_network_by_streamorder, thiessen_catchments, streamorders, studyarea_subset_plots)
    )
  )