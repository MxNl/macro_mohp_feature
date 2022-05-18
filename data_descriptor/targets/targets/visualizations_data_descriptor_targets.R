visualizations_data_descriptor_targets <- 
  list(
    tar_target(
      input_data_table,
      make_input_data_table()
    ),
    tar_target(
      filepath_directory_tree,
      "data_descriptor/tex/directory_tree.pdf"
    ),
    tar_render(
      directory_tree, 
      "directory_tree.Rmd",
      output_file = filepath_directory_tree,
      cue = tar_cue("always")
    ),
    tar_target(
      directory_tree_trimmed,
      trim_background_and_return_time(filepath_directory_tree, list(directory_tree))
    ),
    tar_target(
      selected_hydrologic_orders,
      c(3, 4)
    ),
    tar_target(
      spatial_coverage,
      if(AREA == "europe") "italy2" else "."
    ),
    # tar_target(
    #   dataset_map_overview_plot,
    #   make_dataset_map_overview_plot(
    #     selected_hydrologic_orders = selected_hydrologic_orders, 
    #     spatial_coverage = spatial_coverage
    #   )
    # ),
    tar_target(
      mohp_starsproxy,
      read_mohp_starsproxy(OUTPUT_DIRECTORY),
      iteration = "list",
      cue = tar_cue(mode = "always")
    ),
    tar_target(
      mohp_starsproxy_names,
      names(mohp_starsproxy),
      iteration = "list"
    ),
    tar_target(
      mohp_raster_values,
      stars_to_values(mohp_starsproxy, mohp_starsproxy_names),
      pattern = map(mohp_starsproxy, mohp_starsproxy_names),
      deployment = "main"
    ),
    tar_target(
      quantile_breaks,
      map_quantiles_breaks(
        mohp_raster_values, 
        HYDROLOGIC_ORDERS_TO_PLOT
        ),
    ),
    tar_target(
      quantile_breaks_hydrologicorder9,
      map_quantiles_breaks(
        mohp_raster_values, 
        9
        ),
    ),
    tar_target(
      dataset_map_overview_plot,
      eumohp_plot(
        mohp_starsproxy, 
        quantile_breaks, 
        HYDROLOGIC_ORDERS_TO_PLOT
        ),
    ),
    tar_target(
      dataset_map_hydrologicorder9_plot,
      eumohp_plot(
        mohp_starsproxy, 
        quantile_breaks_hydrologicorder9, 
        9,
        FALSE
        ),
    ),
    tar_target(
      stats_ridge_plot,
      make_stats_ridges_plot(mohp_raster_values),
    ),
    tar_target(
      stats_table_data,
      make_stats_summary(mohp_raster_values),
    ),
    tar_target(
      output_data_table,
      make_output_data_table(streamorders)
    ),
    tar_target(
      targets_runtime_table,
      make_targets_runtime_table(),
      cue = tar_cue("always")
    ),
    tar_target(
      studyarea_figure,
      make_studyarea_figure(selected_studyarea)
    ),
    tar_target(
      river_canal_confusion_plot,
      make_river_canal_confusion_example_plot(871980.836, 5802002.409, 7E3, river_networks)
    ),
    tar_target(
      coastline_plot,
      make_coastline_plot(coastline_watershed)
    ),
    tar_target(
      dfdd_stats_bar_plot,
      make_dfdd_stats_bar_plot(river_networks_clip)
    )
  )