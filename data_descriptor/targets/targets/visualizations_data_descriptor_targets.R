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
    tar_target(
      dataset_map_overview_plot,
      make_dataset_map_overview_plot(
        selected_hydrologic_orders = selected_hydrologic_orders, 
        spatial_coverage = spatial_coverage
      )
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
      dfdd_stats_bar_plot,
      make_dfdd_stats_bar_plot(river_networks_clip)
    )
  )