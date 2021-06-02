visualizations_data_descriptor_targets <- 
  list(
    tar_target(
      input_data_table,
      make_input_data_table()
    ),
    tar_target(
      selected_hydrologic_orders,
      c(3, 4)
    ),
    tar_target(
      dataset_map_overview_plot,
      make_dataset_map_overview_plot(selected_hydrologic_orders = selected_hydrologic_orders)
    ),
    tar_target(
      output_data_table,
      make_output_data_table()
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