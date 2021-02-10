import_targets <-
  list(

    tar_target( #TODO tar_force or tar_target with format = "file
      river_networks_files,
      list_river_network_files(DIRECTORY_RIVER_NETWORKS),
      # format = "file"
    ),
    
    tar_target(
      river_networks,
      river_networks_files %>% 
        future_map_dfr(read_river_networks)
    ),
    
    # tar_target(
    #   river_basins,
    #   river_networks_files %>%
    #     future_map_dfr(read_river_basins)
    # ),
    
    tar_target(
      directory_nonrivers_to_reclassify,
      DIRECTORY_NONRIVERS_TO_RECLASSIFY,
      format = "file"
    ),
    tar_target(
      features_ids_to_reclassify,
      get_feature_ids_to_reclassify(directory_nonrivers_to_reclassify)
    )
  )