import_targets <-
  list(
    # tar_target(
    #   filepath_studyarea_germany,
    #   FILEPATH_STUDYAREA_GERMANY,
    #   format = "file"
    # ),
    # 
    # tar_target(
    #   studyarea_germany,
    #   read_studyarea(filepath_studyarea_germany)
    # ),
    # 
    # tar_target(
    #   filepath_studyarea_pipelinetest,
    #   FILEPATH_STUDYAREA_PIPELINETEST,
    #   format = "file"
    # ),
    # 
    # tar_target(
    #   studyarea_pipelinetest,
    #   read_studyarea(filepath_studyarea_pipelinetest)
    # ),

    tar_target(
      river_networks_files,
      list_river_network_files(DIRECTORY_RIVER_NETWORKS)
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
    
    # tar_target(
    #   directory_river_networks,
    #   DIRECTORY_RIVER_NETWORKS
    # ),    
    # tar_target(
    #   directory_river_networks,
    #   DIRECTORY_RIVER_NETWORKS
    # ),
    # tar_files(
    #   river_networks_files,
    #   list_river_network_files(directory_river_networks)
    # ),
    # tar_target(
    #   river_networks,
    #   read_river_networks(river_networks_files),
    #   pattern = map(river_networks_files)
    # ),
    # 
    # tar_target(
    #   river_basins,
    #   read_river_basins(river_networks_files),
    #   pattern = map(river_networks_files)
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