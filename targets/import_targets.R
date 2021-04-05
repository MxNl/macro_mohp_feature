import_targets <-
  list(
    
    tar_target(
      config,
      FILEPATH_CONFIG,
      format = "file"
    ),
    
    tar_target(
      directory_river_networks,
      DIRECTORY_RIVER_NETWORKS
    ),
    
    tar_target(
      river_networks_files,
      list_river_network_files(directory_river_networks),
    ),
    
    tar_target(
      river_networks,
      river_networks_files %>% 
        read_river_networks_parallel()
    ),

    tar_target(
      inland_waters,
      river_networks_files %>% 
        read_inland_waters(),
      pattern = map(river_networks_files),
      iteration = "list"
    )
  )