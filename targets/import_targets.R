import_targets <-
  list(
    
    tar_target(
      config,
      FILEPATH_CONFIG,
      format = "file"
    ),
    
    tar_target(
      data_import_test,
      read_sf("docker-mount/river_data/test_data/river_network_pipeline_test.shp")
    ),
    
    tar_target(
      directory_river_networks,
      DIRECTORY_RIVER_NETWORKS#,
      # TODO uncomment for final run
      # format = "file"
    ),
    
    tar_target(
      river_networks_files,
      list_river_network_files(directory_river_networks),
    ),
    
    tar_target(
      river_networks,
      river_networks_files %>% 
        future_map_dfr(read_river_networks)
    )
  )