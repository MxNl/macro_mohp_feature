import_targets <-
  list(
    tar_target(
      filepath_studyarea,
      "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/germany_buffer/time_invariant/shape/self_processed/data/buffer_germany_point6.shp",
      format = "file"
    ),
    tar_target(
      studyarea,
      read_studyarea(filepath_studyarea)
    ),
    
    tar_target(
      filepath_studyarea_subset_plots,
      "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/arbitrary/pipeline_test_studyarea/macro_datapreparation_pipeline_test_studyarea.shp",
      format = "file"
    ),
    tar_target(
      studyarea_subset_plots,
      read_studyarea(filepath_studyarea_subset_plots)
    ),
    
    tar_target(
      directory_river_networks,
      "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data/"
    ),
    tar_files(
      river_networks_files,
      list_river_network_files(directory_river_networks)
    ),
    tar_target(
      river_networks,
      read_river_networks(river_networks_files),
      pattern = map(river_networks_files)
    ),
    
    tar_target(
      directory_river_basins,
      "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data/"
    ),
    tar_files(
      river_basins_files,
      list_river_basin_files(directory_river_basins)
    ),
    # tar_target(
    #   river_basins,
    #   read_river_basins(river_basins_files),
    #   pattern = map(river_basins_files)
    # ),
    
    tar_target(
      filepath_coastline,
      "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_coastline/europe/time_invariant/shape/EUHYDRO_Coastline_EEA39_v013/Shapefile/EUHYDRO_Coastline_EEA39_v013.shp"
    ),
    tar_target(
      coastline,
      read_coastline(filepath_coastline)
    ),
    
    tar_target(
      filepath_canals_to_reclassify,
      "qgis/line_features_to_reclassify",
      format = "file"
    ),
    tar_target(
      features_ids_to_reclassify,
      get_feature_ids_to_reclassify(filepath_canals_to_reclassify)
    )
  )