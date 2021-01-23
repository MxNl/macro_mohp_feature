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
      filepath_river_networks,
      "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data/"
    ),
    tar_target(
      river_networks,
      read_river_networks(filepath_river_networks)
    ),
    
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
    ),
    
    tar_target(
      filepath_create_table_brackets_query,
      "sql/filter_brackets.sql",
      format = "file"
    ),
    tar_target(
      create_table_brackets_query,
      read_file(filepath_create_table_brackets_query)
    ),
    
    tar_target(
      filepath_linemerge_query,
      "sql/linemerge_query.sql",
      format = "file"
    ),
    tar_target(
      linemerge_query,
      read_file(filepath_linemerge_query)
    ),
    
    tar_target(
      filepath_connected_query,
      "sql/get_connected_id.sql",
      format = "file"
    ),
    tar_target(
      connected_query,
      read_file(filepath_connected_query)
    )
  )