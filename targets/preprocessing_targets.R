preprocessing_targets <- c(
  pipeline_for(AREA),
  
  # tar_target(
  #   db_selected_studyarea,
  #   write_selected_studyarea(
  #     selected_studyarea,
  #     SELECTED_STUDYAREA_TABLE
  #   )#,
  #   # force = !exists_table(SELECTED_STUDYAREA_TABLE)
  # ),

  tar_target(
    inland_waters_studyarea,
    inland_waters %>% 
      filter_inland_waters(selected_studyarea),
    pattern = map(inland_waters),
    iteration = "vector"
  ),

  tar_target(
    river_networks_clip,
    clip_river_networks(
      river_networks,
      selected_studyarea
    )
  ),

  tar_target(
    river_networks_non_dry_selected_streamtypes,
    filter_rivers(river_networks_clip)
  ),
  
  tar_target(
    river_networks_imputed_streamorder_canals_as_1,
    impute_streamorder(river_networks_non_dry_selected_streamtypes)
  ),
  
  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_imputed_streamorder_canals_as_1)
  ),
  
  tar_target(
    inland_waters_rivers,
    inland_waters_studyarea %>% 
      filter_intersecting_features(river_networks_clean)
  ),

  tar_target(
    db_river_networks_clean,
    write_as_lines_to_db(
      river_networks_clean,
      LINES_CLEAN
    )
  ),
  
  tar_target(
    db_river_networks_strahler_merge,
    merge_same_strahler_segments(
      LINES_MERGED,
      LINES_CLEAN,
      river_basin_names,
      depends_on = list(
        db_river_networks_clean
      )
    ),
    pattern = map(river_basin_names)
  ),
  
  tar_target(
    db_river_networks_strahler_merge_union,
    union_per_basin_merge(
      LINES_MERGED,
      river_basin_names,
      depends_on = list(
        db_river_networks_strahler_merge
      )
    )
  ),
  
  tar_target(
    inland_waters_strahler,
    inland_waters_rivers %>% 
      join_streamorder_to_inland_waters(LINES_MERGED)
  ),
  
  tar_target(
    streamorders,
    get_unique_streamorders(
      LINES_MERGED,
      depends_on = list(db_river_networks_strahler_merge_union)
      )
  ),
  
  tar_target(
    db_river_network_by_streamorder,
    streamorder_filter(
      LINES_BY_STREAMORDER,
      LINES_MERGED,
      streamorders,
      depends_on = list(db_river_networks_strahler_merge_union)
    ),
    pattern = map(streamorders)
  ),

  tar_target(
    reference_raster,
    make_reference_raster(
      selected_studyarea,
      depends_on = list(config)
    )
  ),

  tar_target(
    reference_raster_disk,
    write_reference_raster(reference_raster)
  )
)