preprocessing_targets <- c(
  pipeline_for(AREA),
  
  tar_target(
    db_selected_studyarea,
    write_selected_studyarea(
      selected_studyarea,
      SELECTED_STUDYAREA_TABLE
    )#,
    # force = !exists_table(SELECTED_STUDYAREA_TABLE)
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
    filter_rivers(
      river_networks_clip
    )
  ),
  
  tar_target(
    river_networks_imputed_streamorder_canals_as_1,
    impute_streamorder(
      river_networks_non_dry_selected_streamtypes
    )
  ),
  
  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_imputed_streamorder_canals_as_1)
  ),
  
  tar_target(
    db_river_networks_clean,
    write_as_lines_to_db(
      river_networks_clean,
      LINES_CLEAN
    )#,
    # force = !exists_table(LINES_CLEAN)
  ),
  
  tar_target(
    db_river_networks_strahler_merge,
    merge_same_strahler_segments(
      LINES_MERGED,
      LINES_CLEAN,  
      depends_on = list(
        db_river_networks_clean
      )
    )
  ),
  
  tar_target(
    streamorders,
    get_unique_streamorders(
      LINES_MERGED,
      depends_on = list(db_river_networks_strahler_merge)
      )
  ),
  
  tar_target(
    db_river_network_by_streamorder,
    streamorder_filter(
      LINES_BY_STREAMORDER,
      LINES_MERGED,
      streamorders,
      depends_on = list(db_river_networks_strahler_merge)
    ),
    pattern = map(streamorders)
  ),
  
  tar_target( #TODO ST_AsRaster parameter touched=true setzen?! damit grid das polygon überlappt
    db_grid_polygons,
    make_grid_polygons_in_db(
      SELECTED_STUDYAREA_TABLE,
      GRID_POLYGONS_TABLE,
      index_column = "grid_id",
      depends_on = list(db_selected_studyarea, config)
    )
    # force = !exists_table(GRID_POLYGONS_TABLE)
  ),
  
  tar_target(
    db_grid,
    make_grid_centroids_in_db(
      GRID_POLYGONS_TABLE,
      GRID_CENTROIDS,
      index_column = "grid_id",
      geo_index_column = "geometry",
      depends_on = list(db_grid_polygons))
    # force = !exists_table(GRID_CENTROIDS)
  )
)