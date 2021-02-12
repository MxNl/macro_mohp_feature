preprocessing_targets <- c(
  pipeline_for(AREA),

  tar_force(
    db_selected_studyarea,
    write_selected_studyarea(
      selected_studyarea,
      SELECTED_STUDYAREA_TABLE
    ),
    force = TRUE
  ),

  tar_target(
    river_networks_clip,
    clip_river_networks(
      river_networks,
      #river_basins,
      selected_studyarea
    )
  ),

  tar_target(
    river_networks_only_rivers,
    reclassify_relevant_canals_and_ditches_and_drop_others(
      river_networks_clip,
      features_ids_to_reclassify
    )
  ),

  tar_target(
    river_networks_valid_strahler,
    impute_line_features_with_invalid_strahler_value(river_networks_only_rivers)
  ),

  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_valid_strahler)
  ),

  tar_target(
    db_river_networks_clean,
    write_as_lines_to_db(
      river_networks_clean,
      LINES_CLEAN
    )
  ),

  tar_target(
    db_connected_but_merged_river_networks,
    write_connected_but_merged_river_networks(
      LINES_CLEAN,
      LINES_CONNECTED_ID,
      depends_on = list(
        db_river_networks_clean
      )
    )
  ),

  tar_target(
    river_networks_only_connected,
    drop_disconnected_river_networks(
      river_networks_clean,
      selected_studyarea,
      LINES_CONNECTED_ID,
      depends_on = list(
        db_river_networks_clean,
        db_connected_but_merged_river_networks
      )
    )
  ),

  tar_target(
    river_networks_dissolved_junctions,
    dissolve_line_features_between_junctions(river_networks_only_connected)
  ),

  tar_target(
    river_networks_without_brackets,
    drop_shorter_bracket_line_features(river_networks_dissolved_junctions)
  ),

  tar_target(
    river_networks_dissolved_junctions_after,
    dissolve_line_features_between_junctions(river_networks_without_brackets)
  ),

  tar_target(
    db_river_networks_dissolved_junctions_after,
    write_as_lines_to_db(
      river_networks_dissolved_junctions_after,
      LINES_RAW)
  ),

  tar_target(
    river_networks_strahler_merge,
    merge_same_strahler_segments(
      depends_on = list(
        db_river_networks_dissolved_junctions_after
      )
    )
  ),

  tar_target(
    streamorders,
    river_networks_strahler_merge %>%
      as_tibble() %>%
      distinct(strahler) %>%
      pull(strahler) %>%
      as.numeric() %>%
      sort()
  ),

  tar_target(
    river_network_by_streamorder,
    streamorders %>%
      as.vector() %>%
      as.numeric() %>%
      future_map(
        ~stream_order_filter(
          river_network = river_networks_strahler_merge,
          stream_order = .x
        )
      )
  ),

  tar_target(
    db_river_network_by_streamorder,
    write_by_streamorder(
      streamorders,
      river_network_by_streamorder
    ),
    pattern = map(streamorders)
  ),
  # TODO: make dependent on config.yml
  tar_force(
    cellsize,
    CELLSIZE,
    force = TRUE
  ),

  tar_target( #TODO ST_AsRaster parameter touched=true setzen?! damit grid das polygon überlappt
    db_grid_polygons,
    make_grid_polygons_in_db(
      SELECTED_STUDYAREA_TABLE,
      GRID_POLYGONS_TABLE,
      index_column = "grid_id",
      depends_on = list(db_selected_studyarea, cellsize)
    )
  ),

  tar_target(
    db_grid,
    make_grid_centroids_in_db(
      GRID_POLYGONS_TABLE,
      GRID_CENTROIDS,
      index_column = "grid_id",
      depends_on = list(db_grid_polygons))
  ),
  # TODO: do in target above.
  tar_target(
    db_geo_indices,
    set_geo_indices(GRID_CENTROIDS, depends_on = db_grid)
  )
)