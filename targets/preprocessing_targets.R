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
    river_networks_only_non_dry_rivers,
    filter_rivers(
      river_networks_clip
    )
  ),
  
  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_only_non_dry_rivers)
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
    river_networks_strahler_merge,
    merge_same_strahler_segments(
      depends_on = list(
        db_river_networks_clean
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
      as.integer() %>%
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