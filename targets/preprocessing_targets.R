preprocessing_targets <- c(
  pipeline_for(AREA),

  # helper target -----------------------------------------------------------
  tar_target(
    db_selected_studyarea,
    write_selected_studyarea(
      selected_studyarea,
      SELECTED_STUDYAREA_TABLE,
      geo_index_column = "geometry"
    )
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    db_inland_waters,
    write_inland_waters(
      inland_waters,
      INLAND_WATERS,
      geo_index_column = "geometry"
    )
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    river_networks_clip,
    clip_river_networks(
      river_networks,
      selected_studyarea
    )
  ),
  # river_networks_non_dry_selected_streamtypes -----------------------------
  tar_target(
    river_networks_non_dry_selected_streamtypes,
    filter_rivers(river_networks_clip)
  ),
  # river_networks_imputed_streamorder_canals_as_1 --------------------------
  tar_target(
    river_networks_imputed_streamorder_canals_as_1,
    impute_streamorder(river_networks_non_dry_selected_streamtypes)
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_imputed_streamorder_canals_as_1)
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    db_river_networks_clean,
    write_as_lines_to_db(
      river_networks_clean,
      LINES_CLEAN,
      geo_index_column = "geometry"
    )
  ),
  # db_river_networks_strahler_studyarea ------------------------------------
  tar_target(
    db_river_networks_strahler_studyarea,
    filter_rivers_in_studyarea(
      LINES_STUDYAREA,
      LINES_CLEAN,
      SELECTED_STUDYAREA_TABLE,
      "geometry",
      depends_on = list(
        db_river_networks_clean,
        db_selected_studyarea
      )
    )
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    streamorders,
    get_unique_streamorders(
      LINES_STUDYAREA,
      depends_on = list(db_river_networks_strahler_studyarea)
    )
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    distinct_streamorders_in_riverbasins,
    get_distinct_streamorders_in_riverbasins(
      LINES_STUDYAREA,
      depends_on = list(db_river_networks_strahler_studyarea)
      ) %>%
      rowwise() %>%
      tar_group(),
    iteration = "group"
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    major_path_ids,
    get_unique_major_path_ids(
      LINES_STUDYAREA,
      depends_on = list(db_river_networks_strahler_studyarea)
      )
  ),
  # rivernetworks_merged_per_streamorder ------------------------------------
  tar_target(
    rivernetworks_merged_per_streamorder,
    merge_rivernetworks_per_streamorder(
      LINES_STUDYAREA,
      major_path_ids,
      distinct_streamorders_in_riverbasins,
      depends_on = list(
        db_river_networks_strahler_studyarea
      )
    ) %>% 
      group_by(streamorder) %>%
      tar_group(),
    iteration = "group",
    pattern = map(distinct_streamorders_in_riverbasins),
  ),
  # rivernetworks_feature_id ------------------------------------------------
  tar_target(
    rivernetworks_feature_id,
    order_by_length_and_add_feature_id(rivernetworks_merged_per_streamorder),
    pattern = map(rivernetworks_merged_per_streamorder),
    iteration = "group"
  ),
  # helper target -----------------------------------------------------------
  tar_target(
    db_river_networks_merged_per_streamorder,
    write_as_is_to_db(
      rivernetworks_feature_id,
      LINES_MERGED,
      "geometry"
    ),
    pattern = map(rivernetworks_feature_id)
  ),
  # db_inland_waters_strahler -----------------------------------------------
  tar_target(
    db_inland_waters_strahler,
    join_streamorder_to_inland_waters(
      INLAND_WATERS_STRAHLER,
      INLAND_WATERS,
      LINES_MERGED,
      streamorders,
      depends_on = list(
        db_river_networks_merged_per_streamorder,
        db_inland_waters
      )
    ),
    pattern = map(streamorders)
  )
)