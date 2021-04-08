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
    db_inland_waters,
    write_inland_waters(
      inland_waters,
      INLAND_WATERS,
      geo_index_column = "geometry"
    )
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
    db_river_networks_strahler_studyarea,
    filter_rivers_in_studyarea(
      LINES_STUDYAREA,
      LINES_MERGED,
      SELECTED_STUDYAREA_TABLE,
      "geometry",
      depends_on = list(
        db_river_networks_strahler_merge_union,
        db_selected_studyarea
      )
    )
  ),
  
  tar_target(
    db_inland_waters_strahler,
      join_streamorder_to_inland_waters(
        INLAND_WATERS_STRAHLER,
        INLAND_WATERS,
        LINES_STUDYAREA,
        depends_on = list(
          db_river_networks_strahler_studyarea,
          db_inland_waters
          )
        )
  ),
  
  tar_target(
    streamorders,
    get_unique_streamorders(
      LINES_STUDYAREA,
      depends_on = list(db_river_networks_strahler_studyarea)
      )
  )
)