mohpcalculation_targets <- list(
  tar_target(
    db_objects_to_grass,
    calculate_mohp_metrics_in_grassdb(
      LINES_STUDYAREA,
      INLAND_WATERS_STRAHLER,
      selected_studyarea,
      streamorders,
      coastline,
      depends_on = list(
        db_river_networks_strahler_studyarea,
        db_inland_waters_strahler)
    ),
    pattern = map(streamorders),
    deployment = "main"
  )
)