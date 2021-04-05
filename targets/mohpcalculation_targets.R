mohpcalculation_targets <- list(
  tar_target(
    db_objects_to_grass,
    calculate_mohp_metrics_in_grassdb(
      LINES_BY_STREAMORDER,
      inland_waters_strahler,
      reference_raster,
      selected_studyarea,
      streamorders,
      coastline,
      depends_on = list(
        reference_raster_disk,
        db_river_network_by_streamorder)
    ),
    pattern = map(streamorders),
    deployment = "main"
  )
)