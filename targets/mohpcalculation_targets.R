mohpcalculation_targets <- list(

  tar_target(
    db_objects_to_grass,
    write_objects_to_grassdb(
      LINES_BY_STREAMORDER,
      reference_raster,
      selected_studyarea,
      streamorders
    ),
    pattern = map(streamorders)
  )
)