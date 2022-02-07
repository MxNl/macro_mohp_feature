export_targets <- list(
  # helper target -----------------------------------------------------
  tar_target(
    eumohp_filepaths,
    list_eumohp_filepaths(OUTPUT_DIRECTORY, depends_on = db_objects_to_grass),
    iteration = "list"
  ),
  # helper target -----------------------------------------------------
  tar_target(
    eumohp_files_compression,
    compress_eumohp_files(eumohp_filepaths),
    pattern = map(eumohp_filepaths)
  )
)