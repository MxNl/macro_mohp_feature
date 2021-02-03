db_hash_targets <- 
  list(
    tar_force(
      hash_db_river_networks_clean,
      hash_of_db(LINES_CLEAN),
      force = TRUE
    ),
    tar_force(
      hash_db_river_networks_dissolved_junctions_after,
      hash_of_db(LINES_RAW),
      force = TRUE
    )
  )