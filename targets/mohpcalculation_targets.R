mohpcalculation_targets <- 
  list(
    
    tar_target(
      # TODO: rename to schema
      db_nearest_neighbours,
      nearest_neighbours_between(
        table_name_destination = NN_GRID_RIVERS_TABLE,
        left_table = GRID_CENTROIDS,
        right_table = LINES_BY_STREAMORDER,
        left_columns = c("grid_id", "geometry"),
        right_columns = c("feature_id", "strahler", "stream_order_id"),
        stream_order_id = streamorders,
        right_table_is_long_format = TRUE,
        depends_on = list(
          db_river_network_by_streamorder,
          db_grid
        )
      ),
      pattern = map(streamorders)
    ),
    
    tar_target(
      thiessen_catchments,
      make_thiessen_catchments(
        stream_order_id = streamorders,
        depends_on = list(db_nearest_neighbours, db_grid_polygons)
      ),
      pattern = map(streamorders)
    ),
    
    tar_target(
      # TODO: rename to schema
      db_nearest_neighbours_between_grid_and_catchments,
      nearest_neighbours_between(
        table_name_destination = NN_GRID_CATCHMENTS_TABLE,
        left_table = GRID_CENTROIDS,
        right_table = composite_name(THIESSEN_CATCHMENTS_TABLE, streamorders),
        left_columns = c("grid_id"),
        stream_order_id = streamorders,
        depends_on = list(
          thiessen_catchments,
          db_grid
        )
      ),
      pattern = map(streamorders)
    ),
    
    tar_target(
      # TODO: rename to schema
      db_lateral_position_stream_divide_distance,
      calculate_lateral_position_stream_divide_distance(
        stream_order_id = streamorders,
        depends_on = list(
          db_nearest_neighbours,
          db_nearest_neighbours_between_grid_and_catchments
        )
      ),
      pattern = map(streamorders)
    ),
    
    tar_target(
      lateral_position_stream_divide_distance,
      read_lateral_position_stream_divide_distance_from_db(
        MOHP_FEATURES_TABLE,
        streamorders,
        depends_on = list(db_lateral_position_stream_divide_distance)
      ),
      pattern = map(streamorders),
      iteration = "list"
    )#,
    
    # tar_target(
    #   raster_lateral_position_stream_divide_distance,
    #   
    # )
  )