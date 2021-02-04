mohpcalculation_targets <- 
  list(
    
    tar_target(
      nearest_neighbours,
      nearest_neighbours_between(
        table_name = 'nearest_neighbours_streamorder',
        left_table = GRID_CENTROIDS,
        right_table = LINES_BY_STREAMORDER,
        left_columns = c("id", "geometry"),
        right_columns = c("feature_id", "strahler", "stream_order_id", "geometry"),
        stream_order_id = streamorders,
        right_table_is_long_format = TRUE,
        depends_on = list(
          db_river_network_by_streamorder,
          db_grid,
          db_geo_indices
        )
      ),
      pattern = map(streamorders)
    ),
    
    tar_target(
      # TODO: make dependency with nearest_neighbours_between_grid_and_catchments_1
      thiessen_catchments,
      make_thiessen_catchments(
        left_table = GRID_POLYGONS,
        stream_order_id = 1,
        depends_on = list(
          nearest_neighbours
        )
      )
    ),
    
    tar_target(
      db_geo_indices_thiessen_1,
      set_geo_indices(
        c('thiessen_catchments_1'),
        depends_on = list(thiessen_catchments)
      )
    ),
    
    tar_target(
      nearest_neighbours_between_grid_and_catchments_1,
      nearest_neighbours_between(
        table_name = 'nearest_neighbours_between_grid_and_catchments',
        left_table = GRID_CENTROIDS,
        right_table = 'thiessen_catchments_1',
        left_columns = c("id"),
        right_columns = c("river_network_by_streamorder_feature_id"),
        stream_order_id = 1,
        depends_on = list(
          thiessen_catchments,
          db_grid,
          db_geo_indices_thiessen_1
        )
      )
    ),
    
    tar_target(
      db_lateral_position_stream_divide_distance,
      lateral_position_stream_divide_distance(
        "lateral_position_stream_divide_distance",
        "nearest_neighbours_between_grid_and_catchments",
        "nearest_neighbours_streamorder",
        stream_order_id = 1,
        depends_on = list(
          nearest_neighbours,
          nearest_neighbours_between_grid_and_catchments_1
        )
      )
    )
  )