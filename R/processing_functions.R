clip_river_networks <-
  function(river_networks, studyarea = NULL) {
    ###### Test
    # river_networks <- tar_read(river_networks)
    # studyarea <- tar_read(filepath_studyarea_subset_plots)
    #####
    
    if (!is.null(studyarea)) {
      river_networks <-
        river_networks %>%
        st_intersection(studyarea, .x) %>% 
        st_cast("MULTILINESTRING") %>% 
        add_feature_index_column()
    }

    return(river_networks)
  }

reclassify_relevant_canals_and_ditches_and_drop_others <- 
  function(river_network, id_to_reclassify) {
    ##### Test
    # river_network <- tar_read(river_networks_clip)
    # id_to_reclassify <- tar_read(features_ids_to_reclassify)
    ####
    river_network %>% 
      mutate(dfdd = if_else(inspire_id %in% id_to_reclassify, "BH140", dfdd)) %>% 
      filter(dfdd == "BH140") %>% 
      add_feature_index_column()
  }


impute_line_features_with_invalid_strahler_value <- 
  function(river_network) {
    ### Test
    # river_network <- tar_read(river_networks_only_rivers)
    ###
    features_with_invalid_strahler <- 
      river_network %>% 
      filter(strahler %in% INVALID_STRAHLER_VALUES)
    
    valid_strahler <-
      features_with_invalid_strahler %>% 
      select(strahler, inspire_id) %>% 
      st_join(select(river_network, strahler), .predicate = st_touches) %>%
      as_tibble() %>%
      mutate(geometry = as.character(geometry)) %>%
      filter(!(strahler.y %in% INVALID_STRAHLER_VALUES)) %>% 
      group_by(geometry, strahler.y) %>% 
      summarise(n = n(), inspire_id = unique(inspire_id)) %>% 
      arrange(n) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(inspire_id, strahler.y)
    
    river_network %>% 
      left_join(valid_strahler, by = "inspire_id") %>% 
      mutate(strahler = if_else(is.na(strahler.y), strahler, strahler.y)) %>% 
      select(-strahler.y) %>% 
      add_feature_index_column()
  }


clean_river_networks <-
  function(river_network) {
    #### Test
    # river_network <- tar_read(river_networks_clip)
    # studyarea <- tar_read(studyarea_outline)
    ###

    river_network <-
      river_network %>%
      keep_relevant_columns() %>%
      remove_invalid_streamorder_values() %>%
      st_zm() %>%
      add_feature_index_column()

    river_network %>%
      st_cast("LINESTRING") %>%
      initiate_database(
        connect_to_database(),
        "lines_clean"
      )

    return(river_network)
  }

split_river_network <-
  function(river_network) {
    river_network %>%
      group_by(connected_feature_id) %>%
      group_split()
  }

determine_studyarea_outline <-
  function(studyarea, coastline){
    ### Test
    # coastline <- tar_read(coastline)
    # studyarea <- tar_read(studyarea_subset_plots)
    ###

    studyarea_outline <-
      studyarea %>%
      st_difference(coastline) %>% 
      st_cast("POLYGON")
    
    studyarea_outline <- 
      studyarea_outline %>% 
      mutate(area = as.numeric(st_area(geometry))) %>% 
      arrange(-area) %>% 
      slice(1) %>% 
      select(geometry)
  }

st_envelope <- 
  function(x) {
    st_as_sfc(st_bbox(x))
  }

remove_disconnected_line_segments <- 
  function(river_network, studyarea) {
    ### Test
    # studyarea <- tar_read(studyarea_outline)
    # river_network <-
    #   tar_read(river_networks_clip) %>%
    #   keep_relevant_columns() %>%
    #   remove_invalid_streamorder_values() %>% 
    #   st_zm()
    ###
    
    river_network_test <-
      river_network %>%
      select(-strahler) %>%
      st_cast("LINESTRING") %>%
      st_geometry()

    river_network_components <-
      river_network_test %>%
      st_touches() %>%
      igraph::graph.adjlist() %>%
      igraph::components()

    membership_lines <-
      do.call(
        st_sfc,
        tapply(
          river_network_test,
          river_network_components$membership,
          function(i) {
            st_multilinestring(river_network_test[i])
          }
        )
      ) %>% 
      st_as_sf()
    
    st_crs(membership_lines) <- 
      st_crs(river_network_test)

    river_network <-
      river_network %>%
      drop_isolated_line_segments(membership_lines,
                                  studyarea)
    
    return(river_network)
  }

drop_isolated_line_segments <- 
  function(river_network, merged_river_network, studyarea) {
    ### Test
    # river_network <-
    #   tar_read(river_networks_clip) %>%
    #   keep_relevant_columns() %>%
    #   remove_invalid_streamorder_values() %>%
    #   st_zm()
    # merged_river_network <- membership_lines
    # studyarea <- tar_read(studyarea_outline)
    ###
    
    isolated_line_segments <-
      merged_river_network %>% 
      st_intersects(st_cast(studyarea, "LINESTRING")) %>% 
      map(as.vector) %>%
      map(sort) %>% 
      map(is_empty) %>% 
      unlist()

    merged_river_network <- 
      merged_river_network %>% 
      add_feature_index_column()
    
    merged_river_network <- 
      merged_river_network %>% 
      filter(!isolated_line_segments)
    
    river_network <- 
      river_network %>%
      add_feature_index_column() %>% 
      st_join(merged_river_network, left = FALSE, .predicate = st_touches)
 
    merged_river_network <- 
      merged_river_network %>% 
      summarise()
    
    river_network <-
      river_network %>% 
      st_filter(merged_river_network) %>% 
        group_by(feature_id.x) %>% 
        arrange(feature_id.y) %>% 
        slice(1) %>% 
        ungroup() %>% 
        rename(feature_id = feature_id.x,
               connected_feature_id = feature_id.y) %>% 
        relocate(connected_feature_id, .after = feature_id)
      # filter(st_intersects(., river_network_test, sparse = FALSE)[,1])
    
    river_network %>% 
      return()
  }


add_feature_index_column <- 
  function(sf_object, column_name = "feature_id") {
    sf_object %>% 
      mutate(!!column_name := as.character(1:n()),
             .before = 1)
  }



keep_relevant_columns <- 
  function(river_network, 
           relevant_columns = c("strahler")) {
    
    river_network %>% 
      select(all_of(relevant_columns))
      # filter(strahler != -9999)
  }

remove_invalid_streamorder_values <- 
  function(river_network, 
           invalid_values = c(-9999)) {
    river_network %>% 
      filter(!(strahler %in% invalid_values))
      # filter(strahler != -9999)
  }

dissolve_line_features_between_junctions <-
  function(river_networks) {
    ###### Test
    # river_networks <- tar_read(river_networks_only_connected)
    # studyarea <- tar_read(studyarea_outline)
    #####

    startpoints <- 
      river_networks %>%
      st_cast("LINESTRING") %>% 
      lwgeom::st_startpoint() %>% 
      st_as_sf()
    
    endpoints <- 
      river_networks %>% 
      st_cast("LINESTRING") %>% 
      lwgeom::st_endpoint() %>% 
      st_as_sf()
    
    start_end_points <- 
      startpoints %>% 
      bind_rows(endpoints)
    
    
    split_points <-
      start_end_points %>% 
      st_intersects(river_networks) %>% 
      map(as_vector) %>%
      map_dbl(length) %>% 
      magrittr::is_greater_than(2) %>% 
      filter(start_end_points, .) %>% 
      distinct(x)
    
    lines_merged <- 
      river_networks %>% 
      group_by(strahler) %>%
      summarise() %>%
      st_line_merge() %>% 
      st_cast("MULTILINESTRING")
    
    lines_merged$geometry %>% 
      lwgeom::st_split(split_points$x) %>% 
      st_collection_extract("LINESTRING") %>% 
      st_as_sf() %>% 
      add_feature_index_column() %>%
      st_join(river_networks, join = st_contains) %>% 
      rename(feature_id = feature_id.x,
             geometry = x) %>% 
      group_by(feature_id) %>% 
      summarise(
        # connected_feature_id = unique(connected_feature_id),
        strahler = unique(strahler )
        ) %>% 
      add_feature_index_column()
  }


dissolve_line_features_with_brackets <- 
  function(river_networks) {
    ##### Test
    # river_networks <- tar_read(river_networks_dissolved_junctions)
    #####
    
    startpoints <- 
      river_networks %>% 
      get_startpoints()
    
    endpoints <-
      river_networks %>% 
      get_endpoints()
    
    start_end_points <-
      startpoints %>% 
      merge_start_and_end_points(endpoints)
    
    bracket_pairs <-
      river_networks %>% 
      get_bracket_pairs(start_end_points)
    
    
    split_points <-
      start_end_points %>% 
      left_join(bracket_pairs, by = "feature_id") %>% 
      drop_na(merge_id)
    
    river_networks %>%
      # filter(feature_id %in% %>%  c(107, 114, 105, 108)) %>% 
      st_join(split_points) %>% 
      st_cast("MULTILINESTRING") %>% 
      mutate(merge_id = if_else(is.na(merge_id), str_c("NA_", row_number()), merge_id)) %>% 
      group_by(merge_id) %>% 
      summarise() %>% 
      st_join(river_networks, join = st_contains) %>% 
      group_by(merge_id) %>% 
      summarise(strahler = unique(strahler )) %>% 
      select(-merge_id) %>% 
      add_feature_index_column()
  }


drop_shorter_bracket_line_features <- 
  function(river_networks) {
    ##### Test
    # river_networks <- tar_read(river_networks_dissolved_junctions)
    #####
    
    startpoints <- 
      river_networks %>% 
      get_startpoints()

    endpoints <-
      river_networks %>% 
      get_endpoints()
    
    start_end_points <-
      startpoints %>% 
      merge_start_and_end_points(endpoints)
    
    bracket_pairs <-
      start_end_points %>% 
      st_join(
        river_networks
      ) %>% 
      as_tibble() %>%
      mutate(x = as.character(x)) %>%
      group_by(feature_id.y) %>%
      filter(n() > 1) %>% 
      arrange(feature_id.x) %>% 
      mutate(start_end_id = str_c("point_", 1:n())) %>% 
      ungroup() %>% 
      pivot_wider(id_cols = "feature_id.y", names_from = "start_end_id", values_from = "feature_id.x") %>% 
      group_by(point_1, point_2) %>% 
      filter(n() > 1) %>% 
      mutate(bracketpair_id = str_c(feature_id.y, collapse = "")) %>% 
      ungroup() %>% 
      rename(feature_id = feature_id.y) %>% 
      select(feature_id, bracketpair_id)
    
    river_networks %>% 
      left_join(bracket_pairs, by = "feature_id") %>% 
      mutate(length = if_else(!is.na(bracketpair_id), st_length(geometry), NA_real_)) %>% 
      mutate(bracketpair_id = if_else(is.na(bracketpair_id), str_c("NA_", row_number()), bracketpair_id)) %>% 
      group_by(bracketpair_id) %>% 
      arrange(length) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(
        # connected_feature_id, 
        strahler) %>% 
      add_feature_index_column()
  }

get_startpoints <- 
  function(river_networks) {
    river_networks %>% 
      st_cast("LINESTRING") %>% 
      lwgeom::st_startpoint() %>% 
      st_as_sf()
  }

get_endpoints <- 
  function(river_networks) {
    river_networks %>% 
      st_cast("LINESTRING") %>% 
      lwgeom::st_endpoint() %>% 
      st_as_sf()
  }

merge_start_and_end_points <- 
  function(startpoints, endpoints) {
    startpoints %>% 
      bind_rows(endpoints) %>% 
      distinct(x) %>%
      add_feature_index_column()
  }

get_bracket_pairs <-
  function(rivernetworks, start_end_points) {
    start_end_points %>% 
      st_join(
        river_networks
      ) %>% 
      as_tibble() %>%
      mutate(x = as.character(x)) %>%
      group_by(feature_id.y) %>%
      filter(n() > 1) %>% 
      arrange(feature_id.x) %>% 
      mutate(start_end_id = str_c("point_", 1:n())) %>% 
      ungroup() %>% 
      pivot_wider(id_cols = "feature_id.y", names_from = "start_end_id", values_from = "feature_id.x") %>% 
      group_by(point_1, point_2) %>% 
      filter(n() > 1) %>% 
      distinct(point_1, point_2) %>% 
      ungroup() %>% 
      add_feature_index_column("merge_id") %>% 
      pivot_longer(cols = -"merge_id", values_to = "feature_id") %>% 
      select(-name)
  }

get_unique_feature_ids <- 
  function(river_network) {
    river_network %>% 
      as_tibble() %>% 
      distinct(feature_id) %>% 
      pull(feature_id)
  }


drop_disconnected_river_networks <-
  function(river_networks, studyarea) {
    ### Test
    # river_networks <- tar_read(river_networks_clean)
    # studyarea <- tar_read(studyarea_outline)
    # connection <- connect_to_database()
    ####
    
    run_query_connected()

    river_network <-
      get_table_from_postgress("connected_id") %>% 
      query_result_as_sf()
    
    river_network <- 
      river_network %>%
      filter(st_intersects(., st_cast(studyarea, "LINESTRING"), sparse = FALSE)[,1]) %>% 
      select(-connected_id) %>%
      st_intersection(river_networks) %>%
      add_feature_index_column()
    
    river_network %>% 
      initiate_database(
        connect_to_database(),
        "lines_connected"
      )
    
    return(river_network)
  }



run_query_connected <- 
  function() {
    
    connection <- 
      connect_to_database()
    
    DBI::dbExecute(connection, "DROP TABLE IF EXISTS connected_id")
    # DBI::dbExecute(connection, read_file("sql/get_connected_id.sql"))
    DBI::dbExecute(connection, "
      CREATE TABLE connected_id AS (
	      WITH endpoints AS (
	        SELECT
	          ST_Collect(ST_StartPoint(geometry),
	          ST_EndPoint(geometry)) AS geometry
	        FROM lines_clean
	      ), clusters AS (
	        SELECT
	          unnest(ST_ClusterWithin(geometry, 1e-8)) AS geometry
	        FROM endpoints
	      ), clusters_with_ids AS (
	        SELECT
	          row_number() OVER () AS connected_id,
	          ST_CollectionHomogenize(geometry) AS geometry
	        FROM clusters
	      )
      	SELECT
		      connected_id,
		      ST_Collect(lines_clean.geometry) AS geometry
	      FROM
	        lines_clean
	        LEFT JOIN
	        clusters_with_ids
	        ON ST_Intersects(lines_clean.geometry, clusters_with_ids.geometry)
	      GROUP BY connected_id
      )
    ")
  }



merge_same_strahler_segments <-
  function(sf_lines) {
    ###### Test
    # sf_lines <- tar_read(river_networks_dissolved_junctions2)
    # query <- tar_read(linemerge_query)
    ###

    connection <- 
      connect_to_database()
    
    sf_lines %>% 
      st_cast("LINESTRING") %>%
      write_to_table(
        connection = connection,
        table_name = "lines_raw"
        )
    
    # connection %>% 
    #   run_query_create_table_brackets(query_list[[1]])
    
    connection %>% 
      run_query_linemerge_by_streamorder() %>% 
      prepare_lines()
  }

# sf_lines_merged %>% 
#   ggplot() +
#   geom_sf(aes(colour = feature_id))

list_to_long_df <-
  function(list) {
    long_format <- 
      tibble(id_a = rep(seq_along(list), lengths(list)), 
             id = unlist(list))
    return(long_format)
  }


extract_strahler_by_index <- 
  function(sf_lines, index_vector) {
    sf_lines %>% 
      as_tibble() %>% 
      slice(index_vector) %>% 
      pull(strahler) %>% 
      as.numeric()
  }

determine_segments_to_merge <-
  function(index_vector,
           adjacent_segments_list,
           adjacent_strahler,
           segment_strahler) {
    ##### Test
    # index <- sample(1:162, 1)
    # adjacent_strahler <- adjacent_strahler_list[[index]]
    # segment_strahler <- segment_strahler_list[[index]]
    ###
    
    if ((segment_strahler %>% is_empty())){
      FALSE
    }
    else if (segment_strahler == max(adjacent_strahler, na.rm = TRUE)) {
      adjacent_segments_list %>%
        # pluck(index) %>%
        magrittr::extract(adjacent_strahler == segment_strahler) %>%
        c(index_vector) %>%
        sort()
    } else {
      FALSE
    }
  }

linked_rows <- function(data){
  ## helper function
  ## returns a _function_ to compare two rows of data
  ##  based on group membership.
  
  ## Use Vectorize so it works even on vectors of indices
  Vectorize(function(i, j) {
    ## numeric: 1= i and j have overlapping group membership
    common <- vapply(names(data), function(name)
      data[i, name] == data[j, name],
      FUN.VALUE=FALSE)
    as.numeric(any(common))
  })
}








stream_order_filter <- 
  function(river_network, stream_order) {
    river_network %>% 
      mutate(strahler = as.numeric(strahler)) %>% 
      filter(strahler >= stream_order)
  }

make_grid <- 
  function(river_network) {
    river_network %>% 
      st_make_grid(cellsize = CELLSIZE) %>% 
      st_as_sf() %>% 
      st_geometry() %>% 
      st_sf()
  }

make_grid_centroids <- 
  function(grid) {
    grid %>% 
      st_centroid() %>% 
      st_geometry() %>% 
      st_sf()
  }


make_thiessen_catchments_centroids <-
  function(river_network, grid, grid_centroids) {
    grid_centroids <-
      grid_centroids %>%
      mutate(nearest_feature = grid %>%
        st_centroid() %>%
        st_nearest_feature(river_network) %>%
        as.character(),
        .before = 1)
  }

centroids_to_grid <- 
  function(centroids, grid) {
    grid %>% 
      st_join(centroids)
  }

make_thiessen_catchments <- 
  function(grid, centroids) {
    centroids %>% 
      centroids_to_grid(grid) %>% 
      group_by(nearest_feature) %>% 
      summarise() %>% 
      st_cast("MULTIPOLYGON") %>% 
      st_geometry() %>% 
      st_sf() %>% 
      return()
  }

calculate_stream_distance_centroids <-
  function(centroids, river_network) {
    centroids %>%
      mutate(
        distance_stream = st_distance(.,
          river_network %>%
            slice(as.numeric(centroids$nearest_feature)),
          by_element = TRUE
        ),
        .before = 2
      ) %>%
      mutate(distance_stream = as.numeric(distance_stream)) %>%
      select(-nearest_feature) %>%
      return()
  }

calculate_divide_distance_centroids <-
  function(centroids, catchments) {
    catchment_watershed <-
      catchments %>%
      st_cast("MULTILINESTRING")

    nearest_features_indices <-
      catchment_watershed %>%
      st_nearest_feature(centroids, .)

    centroids %>%
      mutate(
        distance_divide =
          st_distance(., catchment_watershed %>% slice(nearest_features_indices),
            by_element = TRUE
          ),
        .before = 2
      ) %>%
      mutate(distance_divide = as.numeric(distance_divide)) %>%
      select(-nearest_feature) %>%
      return()
  }


generate_filepaths <- 
  function(stream_order, abbreviation){
    filepath <- 
      str_c("output_data/", 
            "mohp_germany_",
            abbreviation,
            "_", 
            "order", 
            stream_order, 
            "_", 
            CELLSIZE, 
            "m_res", 
            ".tiff")
    
    return(filepath)
  }

calculate_lateral_position_grid <- 
  function(stream_distance_centroids, divide_distance_centroids, stream_order, grid, field_name, directory) {
    ###### test
    # stream_distance_centroids <- tar_read(centroids_stream_distance) %>% pluck(3)
    # divide_distance_centroids <- tar_read(centroids_divide_distance) %>% pluck(3)
    # stream_order <- tar_read(streamorders) %>% pluck(3)
    # grid <- tar_read(base_grid)
    # field_name <- "lateral_position"
    # directory <- tar_read(directory_lateral_position)
    ###

    filepath <- 
      str_c(directory, 
            "mohp_germany_",
            "lp",
            "_", 
            "order", 
            stream_order, 
            "_", 
            CELLSIZE, 
            "m", 
            ".tiff")
    
    unlink(filepath)

    
    divide_distance_centroids %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      bind_cols(stream_distance_centroids, .) %>%
      mutate(lateral_position = distance_stream / (distance_stream + distance_divide),
             .before = 1) %>% 
      select(-contains("distance")) %>% 
      centroids_to_grid(grid) %>% 
      st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>%
      write_stars(filepath, 
                  overwrite = TRUE)
      # sfpolygon_to_raster(field_name) %>% 
      # writeRaster(filepath,
      #             overwrite = TRUE)
    
    return(filepath)
  }

calculate_stream_divide_distance_grid <- 
  function(stream_distance_centroids, divide_distance_centroids, stream_order, grid, field_name, directory) {
    
    filepath <- 
      str_c(directory, 
            "mohp_germany_",
            "dsd",
            "_", 
            "order", 
            stream_order, 
            "_", 
            CELLSIZE, 
            "m", 
            ".tiff")
    
    unlink(filepath)
    
    divide_distance_centroids %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      bind_cols(stream_distance_centroids, .) %>% 
      mutate(stream_divide_distance = distance_stream + distance_divide,
             .before = 1) %>% 
      select(-contains("distance_")) %>% 
      centroids_to_grid(grid) %>% 
      st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>%
      write_stars(filepath, 
                  overwrite = TRUE)
    
      # sfpolygon_to_raster(field_name)
      # writeRaster(str_c("output_data/", "mohp_germany_", "dsd_", "order", stream_order, "_", CELLSIZE, "m_res", ".tiff"),
      #             overwrite = TRUE)
    
    return(filepath)
  }


sfpolygon_to_raster <- function(sf_polygon, field_name) {
  sf_polygon %>% 
    fasterize::fasterize(raster = raster::raster(., res = CELLSIZE),
                         field = field_name)
}
