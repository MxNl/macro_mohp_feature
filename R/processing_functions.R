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
        st_cast("MULTILINESTRING")
    }

    return(river_networks)
  }

clean_river_networks <- 
  function(river_network, studyarea) {
    #### Test
    # river_network <- tar_read(river_networks_clip)
    # studyarea <- tar_read(studyarea_outline)
    ###
    
    river_network <- 
      river_network %>% 
      keep_relevant_columns() %>%
      remove_invalid_streamorder_values() %>% 
      st_zm() %>% 
      remove_disconnected_line_segments(studyarea)
  }

split_river_network <-
  function(river_network) {
    river_network %>%
      # st_cast("GEOMETRY") %>%
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
  function(sf_object) {
    sf_object %>% 
      mutate(feature_id = as.character(1:n()),
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
    # river_networks <- tar_read(river_networks_clip)
    # studyarea <- tar_read(filepath_studyarea_subset_plots)
    #####

    river_networks_dissolved <-
      river_networks %>%
      group_by(strahler) %>%
      summarise() %>%
      st_line_merge()

    result_intersection <-
      river_networks %>%
      select(-strahler) %>%
      st_intersection(river_networks_dissolved %>%
                        # select(-strahler) %>%
                        st_cast("MULTILINESTRING"))

    result_intersection_points <-
      result_intersection %>%
      filter(st_is(., "POINT")) %>%
      distinct(geometry)

    river_networks_clean <-
      river_networks_dissolved %>%
      st_cast("MULTILINESTRING") %>%
      st_union() %>%
      lwgeom::st_split(st_combine(result_intersection_points)) %>%
      st_collection_extract("LINESTRING") %>%
      st_as_sf() %>%
      st_cast("MULTILINESTRING") %>%
      rename(geometry = x)

    river_networks_clean <-
      river_networks_clean %>%
      st_intersection(river_networks_dissolved) %>%
      filter(st_is(., "MULTILINESTRING")) %>%
      distinct(geometry, strahler) %>%
      mutate(strahler = as.factor(strahler))

    river_networks_clean %>%
      return()
  }

get_unique_feature_ids <- 
  function(river_network) {
    river_network %>% 
      as_tibble() %>% 
      distinct(feature_id) %>% 
      pull(feature_id)
  }


merge_same_strahler_segments <-
  function(sf_lines, query) {
    ###### Test
    # sf_lines <- tar_read(river_networks_clean)
    # query <- tar_read(linemerge_query)
    ###

    # sf_lines <-
    #   sf_lines %>%
    #   pluck(2)
    
    connection <- 
      connect_to_database()
    
    sf_lines %>% 
      st_cast("LINESTRING") %>% 
      write_to_table(
        connection = connection,
        table_name = "lines"
        )
    
    sf_lines_merged <- 
      connection %>% 
      run_query_linemerge_by_streamorder(query) %>% 
      prepare_lines()
    
    return(sf_lines_merged)
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
    # stream_distance_centroids <- tar_read(centroids_stream_distance)
    # divide_distance_centroids <- tar_read(centroids_divide_distance)
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
    
    # unlink(filepath)
    
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
    
    # unlink(filepath)
    
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
