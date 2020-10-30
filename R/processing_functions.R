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
  function(river_networks) {
    ###### Test
    # river_networks <- tar_read(river_networks_clip)
    # studyarea <- tar_read(filepath_studyarea_subset_plots)
    #####
    
    river_networks_merge <- 
      river_networks %>% 
      select(strahler) %>%
      filter(strahler != -9999)
    
    river_networks_dissolved <- 
      river_networks_merge %>%
      group_by(strahler) %>%
      summarise() %>%
      st_line_merge()
    
    result_intersection <- 
      river_networks_merge %>% 
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

stream_order_filter <- 
  function(river_network, stream_order) {
    river_network %>% 
      filter(strahler %in% 1:stream_order)
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
  function(centroids, river_network, catchments) {
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

calculate_lateral_position_centroids <- 
  function(stream_distance_centroids, divide_distance_centroids, grid) {
    stream_distance_centroids %>% 
      st_join(divide_distance_centroids) %>%
      mutate(lateral_position = distance_stream / (distance_stream + distance_divide),
             .before = 1) %>% 
      select(-contains("distance")) %>% 
      centroids_to_grid(grid)
  }

calculate_stream_divide_distance_centroids <- 
  function(stream_distance_centroids, divide_distance_centroids, grid) {
    stream_distance_centroids %>% 
      st_join(divide_distance_centroids) %>%
      mutate(stream_divide_distance = distance_stream + distance_divide,
             .before = 1) %>% 
      select(-contains("distance_")) %>% 
      centroids_to_grid(grid)
  }

