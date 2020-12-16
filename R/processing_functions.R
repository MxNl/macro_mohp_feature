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


merge_same_strahler_segments <-
  function(sf_lines) {
    ###### Test
    # sf_lines <- river_networks_clean
    ###
    
    adjacent_segments_list <-
      sf_lines %>%
      st_touches()
    
    adjacent_segments_list <-
      adjacent_segments_list %>%
      map(as.vector) %>%
      # imap(add_index) %>%
      map(sort)
    
    adjacent_strahler_list <-
      adjacent_segments_list %>%
      map(~extract_strahler_by_index(sf_lines, .))
    
    segment_strahler_list <-
      sf_lines %>%
      rowwise() %>%
      group_split() %>%
      imap(~ extract_strahler_by_index(sf_lines, .y))
    
    segments_to_merge <-
      list(
        seq_along(adjacent_segments_list),
        adjacent_segments_list,
        adjacent_strahler_list,
        segment_strahler_list
      ) %>%
      pmap(determine_segments_to_merge)
    
    segments_to_merge <-
      segments_to_merge %>%
      discard(isFALSE) %>%
      list_to_long_df() %>%
      lump() %>%
      distinct(id, combined_group)
    
    merged_lines <- 
      sf_lines %>%
      mutate(id = 1:n()) %>%
      left_join(segments_to_merge,
                by = "id"
      ) %>%
      mutate(combined_group = as.character(combined_group)) %>%
      mutate(combined_group = if_else(is.na(combined_group),
                                      str_c("nogroup_", id),
                                      combined_group
      )) %>%
      select(-id) %>%
      group_by(combined_group) %>%
      summarise(strahler = first(strahler))
    
    return(merged_lines)
  }


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

lump_links <- function(A) {
  A <- A %*% A
  A[A > 0] <- 1
  A
}

lump <- function(df) {
  rows <- 1:nrow(df)
  A <- outer(rows, rows, linked_rows(df))
  
  oldA <- 0
  while (any(oldA != A)) {
    oldA <- A
    A <- lump_links(A)
  }
  df$combined_group <- cutree(hclust(as.dist(1 - A)), h = 0)
  df
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
  function(stream_distance_centroids, divide_distance_centroids, stream_order, grid, field_name) {
    ###### test
    # stream_distance_centroids <- tar_read(centroids_stream_distance)
    # divide_distance_centroids <- tar_read(centroids_divide_distance)
    ###
    
    filepath <- 
      str_c("output_data/", 
            "mohp_germany_",
            "lp",
            "_", 
            "order", 
            stream_order, 
            "_", 
            CELLSIZE, 
            "m_res", 
            ".tiff")
    
    divide_distance_centroids %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      bind_cols(stream_distance_centroids, .) %>%
      mutate(lateral_position = distance_stream / (distance_stream + distance_divide),
             .before = 1) %>% 
      select(-contains("distance")) %>% 
      centroids_to_grid(grid) %>% 
      st_rasterize() %>% 
      write_stars(filepath, 
                  overwrite = TRUE)
      # sfpolygon_to_raster(field_name) %>% 
      # writeRaster(filepath,
      #             overwrite = TRUE)
    
    # return(file_path)
  }

calculate_stream_divide_distance_grid <- 
  function(stream_distance_centroids, divide_distance_centroids, stream_order, grid, field_name) {
    
    filepath <- 
      str_c("output_data/", 
            "mohp_germany_",
            "dsd",
            "_", 
            "order", 
            stream_order, 
            "_", 
            CELLSIZE, 
            "m_res", 
            ".tiff")
    
    divide_distance_centroids %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      bind_cols(stream_distance_centroids, .) %>% 
      mutate(stream_divide_distance = distance_stream + distance_divide,
             .before = 1) %>% 
      select(-contains("distance_")) %>% 
      centroids_to_grid(grid) %>% 
      st_rasterize() %>%
      write_stars(filepath, 
                  overwrite = TRUE)
    
      # sfpolygon_to_raster(field_name)
      # writeRaster(str_c("output_data/", "mohp_germany_", "dsd_", "order", stream_order, "_", CELLSIZE, "m_res", ".tiff"),
      #             overwrite = TRUE)
  }


sfpolygon_to_raster <- function(sf_polygon, field_name) {
  sf_polygon %>% 
    fasterize::fasterize(raster = raster::raster(., res = CELLSIZE),
                         field = field_name)
}
