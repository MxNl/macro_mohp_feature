write_by_streamorder <- function(streamorder, river_networks_by_streamorder) {
  table <- composite_name(LINES_BY_STREAMORDER, streamorder)

  river_networks_by_streamorder %>%
    chuck(streamorder) %>%
    write_to_table(table)
  set_geo_index(table)
}

clip_river_networks <-
  function(
    river_networks,
    studyarea
  ) {
    ###### Test
    # river_networks <- tar_read(river_networks)
    # river_basins <- tar_read(selected_studyarea)
    # studyarea <- tar_read(selected_studyarea)
    #####

    if (AREA %in% c("test", "germany")) {

      river_networks %>%
        #filter(river_basin_name %in% relevant_river_basins) %>% 
        filter(as.vector(st_intersects(., studyarea, sparse = FALSE))) %>%
        st_intersection(studyarea) %>%
        st_cast("MULTILINESTRING") %>%
        add_feature_index_column()
    } else if (AREA == "europe") {
      river_networks %>%
        add_feature_index_column()
    } else {
      stop("Please provide a valid character string for the AREA/area parameter in config.yml")
    }
  }

filter_rivers <-
  function(river_network) {
    ##### Test
    # river_network <- tar_read(river_networks_clip)
    # river_network <- tar_read(river_network_pipeline_test)
    # id_to_reclassify <- tar_read(features_ids_to_reclassify)
    ####
    river_network %>%
      filter(dfdd == "BH140") %>%
      add_feature_index_column()
  }

clean_river_networks <-
  function(river_network) {
    #### Test
    # river_network <- tar_read(river_networks_clip)
    # studyarea <- tar_read(studyarea_outline)
    ###

    river_network %>%
      keep_relevant_columns() %>%
      st_zm() %>%
      streamorder_as_integer() %>% 
      add_feature_index_column()
  }

streamorder_as_integer <- 
  function(river_network) {
    river_network %>% 
      mutate(strahler = as.integer(strahler))
  }

determine_studyarea_outline_level_germany <-
  function(studyarea, coastline) {
    ### Test
    # coastline <- tar_read(coastline)
    # studyarea <- tar_read(studyarea_subset_plots)
    ###

    studyarea_outline <-
      studyarea %>%
        st_difference(coastline) %>%
        st_cast("POLYGON")

    studyarea_outline %>%
      mutate(area = as.numeric(st_area(geometry))) %>%
      arrange(-area) %>%
      slice(1) %>%
      select(geometry)
  }

determine_studyarea_outline_level_europe <-
  function(studyarea, coastline) {
    ### Test
    # coastline <- tar_read(coastline)
    # studyarea <- tar_read(studyarea_subset_plots)
    ###

    studyarea %>%
      summarise() %>%
      st_difference(coastline) %>%
      st_cast("POLYGON") %>%
      select(geometry)
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

    river_network %>%
      st_filter(merged_river_network) %>%
      group_by(feature_id.x) %>%
      arrange(feature_id.y) %>%
      slice(1) %>%
      ungroup() %>%
      rename(feature_id = feature_id.x,
             connected_feature_id = feature_id.y) %>%
      relocate(connected_feature_id, .after = feature_id)

  }


add_feature_index_column <-
  function(sf_object, column_name = "feature_id") {
    sf_object %>%
      mutate(!!column_name := 1:n(), .before = 1)
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
           invalid_values = INVALID_STRAHLER_VALUES) {
    river_network %>%
      filter(!(strahler %in% invalid_values))
    # filter(strahler != -9999)
  }

dissolve_line_features_between_junctions <-
  function(river_networks) {
    ###### Test
    # river_networks <- tar_read(river_networks_without_brackets)
    # studyarea <- tar_read(studyarea_outline)
    #####

    river_networks <- #TODO move this step to target river_network_only_connected or river_networks_clean
      river_networks %>%
        filter(st_geometry_type(.) %in% c("MULTILINESTRING", "LINESTRING"))

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
        st_cast("MULTILINESTRING") %>%
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
        strahler = unique(strahler)
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
      summarise(strahler = unique(strahler)) %>%
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

stream_order_filter <-
  function(river_network, stream_order) {
    river_network %>%
      mutate(
        strahler = as.numeric(strahler),
        stream_order_id = stream_order
      ) %>%
      filter(strahler >= stream_order)
  }

make_grid <-
  function(sf_object) {
    sf_object %>%
      st_make_grid(cellsize = CELLSIZE) %>%
      transform_crs_if_required() %>%
      st_as_sf() %>%
      st_geometry() %>%
      st_sf() %>%
      filter(
        as.vector(
          st_intersects(
            .,
            sf_object,
            sparse = FALSE))) %>%
      mutate(grid_id = row_number())
  }

centroids_to_grid <-
  function(centroids, grid) {
    grid %>%
      st_join(centroids)
  }

# make_thiessen_catchments <-
#   function(grid, centroids) {
#     centroids %>%
#       centroids_to_grid(grid) %>%
#       group_by(nearest_feature) %>%
#       summarise(feature_id = first(nearest_feature)) %>%
#       st_cast("MULTIPOLYGON") %>%
#       st_geometry() %>%
#       st_sf()
#   }

generate_filepaths <- function(stream_order, abbreviation) {
  str_c(
    "output_data/",
    "mohp_germany_",
    abbreviation,
    "_",
    "order",
    stream_order,
    "_",
    CELLSIZE,
    "m_res",
    ".tiff"
  )
}

calculate_lateral_position_grid <- function(stream_distance_centroids, divide_distance_centroids, stream_order, grid, field_name, directory) {
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

filter_intersecting_features <- 
  function(x, y) {
    x %>% 
      filter(st_intersects(., y, sparse = FALSE) %>% apply(1, any))
  }

impute_streamorder <-
  function(sf_lines, studyarea) {
    test <- FALSE
    if (test) {
      sf_lines <- tar_read(river_networks_dissolved_junctions_after)# %>%
        # mutate(strahler = if_else(feature_id %in% c(212, 113, 202, 89), -9999L, strahler))
      studyarea <- tar_read(selected_studyarea)
    }
    
    studyarea <- 
      studyarea %>%
      st_cast("LINESTRING")
    
    sf_lines <-
      sf_lines %>%
        mutate(strahler = replace_na(strahler, INVALID_STRAHLER_VALUES),
               strahler = as.integer(strahler)
               )

    lines_na_streamorder <-
      sf_lines %>%
        filter(strahler %in% as.integer(INVALID_STRAHLER_VALUES))
    
    if(lines_na_streamorder %>% nrow() %>% magrittr::equals(0)) {
      sf_lines
    } else {
    
    lines_valid_streamorder <-
      sf_lines %>%
        anti_join(as_tibble(lines_na_streamorder), by = "feature_id")

    endpoints_one_side <-
      lines_na_streamorder %>%
        st_endpoint() %>%
        st_as_sf()
    
    endpoints_other_side <-
      lines_na_streamorder %>%
        st_startpoint() %>%
        st_as_sf()
    
    endpoints <-
      endpoints_one_side %>%
        bind_rows(endpoints_other_side) %>% 
        distinct()

    # lines_valid_streamorder_filter <- 
    #   lines_valid_streamorder %>% 
    #   filter_intersecting_features(endpoints)

    lines_filter <- 
      sf_lines %>% 
      filter_intersecting_features(endpoints)
    
    lines_canals_endpoints_join <-
      endpoints %>%
        st_join(lines_na_streamorder) %>% 
        group_by(feature_id) %>%
        group_split() %>%
        map_dfr(~st_join(., lines_filter)) %>% 
        filter(feature_id.x != feature_id.y) %>% 
        group_by(feature_id.x) %>% 
        group_split() %>% 
        map(add_side_column)
        
    feature_id_order <-
      lines_canals_endpoints_join %>%
        map_int(~.x %>% slice(1) %>% pull(feature_id.x))

    impute_with <-
      lines_canals_endpoints_join %>%
        map_int(impute_streamorder_by_case, studyarea) %>%
        tibble(feature_id = feature_id_order,
               strahler = .)

    lines_na_streamorder %>%
      select(-strahler) %>%
      left_join(impute_with, by = "feature_id") %>%
      bind_rows(lines_valid_streamorder) %>%
      arrange(feature_id) %>%
      relocate(feature_id, strahler, everything())
    }
  }

add_side_column <-
  function(x) {
    x <-
      x %>%
      mutate(geometry_char = as.character(geometry))

    unique_geometries <-
      x %>%
      pull(geometry_char) %>%
      unique()

    if (length(unique_geometries) == 1) {
      x <- x %>%
        mutate(side = "one", .before = 1)
    } else if (length(unique_geometries) == 2) {
      x <- x %>%
        mutate(side = if_else(
          geometry_char == unique_geometries[1],
          "one",
          "other"
        ), .before = 1)
    }
    x %>%
      select(-geometry_char)
  }

impute_streamorder_by_case <-
  function(x, studyarea) {
    touches_n_other_feature_without_streamorder <-
      x %>%
      filter(strahler.y %in% INVALID_STRAHLER_VALUES) %>%
      nrow()

    touches_no_other_feature_without_streamorder <-
      touches_n_other_feature_without_streamorder %>%
      magrittr::equals(0)

    if (touches_no_other_feature_without_streamorder) {
      if (is_case_a(x, studyarea)) {
        impute_case_a(x)
      } else if (is_case_b(x, studyarea)) {
        impute_case_b(x)
      } else if (is_case_c(x)) {
        impute_case_c(x)
      } else if (is_case_d(x)) {
        impute_case_d(x)
      } else if (is_case_d_sub(x)) {
        impute_case_d_sub(x)
      } else if (is_case_e(x)) {
        impute_case_e(x)
      } else {
        stop("no matching case (inner for)")
      }
      # TODO This is just a dirt solution right now. Needs to be solved for features
      # that are connected to other line segments without strahler
    } else if (touches_n_other_feature_without_streamorder == nrow(x)) {
      impute_case_f(x)
    } else if (touches_n_other_feature_without_streamorder > 0) {
      impute_case_g(x)
    } else {
      stop("no matching case (inner for)")
    }
  }

impute_case_a <-
  function(x) {
    x %>%
      pull(strahler.y) %>%
      max(na.rm = TRUE) %>%
      as.integer()
  }

impute_case_f <-
  function(x) {
    1L
  }

impute_case_g <-
  function(x) {
    x %>%
      pull(strahler.y) %>%
      unique() %>%
      sort() %>%
      magrittr::extract(2) %>%
      as.integer()
  }

impute_case_b <-
  function(x) {
    1L
  }

impute_case_c <-
  function(x) {
    x %>%
      group_by(side) %>%
      summarise(n = n(),
                strahler.y = str_c(strahler.y, collapse = ", "),
                .groups = "drop") %>%
      filter(n == 1) %>%
      pull(strahler.y) %>%
      as.integer()
  }

impute_case_d <-
  function(x) {
    x %>%
      pull(strahler.y) %>%
      unique() %>%
      as.integer()
  }

impute_case_d_sub <-
  function(x) {
    x %>%
      pull(strahler.y) %>%
      unique() %>%
      chuck(1) %>% 
      as.integer()
  }

impute_case_e <-
  function(x) {
    unique_streamorders <-
      x %>%
        pull(strahler.y) %>%
        unique()

    n_streamorders <-
      unique_streamorders %>%
        length()

    if(n_streamorders == 2) {
      unique_streamorders %>%
        max(na.rm = TRUE) %>%
        as.integer()
    } else if(n_streamorders == 3) {
      unique_streamorders %>%
        sort() %>%
        magrittr::extract(2) %>%
        as.integer()
    } else if(n_streamorders == 1) {
      unique_streamorders
    } else {
      stop("More than 3 unique streamorders at junction. Investigate!")
    }
  }


is_case_a <-
  function(x, studyarea) {
    x %>%
      st_intersects(studyarea, sparse = FALSE) %>%
      as.vector() %>%
      any()
  }

is_case_b <-
  function(x, studyarea) {

    condition_a <-
      x %>%
        st_intersects(studyarea, sparse = FALSE) %>%
        as.vector() %>%
        any() %>%
        isFALSE()

    x <-
      x %>%
        filter(!is.na(feature_id.y))

    condition_b <-
      x %>%
        distinct(side) %>%
        nrow() %>%
        magrittr::equals(1)

    condition_a & condition_b
  }

is_case_c <-
  function(x) {

    x <-
      x %>%
        filter(!is.na(feature_id.y))

    precondition <-
      x %>%
        distinct(side) %>%
        nrow() %>%
        magrittr::equals(2)

    if (precondition) {
      n_touching_segments_per_endpoint <-
        x %>%
          group_by(side) %>%
          count() %>%
          pull(n)

      condition_a <-
        n_touching_segments_per_endpoint %>%
          magrittr::equals(1) %>%
          any()

      condition_b <-
        n_touching_segments_per_endpoint %>%
          magrittr::is_weakly_greater_than(2) %>%
          any()

      condition_a & condition_b
    } else {
      return(precondition)
    }
  }

is_case_d <-
  function(x) {

    x <-
      x %>%
        filter(!is.na(feature_id.y))

    condition_a <-
      x %>%
        group_by(side) %>%
        count() %>%
        pull(n) %>%
        all.equal(c(1, 1)) %>%
        isTRUE()

    if (condition_a) {
      x %>%
        group_by(side) %>%
        summarise(n = n(),
                  strahler = unique(strahler.y),
                 .groups = "drop") %>%
        pull(strahler) %>%
        unique() %>% 
        length() %>% 
        magrittr::equals(1)
    } else {
      condition_a
    }
  }

is_case_d_sub <-
  function(x) {
    
    x <-
      x %>%
      filter(!is.na(feature_id.y))
    
    condition_a <-
      x %>%
      group_by(side) %>%
      count() %>%
      pull(n) %>%
      all.equal(c(1, 1)) %>%
      isTRUE()
    
    if (condition_a) {
      x %>%
        group_by(side) %>%
        summarise(n = n(),
                  strahler = unique(strahler.y),
                  .groups = "drop") %>%
        pull(strahler) %>%
        unique() %>% 
        length() %>% 
        magrittr::equals(2)
    } else {
      condition_a
    }
  }

is_case_e <-
  function(x) {

    x <-
      x %>%
        filter(!is.na(feature_id.y))

    condition_a <-
      x %>%
        distinct(side) %>%
        nrow() %>%
        magrittr::equals(2)

    if (condition_a) {
      condition_b <-
        x %>%
          group_by(side) %>%
          count() %>%
          pull(n) %>%
          magrittr::is_weakly_greater_than(2) %>%
          all()

      condition_b
    } else {
      return(condition_a)
    }
  }


