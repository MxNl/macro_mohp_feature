make_test_coastline <- 
  function(studyarea) {
    studyarea <- 
      studyarea %>% 
      st_union()
    
    buffer <- 
      studyarea %>% 
      st_centroid() %>% 
      magrittr::subtract(c(10000, -10000)) %>% 
      st_set_crs(st_crs(studyarea)) %>% 
      st_buffer(dist = 30000)
    
    coastline <- 
      buffer %>% 
      st_intersection(st_cast(studyarea, "MULTILINESTRING"), .) %>% 
      st_union() %>%
      st_as_sf() %>% 
      rename(geometry = x) %>% 
      st_cast("MULTILINESTRING") %>% 
      mutate(type = "coastline", .before = geometry)

    studyarea %>% 
      st_difference(buffer) %>% 
      st_buffer(dist = 1) %>% 
      # select(-region_name, -id) %>% 
      st_intersection(st_cast(studyarea, "MULTILINESTRING"), .) %>% 
      st_as_sf() %>% 
      rename(geometry = x) %>% 
      mutate(type = "watershed", .before = geometry) %>% 
      st_cast("MULTILINESTRING") %>% 
      bind_rows(coastline)
  }

filter_coastline_studyarea <-
  function(coastline, studyarea) {
    
    coastline %>% 
      transform_crs_if_required() %>% 
      filter_intersecting_features(studyarea) %>% 
      st_union()
  }

add_buffer <-
  function(coastline) {
    coastline %>% 
      st_as_sf() %>% 
      st_buffer(dist = 3000)
  }

studyarea_to_coastline <-
  function(studyarea, coastline_buffer) {
    studyarea <- 
      studyarea %>% 
      st_cast("LINESTRING")
    
    coastline_buffer %>% 
      st_intersection(studyarea, .) %>% 
      select(geometry) %>% 
      mutate(type = "coastline", .before = geometry)
  }

studyarea_to_watershed <-
  function(studyarea, coastline_buffer, coastline) {
    studyarea <- 
      studyarea %>% 
      st_cast("LINESTRING")
    
    studyarea %>% 
      st_difference(coastline_buffer) %>% 
      st_as_sf() %>% 
      select(geometry) %>% 
      mutate(type = "watershed", .before = geometry) %>% 
      st_cast("MULTILINESTRING") %>% 
      st_as_sf() %>% 
      bind_rows(coastline)
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
        filter_intersecting_features(studyarea) %>%
        st_intersection(studyarea) %>%
        st_cast("MULTILINESTRING") %>%
        add_feature_index_column()
    } else if (AREA == "europe") {
      
      river_networks %>%
        # filter_intersecting_features(studyarea) %>% 
        st_as_sf()
        # filter(river_basin_name %in% river_basin_names) %>% 
        # add_feature_index_column()
    } else {
      stop("Please provide a valid character string for the AREA/area parameter in config.yml")
    }
  }

update_strahler_hypexclusion <-
  function(river_network) {
    river_network <- 
      river_network %>% 
      mutate(feature_id = row_number())
    
    update_table <- 
      river_network %>% 
      filter(!(hyp %in% HYP_CLASSES_TO_INCLUDE)) %>% 
      select(hyp, feature_id) %>% 
      st_join(select(river_network, hyp, feature_id, strahler)) %>% 
      as_tibble() %>% 
      filter(feature_id.x != feature_id.y) %>% 
      filter(hyp.y %in% HYP_CLASSES_TO_INCLUDE) %>% 
      group_by(feature_id.x) %>% 
      filter(n() <= 2) %>% 
      ungroup()
    
    for (i in 1:5) {
      update_table <- 
        update_table %>% 
        group_by(feature_id.x) %>% 
        mutate(new_strahler = if_else(n() > 1, min(strahler), 1)) %>% 
        filter(strahler != new_strahler) %>% 
        ungroup() %>% 
        select(feature_id.y, new_strahler) %>% 
        rename(feature_id = feature_id.y) %>% 
        left_join(update_table, ., by = c("feature_id.y" = "feature_id")) %>% 
        mutate(strahler = if_else(!is.na(new_strahler), new_strahler, strahler)) %>% 
        select(-new_strahler)
    }
    
    update_table %>% 
      select(feature_id.y, strahler) %>% 
      left_join(river_network, ., by = c("feature_id" = "feature_id.y")) %>% 
      mutate(strahler = if_else(!is.na(strahler.y), strahler.y, strahler.x)) %>% 
      select(-strahler.y, -strahler.x, -feature_id)
  }

filter_rivers <-
  function(river_network) {
    ##### Test
    # river_network <- tar_read(river_networks_clip)
    # river_network <- tar_read(river_network_pipeline_test)
    # id_to_reclassify <- tar_read(features_ids_to_reclassify)
    ####
    river_network %>%
      filter(dfdd %in% STREAM_TYPES_TO_USE) %>%
      filter(hyp %in% HYP_CLASSES_TO_INCLUDE)
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

    studyarea %>%
      st_make_valid() %>% 
      as_Spatial() %>% 
      rgeos::gDifference(
        as_Spatial(
          st_make_valid(coastline)
          )
        ) %>% 
      st_as_sf() %>% 
      st_make_valid() %>% 
      st_cast("POLYGON") %>%
      mutate(area = as.numeric(st_area(geometry))) %>%
      arrange(-area) %>%
      slice(1) %>%
      select(geometry) %>% 
      mutate(id = 1L) %>% 
      transform_crs_if_required()
  }

determine_studyarea_outline_level_europe <-
  function(river_basins) {
    ### Test
    # river_basins <- tar_read(river_basins_subset)
    # river_networks <- tar_read(river_networks)
    # river_networks_sub <-
    #   river_networks %>%
    #   slice_sample(n = 1000)
    ###

    river_basins %>%
      st_as_sf() %>%
      mutate(id = 1L, .before = 1) %>% 
      st_cast("POLYGON") %>% 
      mutate(area = as.numeric(st_area(geometry))) %>% 
      arrange(-area) %>% 
      slice(1:10) %>% 
      transform_crs_if_required()
  }

add_region_name <- 
  function(river_basins) {
    eea_countries <- 
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
      filter(name %in% EEA39COUNTRIES) %>% 
      transform_crs_if_required()
    
    river_basins <- 
      river_basins %>% 
      add_feature_index_column()
    
    river_basins %>% 
      st_join(eea_countries) %>% 
      select(feature_id, name_long) %>% 
      st_drop_geometry() %>% 
      as_tibble() %>% 
      group_by(feature_id) %>% 
      summarise(region_name = str_c(name_long, collapse = "-")) %>% 
      mutate(
        region_name = str_replace_all(region_name, " ", ""),
        region_name = str_to_lower(region_name),
        n_char = str_length(region_name),
        region_name = if_else(n_char == max(n_char), "europemainland", region_name)
        ) %>% 
      group_by(region_name) %>% 
      mutate(n = n(),
            region_name = if_else(n > 1, str_c(region_name, row_number()), region_name)) %>% 
      ungroup() %>% 
      select(-n_char) %>% 
      left_join(river_basins, ., by = "feature_id") %>% 
      select(-feature_id)
  }

spatial_filter_europe <- 
  function(river_basins) {
    eea_countries <- 
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
      filter(name %in% EEA39COUNTRIES) %>% 
      transform_crs_if_required()
    
    buffer <- 
      eea_countries %>% 
      filter(name == "Germany") %>% 
      st_geometry() %>% 
      st_centroid() %>% 
      st_buffer(dist = 2.3E6)
    
    river_basins %>% 
      st_cast("POLYGON") %>% 
      filter_intersecting_features(st_transform(buffer, st_crs(river_basins)))
  }

basinwise_union <-
  function(river_basin, river_networks) {
    river_basin_name <- 
      river_basin %>% 
      slice(1) %>% 
      pull(river_basin_name)
    
    river_basin %>%
      filter_intersecting_features(river_networks) %>% 
      st_as_sf() %>%
      rmapshaper::ms_simplify(keep = SIMPLIFY_KEEP_NODES_PERCENTAGE, keep_shapes = TRUE) %>% 
      st_make_valid() %>%
      st_collection_extract(type = "POLYGON") %>% 
      as_Spatial() %>%
      rgeos::gUnaryUnion() %>%
      st_as_sf() %>%
      transform_crs_if_required() %>% 
      mutate(river_basin_name = river_basin_name, .before = 1)
  }

add_feature_index_column <-
  function(sf_object, column_name = "feature_id") {
    sf_object %>%
      mutate(!!column_name := 1:n(), .before = 1)
  }

keep_relevant_columns <-
  function(river_network,
           relevant_columns = c("strahler", "river_basin_name", "longpath", "nextdownid", "object_id")) {

    river_network %>%
      select(all_of(relevant_columns))
    # filter(strahler != -9999)
  }

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

get_unique_basin_names <-
  function(river_basins_files) {
    
    river_basins_files <- tar_read(river_basins_files)
    
    river_basin_name <-
      river_basins_files %>% 
      str_replace(".*(?=GPKG/drainage_network_)", "") %>% 
      str_replace("(?=_public).*", "") %>% 
      str_replace("GPKG/drainage_network_", "")
    
    if(EXCLUDE_SCANDINAVIAN_BASINS) {
      river_basin_name <- 
        river_basin_name %>%
        discard(magrittr::is_in(., SCANDINAVIAN_BASINS_TO_EXCLUDE))
    }
    
    river_basin_name
  }

union_river_basins <-
  function(river_basins) {
    
    if(SIMPLIFY_POLYGONS) {
      river_basins <- 
        river_basins %>%
        rmapshaper::ms_simplify(keep = SIMPLIFY_KEEP_NODES_PERCENTAGE)
    }
    
    river_basins %>%
      st_make_valid() %>%
      st_collection_extract("POLYGON") %>% 
      as_Spatial() %>%
      gUnaryUnion() %>%
      st_as_sf() %>%
      st_cast("POLYGON")
  }

subset_river_basins <-
  function(river_basins) {
    river_basins %>% 
      st_as_sf() %>% 
      sfheaders::sf_remove_holes() %>% 
      st_as_sf() %>% 
      transform_crs_if_required() %>% 
      spatial_filter_europe()
  }

union_coastline <-
  function(coastline) {
    
    if(SIMPLIFY_POLYGONS) {
      coastline <- 
        coastline %>%
        rmapshaper::ms_simplify(keep = SIMPLIFY_KEEP_NODES_PERCENTAGE)
    }
    
    coastline %>%
      st_make_valid() %>%
      st_collection_extract("POLYGON") %>% 
      as_Spatial() %>%
      gUnaryUnion() %>%
      st_as_sf() %>%
      st_cast("POLYGON")
  }

add_levelpathid <- 
  function(river_network) {
    
    river_network_path <- 
      river_network %>% 
      as_tibble() %>% 
      arrange(-longpath) %>% 
      select(object_id, nextdownid)
    
    longestpaths_list <- list()
    i <- 1
    while(nrow(river_network_path) > 0) {
      
      longestpaths_list[[i]] <- 
        river_network_path %>% 
        graph.data.frame(directed = TRUE) %>% 
        subcomponent(1, mode = "out") %>% 
        as.vector() %>% 
        slice(river_network_path, .)
      
      river_network_path <- 
        river_network_path %>% 
        filter(!(object_id %in% longestpaths_list[[i]]$object_id))
      
      i <- i + 1
    }
    
    longestpaths_list %>% 
      imap(~mutate(.x, levelpath_id = .y)) %>% 
      reduce(bind_rows) %>% 
      select(-nextdownid) %>% 
      left_join(river_network, ., by = "object_id") %>% 
      select(-longpath, -nextdownid, -object_id)
  }

merge_rivernetworks_per_streamorder <- 
  function(table_name, major_path_ids, distinct_streamorders_in_riverbasins, depends_on = NULL) {
    
    length(depends_on)
    
    streamorder <- 
      distinct_streamorders_in_riverbasins %>% 
      pull(strahler)

    river_basin_name <- 
      distinct_streamorders_in_riverbasins %>% 
      pull(river_basin_name)
    
    river_network <- 
      st_read(
        connect_to_database(), 
        query = str_glue("SELECT 
                            * 
                          FROM {table_name} 
                            WHERE 
                              strahler >= {streamorder}
                            AND
                              river_basin_name = '{river_basin_name}'")
      )
    
    if(streamorder > 1) {
      river_network <- 
        river_network %>% 
        filter(object_id %in% major_path_ids)
    }

    river_network %>% 
      add_levelpathid() %>% 
      group_by(levelpath_id) %>% 
      summarise() %>% 
      select(-levelpath_id) %>% 
      st_cast("MULTILINESTRING") %>% 
      mutate(streamorder = as.integer(streamorder))
  }

order_by_length_and_add_feature_id <- 
  function(river_network) {
    river_network %>% 
      arrange(-st_length(geometry)) %>% 
      add_feature_index_column()
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
    fasterize(raster = raster::raster(., res = CELLSIZE),
                         field = field_name)
}

filter_intersecting_features <- 
  function(x, y) {
    x %>% 
      filter(st_intersects(., y, sparse = FALSE) %>% apply(1, any))
  }

filter_inland_waters <- 
  function(inland_waters, river_network) {
    inland_waters %>% 
      filter(area >= CELLSIZE^2*2) %>% 
      filter_intersecting_features(river_network) %>% 
      select(-area)
  }

impute_streamorder <- 
  function(river_networks) {
    
    river_networks %>%
      mutate(strahler = replace_na(strahler, 1)) %>% 
      mutate(strahler = if_else(strahler %in% INVALID_STRAHLER_VALUES, 1, strahler))
  }

get_river_networks_from_db <- 
  function(table_name, streamorder, depends_on = NULL) {
    
    length(depends_on)
    
    table_name %>% 
      composite_name(streamorder) %>% 
      get_table_from_postgress() %>% 
      query_result_as_sf() %>% 
      add_feature_index_column() %>% 
      mutate(feature_id = as.integer(feature_id))
  }

make_reference_raster <- 
  function(studyarea, depends_on = NULL) {
    
    length(depends_on)
    
    studyarea <-
      studyarea %>%
      st_buffer(sqrt(2*CELLSIZE)*6)

    base_raster <- 
      studyarea %>% 
      fasterize::raster(resolution = c(CELLSIZE, CELLSIZE))

    # reference_raster <-
    studyarea %>%
      # raster::rasterize(base_raster, field = "id", getCover = TRUE)
      fasterize(base_raster)

    # reference_raster[reference_raster > 0] <- 1
    # reference_raster[reference_raster == 0] <- NA
    # reference_raster
  }

remove_simple_brackets <-
  function(river_network) {
    river_network <-
      river_network %>% 
      add_feature_index_column()
    
    start_end_points <-
      river_network %>%
      st_line_merge() %>%
      rowwise() %>%
      group_map(get_start_and_endpoints) %>%
      reduce(bind_rows)
    
    minor_paths_to_remove <-
      start_end_points %>%
      st_join(river_network) %>%
      filter(feature_id.x != feature_id.y) %>%
      group_by(feature_id.x, feature_id.y) %>%
      filter(n() > 1) %>%
      pull(feature_id.x) %>%
      unique()
    
    river_network %>%
      filter(!feature_id %in% minor_paths_to_remove)
  }

get_start_and_endpoints <-
  function(lines, ...) {
    endpoints_one_side <-
      lines %>%
      st_endpoint() %>%
      st_as_sf()
    
    endpoints_other_side <-
      lines %>%
      st_startpoint() %>%
      st_as_sf()
    
    endpoints <-
      endpoints_one_side %>%
      bind_rows(endpoints_other_side) %>%
      mutate(feature_id = lines %>% pull(feature_id))
  }

add_id_column_and_sync_with_rivers <- 
  function (coastline, river_network) {
    coastline %>% 
      st_as_sf() %>% 
      st_cast("MULTILINESTRING") %>% 
      st_cast("LINESTRING") %>% 
      st_cast("MULTILINESTRING") %>% 
      select(type, geometry) %>% 
      mutate(feature_id = row_number() + max(river_network$feature_id))
  }
