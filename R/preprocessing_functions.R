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
    river_basins <- tar_read(river_basins_subset)
    # river_networks <- tar_read(river_networks)
    # river_networks_sub <-
    #   river_networks %>%
    #   slice_sample(n = 1000)
    ###

    river_basins %>%
      st_make_valid() %>%
      st_collection_extract("POLYGON") %>% 
      st_as_sf() %>% 
      as_Spatial() %>%
      gUnaryUnion() %>%
      st_as_sf() %>% 
      sfheaders::sf_remove_holes() %>%
      st_as_sf() %>%
      mutate(id = 1L, .before = 1) %>% 
      transform_crs_if_required()
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
           relevant_columns = c("strahler", "river_basin_name")) {

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
      mutate(area = as.numeric(st_area(geometry))*1E6) %>% 
      filter(area >= MIN_AREA_ISLAND) %>% 
      select(-area)
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

filter_inland_waters <- 
  function(inland_waters, river_network) {
    inland_waters %>% 
      filter(area >= CELLSIZE^2*2) %>% 
      filter_intersecting_features(river_network) %>% 
      select(-area)
  }

impute_streamorder <- 
  function(river_networks) {
    
    # tar_read(river_networks_clip) %>% 
    river_networks %>%
      # verify(filter(dfdd == DFDD_RIVERS & is.na(strahler)) %>% nrow() %>% magrittr::equals(0)) %>% 
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
      fasterize::fasterize(base_raster)

    # reference_raster[reference_raster > 0] <- 1
    # reference_raster[reference_raster == 0] <- NA
    # reference_raster
  }