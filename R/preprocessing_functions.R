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

    studyarea_outline <-
      studyarea %>%
        st_difference(coastline) %>%
        st_cast("POLYGON")

    studyarea_outline %>%
      mutate(area = as.numeric(st_area(geometry))) %>%
      arrange(-area) %>%
      slice(1) %>%
      select(geometry) %>% 
      mutate(id = 1L)
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
      select(geometry) %>% 
      mutate(id = 1L)
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
  function(river_networks) {
    
    # tar_read(river_networks_clip) %>% 
    river_networks %>%
      # verify(filter(dfdd == DFDD_RIVERS & is.na(strahler)) %>% nrow() %>% magrittr::equals(0)) %>% 
      mutate(strahler = replace_na(strahler, 1)) %>% 
      mutate(strahler = if_else(strahler %in% INVALID_STRAHLER_VALUES, 1, strahler))
  }

make_reference_raster <- 
  function(studyarea, depends_on = NULL) {
    
    length(depends_on)
    
    base_raster <- studyarea %>% 
      fasterize::raster(resolution = c(CELLSIZE, CELLSIZE))
    
    studyarea %>% 
      fasterize::fasterize(base_raster)
  }