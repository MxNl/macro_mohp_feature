library(targets)
library(tarchetypes)
library(future)

source("R/import_functions.R")
source("R/plot_functions.R")
source("R/directory_functions.R")
source("R/processing_functions.R")
source("R/database_functions.R")
source("R/config.R")

options(tidyverse.quiet = TRUE,
        future.globals.maxSize= 4E5*1024^2)

tar_option_set(packages = c(
                            "igraph",
                            "rmarkdown",
                            "raster",
                            "rgdal",
                            "lwgeom",
                            "patchwork",
                            "fasterize",
                            "stars",
                            "sf",
                            "furrr",
                            "tarchetypes",
                            "tidyverse"),
               memory = "transient",
               garbage_collection = TRUE)



plan(multisession)


# Define targets
targets <- list(
  
  
  # Import ------------------------------------------------------------------
  
  tar_target(
    filepath_studyarea,
    "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/germany_buffer/time_invariant/shape/self_processed/data/buffer_germany_point6.shp",
    format = "file"
  ),
  tar_target(
    studyarea,
    read_studyarea(filepath_studyarea)
  ),
  
  tar_target(
    filepath_studyarea_subset_plots,
    "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/arbitrary/pipeline_test_studyarea/macro_datapreparation_pipeline_test_studyarea.shp",
    format = "file"
  ),
  tar_target(
    studyarea_subset_plots,
    read_studyarea(filepath_studyarea_subset_plots)
  ),
  
  tar_target(
    filepath_river_networks,
    "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data/"
  ),
  tar_target(
    river_networks,
    read_river_networks(filepath_river_networks)
  ),
  
  tar_target(
    filepath_coastline,
    "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_coastline/europe/time_invariant/shape/EUHYDRO_Coastline_EEA39_v013/Shapefile/EUHYDRO_Coastline_EEA39_v013.shp"
  ),
  tar_target(
    coastline,
    read_coastline(filepath_coastline)
  ),
  
  tar_target(
    filepath_linemerge_query,
    "sql/linemerge_query.sql",
    format = "file"
  ),
  tar_target(
    linemerge_query,
    read_file(filepath_linemerge_query)
  ),
  
  
  
  # Preprocessing -----------------------------------------------------------
  
  tar_target(
    studyarea_outline,
    determine_studyarea_outline(studyarea_subset_plots, coastline)
  ),
  
  tar_target(
    river_networks_clip,
    clip_river_networks(
      river_networks,
      studyarea_outline
    )
  ),
  
  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_clip, studyarea_outline)
  ),
  
  # tar_target(
  #   river_networks_split,
  #   split_river_network(river_networks_clean)
  # ),
  
  tar_target(
    river_networks_strahler_merge,
    river_networks_clean %>% 
      merge_same_strahler_segments(linemerge_query)
  ),
  
  # tar_target(
  #   test,
  #   river_networks_strahler_merge %>% ggplot() + geom_sf(),
  #   priority = 1
  # ),
  
  tar_target(
    streamorders,
    river_networks_strahler_merge %>% 
      as_tibble() %>% 
      distinct(strahler) %>% 
      pull(strahler) %>% 
      as.numeric() %>%
      sort()
  ),

  tar_target(
    river_network_by_streamorder,
    streamorders %>% 
      as.vector() %>% 
      as.numeric() %>% 
      future_map(
        ~stream_order_filter(
          river_network = river_networks_strahler_merge,
          stream_order = .x
        )
      )
  ),
  
  tar_target(
    base_grid,
    make_grid(river_network_by_streamorder[[1]])
  ),
  
  tar_target(
    base_grid_centroids,
    make_grid_centroids(base_grid)
  ),
  
  tar_target(
    thiessen_catchments_centroids,
    river_network_by_streamorder %>% 
      future_map(
        ~make_thiessen_catchments_centroids(
          .x,
          base_grid,
          base_grid_centroids
        )
      )
  ),
  
  tar_target(
    thiessen_catchments,
    thiessen_catchments_centroids %>% 
      future_map(
        ~make_thiessen_catchments(
          base_grid,
          .x
        )
      )
  ),
  
  tar_target(
    centroids_stream_distance,
    future_map2(
      thiessen_catchments_centroids,
      river_network_by_streamorder,
      calculate_stream_distance_centroids
    )
  ),
  
  tar_target(
    centroids_divide_distance,
    future_map2(
      thiessen_catchments_centroids,
      thiessen_catchments,
      calculate_divide_distance_centroids
    )
  ),
  
  # tar_target(
  #   files_lateral_position,
  #   generate_filepaths(
  #     streamorders,
  #     "lp"
  #     )
  # ),

  tar_target(
    directory_output_data, 
    create_directory_and_return_path("output_data/"),
    format = "file"
  ),
  tar_target(
    directory_lateral_position, 
    create_directory_and_return_path(directory_output_data,
                     "lateral_position/"),
    format = "file"
  ),
  tar_target(
    directory_stream_divide_distance, 
    create_directory_and_return_path(directory_output_data,
                     "stream_divide_distance/"),
    format = "file"
  ),
  
  tar_target(
    grid_lateral_position,
    future_pmap_chr(
      list(
        centroids_stream_distance,
        centroids_divide_distance,
        streamorders  
      ),
      calculate_lateral_position_grid,
      grid = base_grid,
      field_name = "lateral_position",
      directory = directory_lateral_position
    ),
    format = "file"
  ),
  
  tar_target(
    grid_stream_divide_distance,
    future_pmap_chr(
      list(
        centroids_stream_distance,
        centroids_divide_distance,
        streamorders  
      ),
      calculate_stream_divide_distance_grid,
      grid = base_grid,
      field_name = "distance_stream_divide",
      directory = directory_stream_divide_distance
    ),
    format = "file"
  ),
  

  # Visualization -----------------------------------------------------------

  tar_target(
    test_processed_river_network_plot,
    plot_test_processed_river_network(river_networks_strahler_merge,
                                      studyarea_subset_plots)
  ),

  tar_target(
    test_catchments_plot,
    plot_test_catchments(river_network_by_streamorder, thiessen_catchments, streamorders, studyarea_subset_plots)
  )
  
  
)