library(targets)
library(tarchetypes)
library(future)

source("R/import_functions.R")
source("R/plot_functions.R")
source("R/processing_functions.R")
source("R/config.R")

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("rmarkdown",
                            "raster",
                            "rgdal",
                            "lwgeom",
                            "sf",
                            "furrr",
                            "tidyverse"))


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



  # Preprocessing -----------------------------------------------------------

  tar_target(
    river_networks_clip,
    clip_river_networks(
      river_networks,
      studyarea_subset_plots
    )
  ),

  tar_target(
    river_networks_clean,
    clean_river_networks(river_networks_clip)
  ),

  tar_target(
    river_network_by_streamorder,
    stream_order_filter(
      river_networks_clean,
      stream_order = 6
    )
  ),

  tar_target(
    base_grid,
    make_grid(river_network_by_streamorder)
  ),

  tar_target(
    base_grid_centroids,
    make_grid_centroids(base_grid)
  ),

  tar_target(
    thiessen_catchments_centroids,
    make_thiessen_catchments_centroids(
      river_network_by_streamorder,
      base_grid,
      base_grid_centroids
    )
  ),
  
  tar_target(
    thiessen_catchments,
    make_thiessen_catchments(
      base_grid,
      thiessen_catchments_centroids
    )
  ),
  
  tar_target(
    centroids_stream_distance,
    calculate_stream_distance_centroids(
      thiessen_catchments_centroids,
      river_network_by_streamorder
    )
  ),
  
  tar_target(
    centroids_divide_distance,
    calculate_divide_distance_centroids(
      thiessen_catchments_centroids,
      river_network_by_streamorder,
      thiessen_catchments
    )
  ),
  
  tar_target(
    grid_lateral_position,
    calculate_lateral_position_centroids(
      centroids_stream_distance,
      centroids_divide_distance,
      base_grid
    )
  ),
  
  tar_target(
    grid_stream_divide_distance,
    calculate_stream_divide_distance_centroids(
      centroids_stream_distance,
      centroids_divide_distance,
      base_grid
    )
  )
  
  
)

tar_pipeline(targets)


# targets::tar_make_future(workers = future::availableCores(), garbage_collection = TRUE)
