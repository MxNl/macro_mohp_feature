# Program Configuration
YML_CONFIG <- yaml::read_yaml('config.yml')

AREA <- purrr::chuck(YML_CONFIG, "area")
CELLSIZE <- purrr::chuck(YML_CONFIG, "cellsize")
FILEPATH_PREFIX_SPATIAL_COVERAGE <- paste0("mohp_", AREA)
DATABASENAME <- purrr::chuck(YML_CONFIG, "database_name")

# Directories
FILEPATH_STUDYAREA_GERMANY <- "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/germany_buffer/time_invariant/shape/self_processed/data/buffer_germany_point6.shp"
FILEPATH_STUDYAREA_PIPELINETEST <- "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/arbitrary/pipeline_test_studyarea/macro_datapreparation_pipeline_test_studyarea.shp"
DIRECTORY_RIVER_NETWORKS <- "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data"
FILEPATH_COASTLINE <- "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_coastline/europe/time_invariant/shape/EUHYDRO_Coastline_EEA39_v013/Shapefile/EUHYDRO_Coastline_EEA39_v013.shp"
DIRECTORY_NONRIVERS_TO_RECLASSIFY <- "qgis/line_features_to_reclassify"

# Database Table Names
LINES_RAW <- "lines_raw"
LINES_CLEAN <- "lines_clean"
LINES_CONNECTED_ID <- "connected_id"
LINES_BY_STREAMORDER <- "river_network_by_streamorder"
BRACKETS_TO_DROP <- "brackets_to_drop"
SELECTED_STUDYAREA_TABLE <- "selected_studyarea"
GRID_CENTROIDS <- "grid"
GRID_POLYGONS_TABLE <- "grid_polygons"
MOHP_FEATURES_TABLE <- "lateral_position_stream_divide_distance"
NN_GRID_CATCHMENTS_TABLE <- "nn_grid_catchments"
THIESSEN_CATCHMENTS_TABLE <- "thiessen_catchments"
NN_GRID_RIVERS_TABLE <- "nn_grid_rivers"

#Other Constants
FEATURE_NAMES <- c("lateral_position", "divide_stream_distance")
CRS_REFERENCE <- 3035
CRS_LEAFLET <- 3857
INVALID_STRAHLER_VALUES <- c(-9999)
STREAM_TYPE_TO_INCLUDE <- c(
  "Canals_l",
  "Ditches_l",
  "River_Net_l"
)
