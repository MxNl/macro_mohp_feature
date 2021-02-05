YML_CONFIG <- yaml::read_yaml('config.yml')

LINES_RAW <- "lines_raw"
LINES_CLEAN <- "lines_clean"
LINES_CONNECTED_ID <- "connected_id"
LINES_BY_STREAMORDER <- "river_network_by_streamorder"
BRACKETS_TO_DROP <- "brackets_to_drop"
GRID_CENTROIDS <- "grid"
GRID_POLYGONS_TABLE <- "grid_polygons"
MOHP_FEATURES_TABLE <- "lateral_position_stream_divide_distance"
NN_GRID_CATCHMENTS_TABLE <- "nn_grid_catchments"
THIESSEN_CATCHMENTS_TABLE <- "thiessen_catchments"
NN_GRID_RIVERS_TABLE <- "nn_grid_rivers"
FEATURE_NAMES <- c("lateral_position", "divide_stream_distance")
CRS_REFERENCE <- 3035
CRS_LEAFLET <- 3857
INVALID_STRAHLER_VALUES <- c(-9999)

STREAM_TYPE_TO_INCLUDE <- c(
  "Canals_l",
  "Ditches_l",
  "River_Net_l"
)
AREA <- purrr::chuck(YML_CONFIG, "area")
CELLSIZE <- purrr::chuck(YML_CONFIG, "cellsize")
FILEPATH_PREFIX_SPATIAL_COVERAGE <- paste0("mohp_", AREA)

message("Area: ", AREA, " Cellsize: ", CELLSIZE)
