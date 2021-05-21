# Program Configuration
YML_CONFIG <- yaml::read_yaml('config.yml')

AREA <- purrr::chuck(YML_CONFIG, "area")
CELLSIZE <- purrr::chuck(YML_CONFIG, "cellsize")
FILEPATH_PREFIX_SPATIAL_COVERAGE <- paste0("mohp_", AREA)
DATABASENAME <- purrr::chuck(YML_CONFIG, "database_name")
EXCLUDE_SCANDINAVIAN_BASINS <- purrr::chuck(YML_CONFIG, "exclude_scandinavian_basins")
SIMPLIFY_POLYGONS <- purrr::chuck(YML_CONFIG, "simplify_polygons")

# Directories
FILEPATH_STUDYAREA_GERMANY <- "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/germany_buffer/time_invariant/shape/self_processed/data/buffer_germany_point6.shp"
FILEPATH_STUDYAREA_PIPELINETEST <- "J:/NUTZER/Noelscher.M/Studierende/Daten/study_area_polygons/arbitrary/pipeline_test_studyarea/macro_datapreparation_pipeline_test_studyarea_island.shp"
DIRECTORY_RIVER_NETWORKS <- "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/copernicus/data"
FILEPATH_COASTLINE <- "J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_coastline/europe/time_invariant/shape/EUHYDRO_Coastline_EEA39_v013/Shapefile/EUHYDRO_Coastline_EEA39_v013.shp"
FILEPATH_CONFIG <- "config.yml"
GRASS_DIRECTORY <- "grassdata"
GRASS_STREAMORDER_DIRECTORY <- "db_streamorder"
OUTPUT_DIRECTORY <- "output_data"

# Database Table Names
LINES_CLEAN <- "lines_clean"
LINES_MERGED <- "lines_merged"
LINES_STUDYAREA <- "lines_studyarea"
INLAND_WATERS <- "inland_waters"
INLAND_WATERS_STRAHLER <- "inland_waters_strahler"
LINES_BY_STREAMORDER <- "river_network_by_streamorder"
COASTLINE_BUFFER <- "coastline_buffer"
RIVER_BASINS_SUBSET <- "river_basins_subset"
SELECTED_STUDYAREA_TABLE <- "selected_studyarea"
MOHP_FEATURES_TABLE <- "lateral_position_stream_divide_distance"

#Other Constants
FEATURE_NAMES <- c("divide_stream_distance", "lateral_position", "stream_distance")
CRS_REFERENCE <- 3035
CRS_LEAFLET <- 3857
SCANDINAVIAN_BASINS_TO_EXCLUDE <- c("gota", "vorma", "angerman", "tana", "neva", "kemi", "iceland")
INVALID_STRAHLER_VALUES <- c(-9999L)
STREAM_TYPE_TO_INCLUDE <- c(
  "Canals_l",
  "Ditches_l",
  "River_Net_l"
)
HYP_CLASSES_TO_INCLUDE <- c(
  1, 2, 3
)
DFDD_CANALS <- "BH020"
DFDD_RIVERS <- "BH140"
STREAM_TYPES_TO_USE <- c(DFDD_RIVERS)
GRASS_MAX_MEMORY <- 5E4
SIMPLIFY_KEEP_NODES_PERCENTAGE <- 0.05
MIN_AREA_ISLAND <- 3.6E9

EEA39COUNTRIES <- 
  c(
    "Austria",
    "Belgium",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czech Rep.",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Italy",
    "Latvia",
    "Liechtenstein",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Netherlands",
    "Norway",
    "Poland",
    "Portugal",
    "Romania",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden",
    "Switzerland",
    "Turkey",
    "Albania",
    "Bosnia and Herz.",
    "Kosovo",
    "Montenegro",
    "Macedonia",
    "Serbia",
    "United Kingdom"
  )
