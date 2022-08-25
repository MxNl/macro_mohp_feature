# Program Configuration
YML_CONFIG <- yaml::read_yaml('config.yml')

AREA <- purrr::chuck(YML_CONFIG, "area")
CELLSIZE <- purrr::chuck(YML_CONFIG, "cellsize")
FILEPATH_PREFIX_SPATIAL_COVERAGE <- paste0("mohp_", AREA)
DATABASENAME <- purrr::chuck(YML_CONFIG, "database_name")
EXCLUDE_SCANDINAVIAN_BASINS <- purrr::chuck(YML_CONFIG, "exclude_scandinavian_basins")
SIMPLIFY_POLYGONS <- purrr::chuck(YML_CONFIG, "simplify_polygons")
DATA_DESCRIPTOR_ONLY <- purrr::chuck(YML_CONFIG, "data_descriptor_only")
PARALLEL <- purrr::chuck(YML_CONFIG, "parallel")

# Directories
FILEPATH_STUDYAREA_PIPELINETEST <- "input_data/studyarea_test/macro_datapreparation_pipeline_test_studyarea_island.shp"
DIRECTORY_RIVER_NETWORKS <- "input_data/data"
FILEPATH_COASTLINE <- "input_data/EUHYDRO_Coastline_EEA39_v013/EUHYDRO_Coastline_EEA39_v013.shp"
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
  1, 2, 3, -9999
)
DFDD_CANALS <- "BH020"
DFDD_RIVERS <- "BH140"
STREAM_TYPES_TO_USE <- c(DFDD_RIVERS)
GRASS_MAX_MEMORY <- 5E4
SIMPLIFY_KEEP_NODES_PERCENTAGE <- 0.05

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

#Validation
FILEPATH_NHDPLUS <- "input_data/validation/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
HYDROLOGICORDER_VALIDATION <- 7
CELLSIZE_VALIDATION <- 500
VALIDATION_STUDYAREA <- "validation_studyarea"
CRS_VALIDATION <- 102003
BUFFER_SAMPLING_AREA <- -300000
SAMPLING_SIZE <- 1000
