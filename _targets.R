library(targets)
library(tarchetypes)
library(future)

options(
  tidyverse.quiet = TRUE,
  future.globals.maxSize = 4E5 * 1024^2
)

tar_option_set(
  packages = c(
    "here",
    # "igraph",
    "DBI",
    "glue",
    "RPostgres",
    # "rmarkdown",
    "raster",
    "janitor",
    "rgdal",
    "lwgeom",
    "assertr",
    "patchwork",
    "fasterize",
    "stars",
    "sf",
    "furrr",
    "tarchetypes",
    "tidyverse"),
  memory = "transient",
  garbage_collection = TRUE
)


source("R/constants.R")

# source("targets/studyarea_targets.R")
source("targets/import_targets.R")
source("targets/preprocessing_targets.R")
source("targets/mohpcalculation_targets.R")
# source("targets/visualization_targets.R")
source("targets/export_targets.R")

source("R/import_functions.R")
# source("R/plot_functions.R")
# source("R/directory_functions.R")
source("R/processing_functions.R")
source("R/database_functions.R")
source("R/postgis_functions.R")
source("R/export_functions.R")


plan(multisession)

logging::basicConfig()
logging::addHandler(
  logging::writeToFile,
  file = here::here(
    'logs',
    stringr::str_replace_all(lubridate::now(), ' |:', '-')),
  level = 'INFO'
)

logging::loginfo(paste("Area: ", AREA, " Cellsize: ", CELLSIZE))

# Define targets
c(
  import_targets,
  preprocessing_targets,
  mohpcalculation_targets,
  export_targets # ,
  #TODO visualization_targets include again
)
  