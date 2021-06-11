library(targets)
library(tarchetypes)
library(future)
library(renv)
library(extrafont)

options(
  tidyverse.quiet = TRUE,
  future.globals.maxSize = 1E5 * 1024^2
)

tar_option_set(
  packages = c(
    "tmap",
    "igraph",
    "here",
    "fs",
    "DBI",
    "glue",
    "rmapshaper",
    "rnaturalearth",
    "rgeos",
    "rgrass7",
    "RPostgres",
    "sfheaders",
    "rmarkdown",
    "raster",
    "janitor",
    "rgdal",
    "rgeos",
    "lwgeom",
    "assertr",
    "patchwork",
    "hues",
    "stars",
    "sf",
    "furrr",
    "tarchetypes",
    "tidyverse"),
  memory = "transient",
  garbage_collection = TRUE
)


source("R/constants.R")

source("targets/studyarea_targets.R")
source("targets/import_targets.R")
source("targets/preprocessing_targets.R")
source("targets/mohpcalculation_targets.R")
source("data_descriptor/targets/targets/visualizations_data_descriptor_targets.R")
source("data_descriptor/targets/targets/data_descriptor_targets.R")

source("R/import_functions.R")
source("R/preprocessing_functions.R")
source("R/database_functions.R")
source("R/postgis_functions.R")
source("R/grass_functions.R")
source("R/export_functions.R")

source("data_descriptor/targets/R/visualization_functions.R")

if(PARALLEL) plan(multisession)

# Define targets
c(
  import_targets,
  preprocessing_targets,
  if(!DATA_DESCRIPTOR_ONLY) mohpcalculation_targets,
  if(DATA_DESCRIPTOR_ONLY) data_descriptor_targets,
  if(DATA_DESCRIPTOR_ONLY) visualizations_data_descriptor_targets
)
  