library(targets)
library(tarchetypes)
library(future)

options(tidyverse.quiet = TRUE,
        future.globals.maxSize = 4E5 * 1024^2)

tar_option_set(
  packages = c(
    "here",
    "igraph",
    "DBI",
    "glue",
    "RPostgres",
    "rmarkdown",
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

source("import_targets.R")
source("preprocessing_targets.R")
source("mohpcalculation_targets.R")
source("export_targets.R")
source("visualization_targets.R") #TODO include again

source("R/import_functions.R")
source("R/plot_functions.R")
source("R/directory_functions.R")
source("R/processing_functions.R")
source("R/database_functions.R")
source("R/postgis_functions.R")
source("R/config.R")
source("R/constants.R")
source("R/directories_data.R")

plan(multisession)

# Define targets
c(
  import_targets,
  preprocessing_targets,
  mohpcalculation_targets,
  export_targets # ,
  # visualization_targets #TODO include again
)
  