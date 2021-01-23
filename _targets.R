library(targets)
library(tarchetypes)
library(future)

source("import_targets.R")
source("preprocessing_targets.R")
source("visualization_targets.R")

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
                            "DBI",
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
               garbage_collection = TRUE)



plan(multisession)


# Define targets
targets <- 
  c(
    import_targets,
    preprocessing_targets,
    visualization_targets
  )
  