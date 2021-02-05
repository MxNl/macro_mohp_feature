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

pipeline_for <- function(area) {
  pipelines <- list(
    test = list(
      tar_target(
        filepath_studyarea_pipelinetest,
        FILEPATH_STUDYAREA_PIPELINETEST,
        format = "file"
      ),
      tar_target(
        selected_studyarea,
        read_studyarea(filepath_studyarea_pipelinetest)
      )
    ),
    germany = list(
      tar_target(
        filepath_coastline,
        FILEPATH_COASTLINE
      ),
      tar_target(
        coastline,
        read_coastline(filepath_coastline)
      ),
      tar_target(
        filepath_studyarea_germany,
        FILEPATH_STUDYAREA_GERMANY,
        format = "file"
      ),
      tar_target(
        studyarea_germany,
        read_studyarea(filepath_studyarea_germany)
      ),
      tar_target(
        selected_studyarea,
        determine_studyarea_outline_level_germany(studyarea_germany, coastline)
      )
    ),
    europe = list(
      tar_target(
        filepath_coastline,
        FILEPATH_COASTLINE
      ),
      tar_target(
        coastline,
        read_coastline(filepath_coastline)
      ),
      tar_target(
        river_basins,
        river_networks_files %>%
          future_map_dfr(read_river_basins)
      ),
      tar_target(
        selected_studyarea,
        determine_studyarea_outline_level_europe(
          river_basins,
          coastline
        )
      )
    )
  )
  purrr::chuck(pipelines, area)
}
source("R/constants.R")
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
source("R/export_functions.R")
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
  