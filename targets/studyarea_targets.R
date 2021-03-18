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
        read_studyarea(filepath_studyarea_pipelinetest) %>% 
        mutate(id = 1L)
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
        river_basins_files,
        list_river_basin_files(directory_river_networks)
      ),
      tar_target(
        river_basins,
        river_basins_files %>%
          parallel_read_river_basins_land()
      ),
      tar_target(
        selected_studyarea,
        determine_studyarea_outline_level_europe(
          river_basins,
          river_networks
        )
      )
    )
  )
  purrr::chuck(pipelines, area)
}