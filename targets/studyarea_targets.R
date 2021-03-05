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