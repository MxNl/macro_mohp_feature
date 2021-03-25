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
        coastline_grouped,
        read_coastline(filepath_coastline) %>% 
          group_by(basin_distr) %>% 
          tar_group(),
        iteration = "group"
      ),
      tar_target(
        coastline_unioned,
        coastline_grouped %>% 
          union_coastline(),
        pattern = map(coastline_grouped)
      ),
      tar_target(
        coastline_unioned_all,
        coastline_unioned %>% 
          st_union()
      ),
      tar_target(
        river_basins_files,
        list_river_basin_files(directory_river_networks)
      ),
      tar_target(
        river_basin_names,
        get_unique_basin_names(river_basins_files)
          # magrittr::extract(str_detect(., "garonne|loire")),
      ),
      tar_target(
        river_basins,
        read_river_basins(river_basins_files, river_basin_names),
        pattern = map(river_basin_names)
      ),
      tar_target(
        river_basins_grouped,
        river_basins %>% 
          group_by(river_basin_name) %>% 
          tar_group(),
        iteration = "group"
      ),
      tar_target(
        river_basins_unioned,
          union_river_basins(river_basins_grouped),
        pattern = map(river_basins_grouped)
      ),
      tar_target(
        selected_studyarea,
        determine_studyarea_outline_level_europe(
          river_basins_unioned
        )
      )
    )
  )
  purrr::chuck(pipelines, area)
}