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
      ),
      tar_target(
        river_basin_names,
        "rhine"
      ),
      tar_target(
        coastline_unioned_all,
          make_test_coastline(selected_studyarea)
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
        coastline_regrouped,
        coastline_unioned %>% 
          mutate(grouping = rep(1:30, length.out = n())) %>% 
          group_by(grouping) %>% 
          tar_group(),
        iteration = "group"
      ),
      tar_target(
        coastline_filtered,
        coastline_regrouped %>% 
          filter_coastline_studyarea(selected_studyarea),
        pattern = map(coastline_regrouped)
      ),
      tar_target(
        coastline_buffer,
          add_buffer(coastline_filtered),
        pattern = map(coastline_filtered)
      ),
      tar_target(
        coastline_buffer_unioned,
        union_coastline_in_db(
          coastline_buffer,
          COASTLINE_BUFFER
        )
      ),
      tar_target(
        studyarea_as_coastline,
        selected_studyarea %>% 
          studyarea_to_coastline(coastline_buffer_unioned)
      ),
      tar_target(
        coastline_watershed,
        selected_studyarea %>% 
          studyarea_to_watershed(coastline_buffer_unioned, studyarea_as_coastline)
      ),
      tar_target(
        river_basins_files,
        list_river_basin_files(directory_river_networks)
      ),
      tar_target(
        river_basin_names,
        get_unique_basin_names(river_basins_files)
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
        pattern = map(river_basins_grouped),
        iteration = "group"
      ),
      tar_target(
        river_basins_subset,
        subset_river_basins(river_basins_unioned)
      ),
      tar_target(
        river_basins_subset_union_in_db,
        union_studyarea_in_db(
          river_basins_subset,
          RIVER_BASINS_SUBSET
        )#,
        # force = !exists_table(SELECTED_STUDYAREA_TABLE)
      ),
      tar_target(
        river_basins_region_name,
        determine_studyarea_outline_level_europe(
          river_basins_subset_union_in_db
        )
      ),
      tar_target(
        selected_studyarea,
        add_region_name(
          river_basins_region_name
        )
      )
    )
  )
  purrr::chuck(pipelines, area)
}