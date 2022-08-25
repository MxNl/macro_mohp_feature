validation_targets <-
  list(
    tar_target(
      nhdplus_lines,
      read_nhdplus_lines(FILEPATH_NHDPLUS)
    ),
    # tar_target(
    #   nhdplus_lines,
    #   read_nhdplus_lines(FILEPATH_NHDPLUS)
    # ),
    # tar_target(
    #   nhdplus_joined,
    #   join_nhdplus_data(nhdplus_attributes, nhdplus_lines)
    # ),
    # tar_target(
    #   nhdplus_rivers,
    #   nhdplus_joined %>%
    #     filter(flowdir == "With Digitized")
    # ),
    tar_target(
      nhdplus_coastline,
      read_nhdplus_coastline(FILEPATH_NHDPLUS)
    ),
    tar_target(
      nhdplus_coastline_contiguous,
      make_coastline_contiguous(nhdplus_coastline)
    ),
    tar_target(
      nhdplus_hortonmerge,
      linemerge_ala_horton(nhdplus_lines)
    ),
    tar_target(
      catchments_contiguous_us,
      read_studyarea_validation(FILEPATH_NHDPLUS)
    ),
    tar_target(
      contiguous_us_single_polygon,
      union_studyarea_in_db(
        catchments_contiguous_us,
        VALIDATION_STUDYAREA,
        st_crs(catchments_contiguous_us)$epsg
      )
    ),
    tar_target(
      studyarea_validation,
      contiguous_us_single_polygon %>%
        adaptor_studyarea_validation()
    ),
    tar_target(
      watershed_validation,
      studyarea_validation %>%
        make_watershed_boundary(nhdplus_coastline_contiguous)
    ),
    tar_target(
      water_bodies_validation,
      read_water_bodies_validation(FILEPATH_NHDPLUS)
    ),
    # tar_target(
    #   water_bodies_validation_intersected,
    #   filter_and_intersect_waterbodies(
    #     water_bodies_validation,
    #     nhdplus_horton_merge
    #   )
    # ),
    tar_target(
      usmohp_reproduction_data,
      calculate_mohp_metrics_in_grassdb_validation(
        nhdplus_hortonmerge,
        NULL,
        studyarea_validation,
        7,
        nhdplus_coastline_contiguous
      ),
      deployment = "main"
    ),
    tar_target(
      sampling_area,
      studyarea_validation %>% 
        st_buffer(BUFFER_SAMPLING_AREA)
    ),
    tar_target(
      sampling_points,
      sampling_area %>% 
        st_sample(size = SAMPLING_SIZE)
    ),
    tar_target(
      plot_validation_sampling,
      make_validation_sampling_plot()
    )
  )