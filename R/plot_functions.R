plot_test_processed_river_network <- 
  function(river_network, test_studyarea) {

    river_network <- 
      river_network %>%
      st_intersection(test_studyarea) %>% 
      mutate(id = as.character(1:n())) %>% 
      mutate(strahler = as.character(strahler))
    
    segment_colours <- 
      river_network %>%
      nrow() %>% 
      hues::iwanthue(lmin = 40,
                     cmax = 70)
    plot_a <- 
      river_network %>% 
      ggplot() +
      geom_sf(aes(colour = strahler),
              size = 1) +
      theme(legend.position = "none") +
      labs(title = "Colour represents streamorder after Strahler")
    
    plot_b <- 
      river_network %>% 
      ggplot() +
      geom_sf(aes(colour = id),
              size = 1) +
      scale_colour_manual(values = segment_colours) +
      theme(legend.position = "none") +
      labs(title = "Colour represents line features")
    
    return(plot_a + plot_b)
  }

plot_test_catchments <-
  function(river_network, catchments, streamorders, test_studyarea) {
    #### Test
    # catchments <- tar_read(thiessen_catchments)
    # river_network <- tar_read(river_network_by_streamorder)
    # streamorders <- tar_read(streamorders)
    # test_studyarea <- tar_read(studyarea_subset_plots)
    ####

    list(
      river_network,
      catchments,
      streamorders
    ) %>%
      pmap(
        .l = .,
        plot_test_single_catchment,
        test_studyarea
      ) %>% 
      cowplot::plot_grid(plotlist = .,
                         ncol = 3,
                         align = "hv")
  }

plot_test_single_catchment <- 
  function(river_network, catchment, streamorder, test_studyarea) {
    #### Test
    # catchment <- tar_read(thiessen_catchments)[[2]]
    # river_network <- tar_read(river_network_by_streamorder)[[2]]
    # streamorder <- tar_read(streamorders)[[2]]
    # test_studyarea <- tar_read(studyarea_subset_plots)
    ####
    
    river_network <- 
      river_network %>% 
      st_intersection(test_studyarea)
    
    catchment <- 
      catchment %>% 
      st_intersection(test_studyarea) %>% 
      st_cast("POLYGON")
    
    ggplot() +
      geom_sf(data = catchment,
              fill = "grey",
              colour = "white") +
      geom_sf(data = river_network,
              aes(colour = strahler)) +
      labs(title = str_c("Order = ", streamorder)) +
      theme_void() +
      theme(legend.position = "none") +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE))
  }


generate_discrete_colour_values <- 
  function(x, var){
    x %>% 
      pull({{ var }}) %>% 
      unique() %>% 
      length() %>% 
      hues::iwanthue()
  }


plot_lines_coloured_by_categorical_attribute <-
  function(x, var) {
    
    plot <-
      x %>%
      ggplot() +
      geom_sf(aes(colour = {{ var }}),
              size = 1
      ) +
      scale_colour_manual(values = generate_discrete_colour_values(x, {{ var }})) +
      theme_void() +
      theme(
        legend.position = "none",
        legend.direction = "horizontal"
      )
    
    return(plot)
  }


plot_before_vs_after <- 
  function(x, y){
    
    # x <- tar_read(sf_lines)
    # y <- tar_read(line_merge_by_streamorder)
    
    plot_lines_by_feature_id_before <- 
      x %>% 
      plot_lines_coloured_by_categorical_attribute(feature_id)
    
    plot_lines_by_feature_id_after <- 
      y %>% 
      plot_lines_coloured_by_categorical_attribute(feature_id)
    
    plot <- plot_lines_by_feature_id_before | plot_lines_by_feature_id_after
    
    return(plot)
  }

plot_area_around_feature_id <- 
  function(river_network, feature_id_buffer, buffer_distance = 1E3) {
    
    area <- 
      river_network %>% 
      filter(feature_id == feature_id_buffer) %>% 
      st_buffer(dist = buffer_distance)
    
    river_network_in_area <- 
      river_network %>% 
      filter_intersecting_features(area) %>%
      # st_intersection(area) %>% 
      st_cast("LINESTRING")
    
    river_network_in_area %>% 
        mutate(strahler = as.character(strahler)) %>% 
        plot_lines_coloured_by_categorical_attribute(strahler) +
        geom_sf_label(aes(label = feature_id))
  }

# stars::read_stars("output_data/mohp_germany_lp_order1_500m_res.tiff") %>% 
#   plot()
# raster::raster("output_data/mohp_germany_lp_order1_500m_res.tiff") %>% 
#   raster::as.data.frame(xy = TRUE) %>%
#   ggplot() +
#   geom_raster(aes(x, y, fill = mohp_germany_lp_order1_500m_res))
