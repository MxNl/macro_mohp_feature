
pre_r_steps <-
  function() {
    char_vector <- c("Manual data download")
  
    names(char_vector) <- paste0("Step ", 1:(length(char_vector)))
    
    char_vector
  }

r_steps <-
  function() {
    char_vector <- 
      c(
      "Data import",
      "Study area definition",
      "Preprocessing in R",
      "Intitiate PostgreSQL steps",
      "Rasterization of study area",
      "Intitiate GRASS GIS steps"
    )
    
    names(char_vector) <- paste0("Step ", 2:(length(char_vector)+1))
    
    char_vector
  }

postgis_steps <-
  function() {
    char_vector <- c(
      "Linemerge",
      "Lines per hydrologic order"
      )
    
    relation_node <- 5
    names(char_vector) <- paste0("Step ", relation_node, ".", 1:(length(char_vector)))
    
    char_vector
  }

grassgis_steps <-
  function() {
    char_vector <- c(
      "Calculaction of EU-MOHP measures",
      "Export raster images to disk")
    
    relation_node <- 7
    names(char_vector) <- paste0("Step ", relation_node, ".", 1:(length(char_vector)))
    
    char_vector
  }

make_workflow_diagram <-
  function(path) {

    path %>% 
      dirname() %>% 
      fs::dir_create()
    
    diagramm <- DiagrammeR::grViz("
    digraph {
  graph [compound = true, nodesep = .5, ranksep = .25,
         color = crimson, outputorder=edgesfirst]

  node [shape = box, style = 'filled', fillcolor = white, fontname = Corbel, 
        fontsize = 10, fontcolor = darkgray,
        shape = rectangle,
        color = darkslategray, fixedwidth = true, width = 2]

  edge [color = grey, arrowhead = none, arrowtail = none]
  

    subgraph cluster0 {
    style='rounded,filled';
    color=Gainsboro;
    fontsize=11;
    node [style=filled,color=white]
    label = 'R'
    '@@2-1' -> '@@2-2' -> '@@2-3' -> '@@2-4' -> '@@2-5' -> '@@2-6'
    }

    subgraph cluster1 {
    style='rounded,filled';
    color='#31648C';
    fontcolor='white';
    fontsize=11;
    node [style=filled,color=white]
    label = 'PostgreSQL + PostGIS'
    '@@3-1' -> '@@3-2'
    }

    subgraph cluster2 {
    style='rounded,filled';
    color='#0B8B36';
    fontcolor='white';
    fontsize=11;
    node [style=filled,color=white, width = 3]
    label = 'GRASS GIS'
    '@@4-1' -> '@@4-2'
    }


  '@@1' -> '@@2-1'        [lhead = cluster0]
  '@@2-4' -> '@@3-1'        [ltail = cluster0, lhead = cluster1]
  '@@2-6' -> '@@4'        [ltail = cluster0, lhead = cluster2]
  '@@3-2' -> '@@4'        [ltail = cluster1, lhead = cluster2]
  
  }
      [1]: paste0(names(pre_r_steps()), '\\n ', pre_r_steps())
      [2]: paste0(names(r_steps()), '\\n ', r_steps())                        
      [3]: paste0(names(postgis_steps()), '\\n ', postgis_steps())                        
      [4]: paste0(names(grassgis_steps()), '\\n ', grassgis_steps())                        
                                  ",
      height = 500
    )

    tmp <- capture.output(rsvg_pdf(charToRaw(export_svg(diagramm)), path))
    return(path)
    # cat("![Standards QA flowchart](stnds.qa.png){#fig:stnds.qa.flow}\n\n")
  }

# make_workflow_diagram("data_descriptor/test.pdf")


make_dataset_map_overview_plot <-
  function(selected_hydrologic_orders = c(2, 5)) {
    
    get_mop_files <-
      function(streamorder, directory) {
        list.files(directory) %>%
          tibble(files = .) %>%
          filter(word(files, 2, sep = "_") == AREA) %>%
          filter(word(files, 6, sep = "_") == str_glue("{CELLSIZE}m.tif")) %>%
          filter(word(files, 5, sep = "_") %in% str_glue("streamorder{streamorder}"))
      }
    
    make_output_data_map_plot <-
      function(feature_name, streamorder, legend_title = "", tag_title, guide_range_source = selected_hydrologic_orders) {
        if (feature_name == FEATURE_NAMES[2]) {
          filepath_prefix_feature_name <- "lp"
          directory <- glue::glue("{OUTPUT_DIRECTORY}/{feature_name}/")
        } else if (feature_name == FEATURE_NAMES[1]) {
          filepath_prefix_feature_name <- "dsd"
          directory <- glue::glue("{OUTPUT_DIRECTORY}/{feature_name}/")
        } else if (feature_name == FEATURE_NAMES[3]) {
          filepath_prefix_feature_name <- "sd"
          directory <- glue::glue("{OUTPUT_DIRECTORY}/{feature_name}/")
        } else {
          stop("Provide valid value for the argument feature_name")
        }
        
        files <-
          streamorder %>%
          get_mop_files(directory)
        
        guide_range <-
          guide_range_source %>%
          get_mop_files(directory) %>%
          pull(files) %>%
          str_c(directory, .) %>%
          map(read_stars) %>%
          map(~ map(.x, range, na.rm = TRUE)) %>%
          unlist() %>%
          range(finite = TRUE)
        
        raster_stars <-
          str_glue("{directory}{files$files}") %>%
          map(read_stars)
        
        raster_stars_mosaic <-
          st_mosaic(raster_stars[[1]])
        
        for (i in 2:length(raster_stars)) {
          raster_stars_mosaic <-
            st_mosaic(raster_stars_mosaic, raster_stars[[i]])
        }
        
        final_plot <- 
          ggplot() +
          geom_stars(data = raster_stars_mosaic, downsample = 3) +
          coord_equal() +
          theme_void() +
          labs(
            fill = legend_title,
            tag = tag_title
          ) +
          theme(
            legend.position = "top",
            text = element_text(family = "Corbel", size = 8)
            # legend.key.width = unit(dev.size()[1] / 30, "inches")
          ) +
          guides(
            fill = guide_colourbar(title.position = "top", title.hjust = 0.5, barheight = 0.5),
            size = guide_legend(title.position = "top", title.hjust = 0.5)
          )
        
        if (feature_name == FEATURE_NAMES[2]) {
          final_plot +
            scale_fill_viridis_c(limits = guide_range, na.value = NA, 
                                 labels = scales::label_number(scale = 1e-2, accuracy = 1))
        } else {
          final_plot +
            scale_fill_viridis_c(limits = guide_range, na.value = NA, 
                                 labels = scales::label_number(scale = 1e-3, accuracy = 1))
        } 
      }
    
    plot_lp_one <- make_output_data_map_plot(
      feature_name = FEATURE_NAMES[2],
      streamorder = selected_hydrologic_orders[1],
      legend_title = "Lateral position [%]",
      tag_title = "A",
      guide_range_source = selected_hydrologic_orders
    )
    plot_lp_two <- make_output_data_map_plot(
      FEATURE_NAMES[2],
      selected_hydrologic_orders[2],
      "Lateral position [%]",
      "D"
    )
    
    plot_lp <-
      (plot_lp_one / plot_lp_two +
         plot_layout(guides = "auto") &
         theme(legend.position = "top")) +
      plot_layout(guides = "collect")
    
    plot_dsd_one <- make_output_data_map_plot(
      FEATURE_NAMES[1],
      selected_hydrologic_orders[1],
      "Divide stream distance [km]",
      "B"
    )
    plot_dsd_two <- make_output_data_map_plot(
      FEATURE_NAMES[1],
      selected_hydrologic_orders[2],
      "Divide stream distance [km]",
      "E"
    )
    
    plot_dsd <-
      (plot_dsd_one / plot_dsd_two +
         plot_layout(guides = "auto") &
         theme(legend.position = "top")) +
      plot_layout(guides = "collect")
    
    plot_sd_one <- make_output_data_map_plot(
      FEATURE_NAMES[3],
      selected_hydrologic_orders[1],
      "Stream distance [km]",
      "C"
    )
    plot_sd_two <- make_output_data_map_plot(
      FEATURE_NAMES[3],
      selected_hydrologic_orders[2],
      "Stream distance [km]",
      "F"
    )
    
    plot_sd <-
      (plot_sd_one / plot_sd_two +
         plot_layout(guides = "auto") &
         theme(legend.position = "top")) +
      plot_layout(guides = "collect")
    
    (plot_lp | plot_dsd | plot_sd)  + plot_annotation(theme = theme(plot.margin = margin()))
  }

make_input_data_table <- 
  function() {
    tribble(
      ~'Data layer', ~'Data source', ~'Layers in .gpkg files', ~'Data type', ~'Geometry type', ~Description,
      "river network",   "EU-Hydro -- River Network Database", "Canals_l, Ditches_l, River_Net_l", "vector", "linestring", "representing stream lines of rivers",
      "surface water bodies",   "EU-Hydro -- River Network Database", "InlandWater", "vector", "polygon", "representing lakes, ponds and wide rivers",
      "river basins/ study area",   "EU-Hydro -- River Network Database", "_eudem2_basins_h1", "vector", "linestring", "required to set the area for which the EU-MOHP measures are calculated for",
      "coastline",   "EU-Hydro -- Coastline", "-", "vector", "linestring", "representing the coastline"
    ) %>% 
      mutate('No' = row_number(), .before = 1)
  }

make_output_data_table <- 
  function() {
    expand_grid(
      '<region name for spatial coverage>' = c("europemainland", "finland-norway-sweden", "turkey", "unitedkingdom", 
                                        "iceland", "unitedkingdom-ireland", "italy1", "italy2", "france", 
                                        "greece"),
      '<abbreviation of the EU-MOHP measure>' = c("lp", "dsd", "sd"),
      '<hydrologic order>' = str_glue("streamorder{tar_read(streamorders)}"),
      '<spatial resolution>' = str_glue("{CELLSIZE}m")
    ) %>% 
      mutate(across(everything(), as.factor)) %>% 
      pivot_longer(cols = everything()) %>% 
      mutate(name = factor(name, levels = c('<region name for spatial coverage>', 
                                            '<abbreviation of the EU-MOHP measure>', 
                                            '<hydrologic order>',
                                            '<spatial resolution>'))) %>% 
      arrange(across(everything())) %>% 
      distinct(across(everything())) %>% 
      left_join(
        tibble(
          value = 
            c("europemainland", "finland-norway-sweden", "turkey", "unitedkingdom", 
              "iceland", "unitedkingdom-ireland", "italy1", "italy2", "france", "greece",
              "lp", "dsd", "sd",
              str_glue("streamorder{tar_read(streamorders)}"),
              str_glue("{CELLSIZE}m")
            ),
          description = c(
            "Raster data covers the contiguous land area of continental Europe, ...", "...the Scandinavian countries Finland, Norway and Sweden", "...Turkey", "...United Kingdom", "...Iceland", "Ireland and Northern Ireland", "...Sicily", "...Sardinia", "...Corsica", "...Creta", "Lateral Position", "Divide stream distance", "Stream distance", rep("Hydrologic order", times = length(str_glue("streamorder{tar_read(streamorders)}"))),
            "Spatial resolution"
          )
        )
      ) %>% 
      rename('Placeholder in output file name' = name) %>% 
      set_names(str_to_sentence(names(.)))
  }


make_studyarea_figure <- 
  function(studyarea) {

    eea_countries <- 
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
      filter(name %in% EEA39COUNTRIES) %>% 
      transform_crs_if_required()
    
    distinct_colours <-
      studyarea %>%
      nrow() %>%
      hues::iwanthue(
        # lmin = 34,
        # lmax = 90,
        # cmin = 0,
        # cmax = 72
      )
    
    administrative_borders <-
      eea_countries %>%
      spatial_filter_europe()

    coloured_areas <- 
      eea_countries %>% 
      st_cast("POLYGON") %>% 
      filter_intersecting_features(studyarea) %>% 
      summarise() %>% 
      st_cast("POLYGON") %>% 
      st_join(studyarea) %>% 
      mutate(studyarea = "covered")
    
    not_covered_areas <-
      administrative_borders %>%
      filter(!st_intersects(., coloured_areas, sparse = FALSE) %>% apply(1, any)) %>% 
      mutate(same_colour = "Not covered")
    
    administrative_borders <-
      administrative_borders %>% 
      filter(!st_intersects(., not_covered_areas, sparse = FALSE) %>% apply(1, any))
    
    tm_shape(coloured_areas) +
      tm_fill(col = "studyarea",
                  title = "Spatial coverage",
                  palette = "#ffcf46") +
      tm_shape(not_covered_areas) +
      tm_fill(col = "same_colour",
              palette = "grey",
              title = "") +
      tm_shape(administrative_borders) +
      tm_borders(col = "white", lwd = 0.5) +
      tm_layout(frame = FALSE, fontfamily = "Corbel")
      # tm_shape(studyarea) +
      # tm_text(text = "region_name")
      # tm_text("name", remove.overlap = TRUE)
    
    # studyarea %>% 
    #   ggplot() +
    #   geom_sf(aes(fill = region_name),
    #           colour = NA) +
    #   scale_fill_manual(values = distinct_colours) +
    #   theme_void() +
    #   theme(text = element_text(family="Bahnschrift")) +
    #   labs(fill = "Region as\nin file names")
  }

aoi_from_coordinates <- 
  function(x, y, dist) {
    tibble(x = x,
           y = y) %>% 
      st_as_sf(coords = c("x", "y")) %>% 
      st_sf(crs = 25832) %>% 
      transform_crs_if_required() %>% 
      st_buffer(dist = dist, endCapStyle = "SQUARE")
  }

make_river_canal_confusion_example_plot <- 
  function(x, y, dist, layer_to_clip) {
    
    clip_polygon <- aoi_from_coordinates(x, y, dist)
    
    clipped_geometries <- 
      layer_to_clip %>% 
      filter_intersecting_features(clip_polygon) %>% 
      st_intersection(clip_polygon)
    
    colours <- 
      clipped_geometries %>% 
      generate_discrete_colour_values(dfdd)
    
    clipped_geometries %>% 
      mutate(dfdd = if_else(dfdd == "BH140", 
                            str_glue("{dfdd} (river)"), 
                            if_else(dfdd == "BH20", 
                                    str_glue("{dfdd} (ditch)"), 
                                    str_glue("{dfdd} (canal)")
                            ))) %>% 
      ggplot() +
      geom_sf(aes(colour = dfdd), size = 1) +
      scale_colour_manual(values = colours) +
      geom_curve(data = data.frame(x = 4628921.04384992, y = 3250201.40311892, xend = 4628817.60981672, yend = 3246581.09972031),
                 colour = "grey",
                 mapping = aes(x = x, y = y, xend = xend, yend = yend),
                 curvature = -0.34, arrow = arrow(30L, unit(0.1, "inches"),
                                                  "last", "closed"),
                 inherit.aes = FALSE) + 
      geom_curve(data = data.frame(x = 4622715.00185787, y = 3249011.87485938, xend = 4623852.77622308, yend = 3248236.09555967),
                 colour = "grey", mapping = aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(30L, unit(0.1, "inches"),
                               "last", "closed"),
                 inherit.aes = FALSE) + 
      geom_text(data = data.frame(x = 4622207.80957843, y = 3249825.99554802, label = "canal-like shape"),
                colour = "grey", mapping = aes(x = x, y = y, label = label),
                family = "Corbel", fontface = 2, inherit.aes = FALSE) + 
      geom_text(data = data.frame(x = 4629024.47788312, y = 3251184.05689855, label = "river-like shape"),
                colour = "grey", mapping = aes(x = x, y = y, label = label),
                family = "Corbel", fontface = 2, vinherit.aes = FALSE) +
      geom_curve(data = data.frame(x = 4619762.48546094, y = 3249529.06105918, xend = 4619090.16424514, yend = 3246736.25558025),
                 colour = "grey", mapping = aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(30L, unit(0.1, "inches"),
                               "last", "closed"),
                 inherit.aes = FALSE) +
      theme_minimal() +
      theme(legend.position = "top",
            text = element_text(family = "Corbel"), 
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
      ) +
      labs(colour = "dfdd")
  }

make_dfdd_stats_bar_plot <- 
  function(river_network) {
    
    # colours <- iwanthue(1)
    
    river_network %>% 
      drop_na(dfdd) %>% 
      group_by(dfdd) %>% 
      count() %>% 
      mutate(
        dfdd = if_else(dfdd == "BH140", str_glue("{dfdd} (=river)"), dfdd),
        dfdd = if_else(dfdd == "BH020", str_glue("{dfdd} (=canal)"), dfdd),
        dfdd = if_else(dfdd == "BH030", str_glue("{dfdd} (=ditch)"), dfdd)
        ) %>% 
      mutate(n = n/1000) %>% 
      ggplot(aes(n, reorder(dfdd, n))) +
      geom_col(fill = "#ffcf46",
               alpha = .7) +
      theme_minimal() +
      theme(text = element_text(family = "Corbel"),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(x = "Number of linestring geometries [x1000]",
           title = "Histogram of the column dfdd")
  }

deprintize <-
  function(f) {
    return(function(...) {
      capture.output(w <- f(...))
      return(w)
    })
  }

make_dir_tree <-
  function() {
    fs::dir_tree(
      ".",
      recurse = 1,
      regex = "qgis|junk|*streamorder*|grass_hack_order_test.Rmd|grass_playground.R|playground.R|_dummy.R|data_descriptor|diagramms|*.bib|*.ldf|*.sty|*.pdf|*.Rmd|*.tex|main_files|*.bst|*.cls|*.log|*.md|_targets_packages.R|test_data|test_files|^test",
      invert = TRUE
    )
  }

dirtree_lineno <- 
  function(pattern) {
    deprintize(make_dir_tree)() %>% 
      tibble(rownames = as.character(.)) %>% 
      select(rownames) %>% 
      add_row(tibble(rownames = "."), .before = 1) %>% 
      mutate(rownumber = row_number(), .before = 1) %>% 
      mutate(rownames = str_remove(rownames, ".*/")) %>% 
      filter(str_detect(rownames, str_glue("^{pattern}$"))) %>% 
      pull(rownumber)
  }
