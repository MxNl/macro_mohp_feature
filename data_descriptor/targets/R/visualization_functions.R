generate_discrete_colour_values <- 
  function(x, var){
    x %>% 
      pull({{ var }}) %>% 
      unique() %>% 
      length() %>% 
      hues::iwanthue()
  }

get_mop_files <-
  function(streamorder, directory, spatial_coverage) {
    list.files(directory) %>%
      tibble(files = .) %>%
      filter(word(files, 2, sep = "_") == AREA) %>%
      filter(word(files, 6, sep = "_") == str_glue("{CELLSIZE}m.tif")) %>%
      filter(word(files, 5, sep = "_") %in% str_glue("hydrologicorder{streamorder}")) %>%
      filter(str_detect(word(files, 3, sep = "_"), str_c(spatial_coverage, collapse = "|")))
  }

make_output_data_map_plot <-
  function(feature_name, 
           streamorder, 
           legend_title = "", 
           tag_title, 
           guide_range_source = selected_hydrologic_orders, 
           spatial_coverage = spatial_coverage) {
    
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
      get_mop_files(directory, spatial_coverage)
    
    guide_range <-
      guide_range_source %>%
      get_mop_files(directory, spatial_coverage) %>%
      pull(files) %>%
      str_c(directory, .) %>%
      map(read_stars, proxy = FALSE) %>%
      map(~ map(.x, range, na.rm = TRUE)) %>%
      unlist() %>%
      range(finite = TRUE)
    
    raster_stars <-
      str_glue("{directory}{files$files}") %>%
      map(read_stars, proxy = TRUE)
    
    raster_stars_mosaic <-
      st_mosaic(raster_stars[[1]])
    
    if (length(raster_stars) > 1) {
      for (i in 2:length(raster_stars)) {
        raster_stars_mosaic <-
          st_mosaic(raster_stars_mosaic, raster_stars[[i]])
      }
    }
    
    # guide_range <-
    #   raster_stars_mosaic %>%
    #   st_as_stars() %>%
    #   as("Raster") %>%
    #   getValues() %>%
    #   range(na.rm = TRUE)
    
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

trim_background_and_return_time <-
  function(path, depends_on = NULL) {
    length(depends_on)
    
    image_read_pdf(path) %>%
      image_trim() %>%
      image_write(image = ., path = path, format = "pdf")
    
    Sys.time()
  }


make_dataset_map_overview_plot <-
  function(selected_hydrologic_orders = c(3, 4), spatial_coverage = ".") {
    
    plot_lp_one <- make_output_data_map_plot(
      feature_name = FEATURE_NAMES[2],
      streamorder = selected_hydrologic_orders[1],
      legend_title = "Lateral position [%]",
      tag_title = "A", 
      guide_range_source = selected_hydrologic_orders,
      spatial_coverage = spatial_coverage
    )
    plot_lp_two <- make_output_data_map_plot(
      FEATURE_NAMES[2],
      selected_hydrologic_orders[2],
      "Lateral position [%]",
      "D", 
      guide_range_source = selected_hydrologic_orders,
      spatial_coverage = spatial_coverage
    )
    
    plot_lp <-
      (plot_lp_one / plot_lp_two +
         plot_layout(guides = "auto") &
         theme(legend.position = "top")) +
      plot_layout(guides = "collect")
    
    plot_dsd_one <- make_output_data_map_plot(
      FEATURE_NAMES[1],
      selected_hydrologic_orders[1],
      "Divide to stream distance [km]",
      "B", 
      guide_range_source = selected_hydrologic_orders,
      spatial_coverage = spatial_coverage
    )
    plot_dsd_two <- make_output_data_map_plot(
      FEATURE_NAMES[1],
      selected_hydrologic_orders[2],
      "Divide to stream distance [km]",
      "E", 
      guide_range_source = selected_hydrologic_orders,
      spatial_coverage = spatial_coverage
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
      "C", 
      guide_range_source = selected_hydrologic_orders,
      spatial_coverage = spatial_coverage
    )
    plot_sd_two <- make_output_data_map_plot(
      FEATURE_NAMES[3],
      selected_hydrologic_orders[2],
      "Stream distance [km]",
      "F", 
      guide_range_source = selected_hydrologic_orders,
      spatial_coverage = spatial_coverage
    )
    
    plot_sd <-
      (plot_sd_one / plot_sd_two +
         plot_layout(guides = "auto") &
         theme(legend.position = "top")) +
      plot_layout(guides = "collect")
    
    (plot_lp | plot_dsd | plot_sd) + plot_annotation(theme = theme(plot.margin = margin()))
  }

make_input_data_table <- 
  function() {
    tribble(
      ~'Data layer', ~'Data source', ~'Layers in .gpkg files', ~'Data type', ~'Geometry type', ~Description,
      "river network",   "EU-Hydro -- River Network Database", "River_Net_l", "vector", "linestring", "representing stream lines of rivers",
      "surface water bodies",   "EU-Hydro -- River Network Database", "InlandWater", "vector", "polygon", "representing lakes, ponds and wide rivers",
      "river basins/ study area",   "EU-Hydro -- River Network Database", "_eudem2_basins_h1", "vector", "linestring", "required to set the area for which the EU-MOHP measures are calculated for",
      "coastline",   "EU-Hydro -- Coastline", "-", "vector", "linestring", "representing the coastline"
    ) %>% 
      mutate('No' = row_number(), .before = 1)
  }

make_output_data_table <-
  function(streamorders) {
    expand_grid(
      "<region name for spatial coverage>" = c(
        "europemainland", "finland-norway-sweden", "turkey", "unitedkingdom",
        "iceland", "unitedkingdom-ireland", "italy1", "italy2", "france",
        "greece"
      ),
      "<abbreviation of the EU-MOHP measure>" = c("lp", "dsd", "sd"),
      "<hydrologic order>" = str_glue("hydrologicorder{tar_read(streamorders)}"),
      "<spatial resolution>" = str_glue("{CELLSIZE}m")
    ) %>%
      mutate(across(everything(), as.factor)) %>%
      pivot_longer(cols = everything()) %>%
      mutate(name = factor(name, levels = c(
        "<region name for spatial coverage>",
        "<abbreviation of the EU-MOHP measure>",
        "<hydrologic order>",
        "<spatial resolution>"
      ))) %>%
      arrange(across(everything())) %>%
      distinct(across(everything())) %>%
      left_join(
        tibble(
          value =
            c(
              "europemainland", "finland-norway-sweden", "turkey", "unitedkingdom",
              "iceland", "unitedkingdom-ireland", "italy1", "italy2", "france", "greece",
              "lp", "dsd", "sd",
              str_glue("hydrologicorder{streamorders}"),
              str_glue("{CELLSIZE}m")
            ),
          description = c(
            "Raster data covers the contiguous land area of continental Europe, ...", "...the Scandinavian countries Finland, Norway and Sweden", "...Turkey", "...United Kingdom", "...Iceland", "Ireland and Northern Ireland", "...Sicily", "...Sardinia", "...Corsica", "...Creta", "Lateral Position", "Divide to stream distance", "Stream distance", rep("Hydrologic order (increasing order translates to larger catchments and therefore a larger scale)", times = length(str_glue("streamorder{tar_read(streamorders)}"))),
            "Spatial resolution"
          )
        )
      ) %>%
      rename("Placeholder in output file name" = name) %>%
      set_names(str_to_sentence(names(.)))
  }

make_targets_runtime_table <-
  function() {
    branch_seconds <-
      targets::tar_meta() %>%
      select(name, type, bytes, seconds, parent) %>%
      drop_na(parent) %>%
      group_by(parent) %>%
      summarize(sum_seconds = sum(seconds))
    
    targets::tar_meta() %>%
      select(name, type, bytes, seconds, parent) %>%
      arrange(-seconds) %>%
      filter(!type %in% c("function", "object", "branch")) %>%
      left_join(branch_seconds, by = c("name" = "parent")) %>%
      mutate(seconds = if_else(is.na(sum_seconds), seconds, sum_seconds)) %>%
      mutate(
        minutes = seconds / 60,
        hours = minutes / 60,
        days = hours / 24,
        mb = bytes / 1E6
      ) %>%
      mutate(across(c("seconds", "minutes", "hours", "days", "mb"), round, 1)) %>%
      rename("target name" = name) %>%
      select(-parent, -sum_seconds, -type, -bytes) %>%
      set_names(str_to_sentence(names(.))) %>%
      filter(!(`Target name` %in% c("input_data_table",
                                    "directory_tree",
                                    "selected_hydrologic_orders",
                                    "spatial_coverage",
                                    "dataset_map_overview_plot",
                                    "output_data_table",
                                    "targets_runtime_table",
                                    "studyarea_figure",
                                    "river_canal_confusion_plot",
                                    "dfdd_stats_bar_plot",
                                    "data_descriptor",
                                    "readme",
                                    "readme_hydroshare",
                                    "index",
                                    "directory_tree_trimmed",
                                    "dataset_map_overview_europe_plot",
                                    "filepath_directory_tree",
                                    "tex_filepath",
                                    "modified_tex_file",
                                    "bibfile_copied",
                                    "workflow_figure",
                                    "mohp_starsproxy",
                                    "mohp_raster_values",
                                    "eumohp_filepaths",
                                    "stats_ridge_per_measure_plot",
                                    "stats_ridge_plot",
                                    "stats_ridge_per_measure_plot",
                                    "stats_table_data",
                                    "mohp_starsproxy_names",
                                    "quantile_breaks",
                                    "coastline_plot",
                                    "quantile_breaks_hydrologicorder9",
                                    "dataset_map_hydrologicorder9_plot",
                                    "nhdplus_lines",
                                    "nhdplus_coastline",
                                    "nhdplus_coastline_contiguous",
                                    "nhdplus_hortonmerge",
                                    "catchments_contiguous_us",
                                    "contiguous_us_single_polygon",
                                    "studyarea_validation",
                                    "watershed_validation",
                                    "waterbodies_validation",
                                    "water_bodies_validation_intersected",
                                    "water_bodies_validation_intersected",
                                    "usmohp_reproduction_data",
                                    "sampling_area",
                                    "sampling_points",
                                    "plot_validation_sampling",
                                    "lp7_original",
                                    "lp7_reproduced",
                                    "raster_difference_perc",
                                    "raster_difference_plot",
                                    "raster_difference_perc_plot",
                                    "raster_values_original",
                                    "raster_values_reproduced",
                                    "lm_plot",
                                    "nhdplus_attributes",
                                    "nhdplus_horton_merge",
                                    "water_bodies_validation",
                                    "nhdplus_joined",
                                    "nhdplus_rivers",
                                    "lp7reproduced",
                                    "comparison_plot",
                                    "raster_difference",
                                    "waterbodies_plus",
                                    "validation_sampling_plot",
                                    "technical_report"
      ))
      ) %>% 
      janitor::adorn_totals("row")
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
    
    # colours <- 
    #   clipped_geometries %>% 
    #   generate_discrete_colour_values(dfdd)
    
    clipped_geometries %>% 
      mutate(dfdd = if_else(dfdd == "BH140", 
                            str_glue("{dfdd} (river)"), 
                            if_else(dfdd == "BH20", 
                                    str_glue("{dfdd} (ditch)"), 
                                    str_glue("{dfdd} (canal)")
                            ))) %>% 
      ggplot() +
      geom_sf(aes(colour = dfdd), size = 1) +
      scale_colour_manual(values = c("#DCE319", "#287D8E")) +
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
    
    river_network %>%
      drop_na(dfdd) %>% 
      group_by(dfdd) %>% 
      count() %>% 
      mutate(
        dfdd = if_else(dfdd == "BH140", str_glue("{dfdd} (=river)"), dfdd),
        dfdd = if_else(dfdd == "BH020", str_glue("{dfdd} (=canal)"), dfdd),
        dfdd = if_else(dfdd == "BH030", str_glue("{dfdd} (=ditch)"), dfdd)
      ) %>% 
      # mutate(n = n/1000) %>%
      ggplot(aes(n, reorder(dfdd, n))) +
      geom_col(fill = "#DCE319",
               alpha = .7) +
      scale_x_continuous(trans = 'log10',
                         breaks = scales::trans_breaks('log10', function(x) 10^x),
                         labels = scales::trans_format('log10', scales::math_format(10^.x))) +
      theme_minimal() +
      theme(text = element_text(family = "Corbel"),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(x = "Number of linestring geometries")
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
      regex = "qgis|junk|*streamorder*|grass_hack_order_test.Rmd|grass_playground.R|playground.R|_dummy.R|data_descriptor|diagramms|*.bib|*.ldf|*.sty|*.pdf|*.Rmd|*.tex|main_files|*.bst|*.cls|*.log|*.md|_targets_packages.R|test_data|test_files|^test|README_files|index*|README.html|readme_hydroshare.*|readme_hydroshare.html",
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

make_ridges_per_measure <- function(data) {
  plot <- data %>%
    ggplot(aes(
      x = value,
      y = hydrologic_order,
      fill = stat(x)
    )) +
    ggridges::geom_density_ridges_gradient(
      quantile_lines = TRUE, quantiles = 2,
      vline_size = .7, vline_color = "white",
      position = ggridges::position_raincloud(
        adjust_vlines = TRUE,
        ygap = -.15,
        height = .15
      ),
      colour = "white",
      alpha = .1,
      scale = 2,
      # panel_scaling = FALSE,
      # rel_min_height = 0.01,
      show.legend = FALSE
    ) +
    # ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, scale = 0.9, alpha = 0.7,
    #   vline_size = 1, vline_color = "red",
    #   point_size = 0.4, point_alpha = 1,
    #   position = ggridges::position_raincloud(adjust_vlines = TRUE),
    #   show.legend = FALSE) +
    scale_fill_viridis_c() +
    labs(y = "Hydrologic order") +
    # scale_y_reverse() +
    scale_x_continuous(expand = c(0, 0), guide = guide_axis(check.overlap = TRUE)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, .3))) +
    ggridges::theme_ridges() +
    # theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, family = "Corbel"),
      axis.title.x = element_text(size = 10, hjust = 0.5),
      axis.text = element_text(family = "Corbel"),
      axis.title.y = element_text(size = 10, family = "Corbel"),
      text = element_text(family = "Corbel")
    )

  if (data %>% slice_head(n = 1) %>% pull(measure) %>% magrittr::is_in(c("dsd", "sd"))) {
    plot <- plot +
      scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        breaks = 10^(0:4),
        # labels = scales::comma,
        expand = c(0, 0),
        guide = guide_axis(check.overlap = TRUE)
      )
  }
  if (data %>% slice_head(n = 1) %>% pull(measure) %>% magrittr::is_in(c("lp", "sd"))) {
    plot <- plot +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      )
  }
  if (data %>% slice_head(n = 1) %>% pull(measure) %>% magrittr::is_in(c("lp"))) {
    plot <- plot +
      xlab(data %>%
        slice_head(n = 1) %>%
        pull(measure) %>%
        stringr::str_to_upper() %>%
        stringr::str_c(" [-]"))
  } else {
    plot <- plot +
      xlab(data %>%
             slice_head(n = 1) %>%
             pull(measure) %>%
             stringr::str_to_upper() %>%
             stringr::str_c(" [km]"))
  }
  plot
}

make_stats_ridges_plot <- function(data) {
  data %>% 
    group_by(measure) %>% 
    group_split() %>% 
    map(make_ridges_per_measure) %>% 
    patchwork::wrap_plots(nrow = 1) +
    # plot_annotation(title = "Distribution of the calculated measures") &
    theme(plot.title = element_text(hjust = 0.5, family = "Corbel"))
}

stars_to_values <- function(stars_object, index) {
  stars_object %>% 
    lazy_dt() %>% 
    rename("value" = 3) %>% 
    mutate(index = index) %>% 
    filter(!is.na(value)) %>% 
    lazy_dt() %>% 
    mutate(measure = stringr::word(index, end = 1, sep = "_")) %>% 
    mutate(hydrologic_order = stringr::str_extract(index, "hydrologicorder\\d")) %>% 
    mutate(hydrologic_order = stringr::str_remove(hydrologic_order, "hydrologicorder")) %>% 
    select(-index, -x, -y) %>% 
    mutate(value = if_else(measure == "lp", value / 1E4, value)) %>% 
    mutate(value = if_else(measure %in% c("dsd", "sd"), value / 1E3, value)) %>% 
    mutate(hydrologic_order = factor(hydrologic_order, levels = 9:1)) %>% 
    as_tibble()
}

round_off <- function (x, digits=0) 
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}

make_stats_summary <- function(data) {
  data %>%
    lazy_dt() %>%
    mutate(hydrologic_order = factor(hydrologic_order, levels = 1:9)) %>%
    group_by(measure, hydrologic_order) %>%
    summarise(
      min = min(value),
      median = median(value),
      mean = mean(value),
      max = max(value),
      .groups = "drop"
    ) %>%
    as_tibble() %>%
    tidyr::pivot_wider(
      id_cols = "hydrologic_order",
      names_from = "measure",
      values_from = c("min", "median", "mean", "max")
    ) %>%
    mutate(across(where(is.numeric), round_off, 2)) %>%
    relocate(
      hydrologic_order,
      contains("dsd"),
      contains("lp"),
      contains("sd")
    )
}

map_quantiles_breaks <- function(mohp_raster_values, hydrologic_orders) {
  mohp_raster_values %>% 
    filter(hydrologic_order %in% hydrologic_orders) %>% 
    mutate(value = if_else(measure == "lp", value * 1E4, value)) %>% 
    mutate(value = if_else(measure %in% c("dsd", "sd"), value * 1E3, value)) %>% 
    group_by(measure) %>% 
    group_split() %>% 
    map(quantiles_breaks)
}

quantiles_breaks <- function(x) {
  x %>%
    drop_na(value) %>%
    pull(value) %>%
    quantile(probs = seq(0, 1, 1 / QUANTILE_SCALE_NUMBER_BINS)) %>%
    enframe() %>%
    mutate(name = as.numeric(str_remove(name, "%"))) %>%
    deframe()
}

.patchwork_measures <- function(plot_list) {
  patch <- plot_list %>%
    patchwork::wrap_plots(col = 1)

  patch +
    # grid::textGrob(plot_list %>%
    #                        names() %>%
    #                        purrr::chuck(1) %>%
    #                        stringr::word(start = 2, sep = "_") %>%
    #                        stringr::str_to_upper()) +
    patchwork::guide_area() +
    patchwork::plot_layout(
      guides = "collect",
      tag_level = "new",
      heights = c(
        rep_len(
          10,
          length.out = length(plot_list)
        ),
        4
      )
    )
}

patchwork_all <- function(plot_list) {
  n_orders <- plot_list %>% 
    chuck(1) %>% 
    magrittr::extract("patches") %>% 
    magrittr::extract2("patches") %>% 
    magrittr::extract2("layout") %>% 
    magrittr::extract2("heights") %>% 
    length()
  
  if (n_orders >= 3) {
    tag_levels <- c("A", "1")
  } else {
    tag_levels <- "A"
  }
  
  plot_list %>%
    patchwork::wrap_plots(ncol = length(plot_list)) &
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = .5,
          size = 12
        )
      ),
      tag_levels = tag_levels
    ) &
    theme(
      plot.tag = element_text(size = 10, family = "Corbel"),
      plot.tag.position = c(0.1, 0.96))
}

plot_single_order <- function(stars_object, name, downsample = 50, quantiles_breaks, binned_colour_scale) {
  eumohp_measures <- filename_placeholders_values[
    names(filename_placeholders_values) == "abbreviation_measure"
  ]
  eumohp_measure <- name %>% stringr::word(end = 1, sep = "_")

  if (eumohp_measure == eumohp_measures[1]) {
    quantiles_breaks <- quantiles_breaks[[1]] %>% round_half_up(-3)
  } else if (eumohp_measure == eumohp_measures[2]) {
    quantiles_breaks <- quantiles_breaks[[2]] %>% round_half_up(-2)
  } else if (eumohp_measure == eumohp_measures[3]) {
    quantiles_breaks <- quantiles_breaks[[3]] %>% round_half_up(-3)
  }

  if (eumohp_measure == eumohp_measures[2]) {
    labels <- function(x) {
      x / 1E4
    }
    unit_label <- "[ - ]"
  } else if (eumohp_measure %in% eumohp_measures[c(1, 3)]) {
    labels <- function(x) {
      x / 1E3
    }
    unit_label <- "[km]"
  }
  
  p <- ggplot2::ggplot() +
    stars::geom_stars(
      data = stars_object,
      downsample = downsample
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      text = element_text(family = "Corbel"),
      legend.position = "top",
      legend.title = ggplot2::element_text(hjust = .5, size = 9, family = "Corbel")
    ) +
    ggplot2::labs(
      fill = str_glue("{stringr::str_to_upper(eumohp_measure)} {unit_label}")
    )
  
  if (binned_colour_scale) {
    p <- p +
    ggplot2::binned_scale("fill",
      "measures_binned_quantiles",
      ggplot2:::binned_pal(scales::manual_pal(viridis::viridis_pal()(QUANTILE_SCALE_NUMBER_BINS))),
      labels = labels,
      limits = range(quantiles_breaks),
      show.limits = FALSE,
      guide = ggplot2::guide_coloursteps(
        direction = "horizontal",
        barheight = ggplot2::unit(2, units = "mm"),
        barwidth = ggplot2::unit(50, units = "mm"),
        draw.ulim = TRUE,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0.5,
        order = 1
      ),
      breaks = quantiles_breaks %>% as.vector()
    )
  } else if (!binned_colour_scale) {
    p <- p +
      ggplot2::scale_fill_viridis_c(na.value = NA,
                                    labels = labels,
                                    guide = ggplot2::guide_colourbar(
                                      direction = "horizontal",
                                      barheight = ggplot2::unit(2, units = "mm"),
                                      barwidth = ggplot2::unit(43, units = "mm"),
                                      draw.ulim = F,
                                      title.position = "top",
                                      title.hjust = 0.5,
                                      label.hjust = 0.5,
                                      order = 1
                                    ))
  }
}

eumohp_plot <- function(eumohp_starsproxy, quantiles_breaks, hydrologic_orders, binned_colour_scale = TRUE) {
  eumohp_measures <- filename_placeholders_values[
    names(filename_placeholders_values) == "abbreviation_measure"
  ]

  selection_suffix <- hydrologic_orders %>% 
    as.character() %>% 
    str_c("hydrologicorder", .)
  
  single_plots <- eumohp_starsproxy %>%
    tidyselect:::select(dplyr::contains(selection_suffix)) %>%
    purrr::imap(
      plot_single_order, 
      downsample = 1, 
      quantiles_breaks = quantiles_breaks,
      binned_colour_scale
      )

  single_plots %>%
    split(f = str_remove(names(single_plots), "hydrologicorder\\d_")) %>%
    map(.patchwork_measures) %>%
    patchwork_all()
}

make_coastline_plot <- function(x) {
  x %>%
    mutate(type = if_else(type == "watershed", "Administrative border\nover land", "Coastline")) %>% 
    ggplot() + 
    geom_sf(aes(colour = type)) +
    scale_colour_manual(values = c("#DCE319", "#287D8E")) +
    theme_minimal() +
    theme(
      legend.position = c(0.8, 0.55),
      text = element_text(family = "Corbel"),
      legend.title = element_blank(),
      legend.spacing.y = unit(0.25, 'cm') 
    ) +
    guides(color=guide_legend(override.aes=list(fill=NA),
                              byrow = TRUE))
}
