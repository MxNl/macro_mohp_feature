
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

make_input_data_table <- 
  function() {
    tribble(
      ~'Layer name', ~'Data source', ~'GeoPackage Layers', ~'Data type', ~'Geometry type', ~Description,
      "river network",   "EU-Hydro -- River Network Database", "Canals_l, Ditches_l, River_Net_l", "vector", "linestring", "representing stream lines of rivers",
      "surface water bodies",   "EU-Hydro -- River Network Database", "InlandWater", "vector", "polygon", "representing lakes, ponds and wide rivers",
      "river basins or study area",   "EU-Hydro -- River Network Database", "_eudem2_basins_h1", "vector", "linestring", "required to set the area for which the EU-MOHP measures are calculated for",
      "coastline",   "EU-Hydro -- Coastline", "-", "vector", "linestring", "representing the coastline"
    ) %>% 
      mutate('Layer number' = row_number(), .before = 1)
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
      st_join(studyarea)
    
    not_covered_areas <-
      administrative_borders %>%
      filter(!st_intersects(., coloured_areas, sparse = FALSE) %>% apply(1, any)) %>% 
      mutate(same_colour = "not covered")
    
    administrative_borders <-
      administrative_borders %>% 
      filter(!st_intersects(., not_covered_areas, sparse = FALSE) %>% apply(1, any))
    
    tm_shape(coloured_areas) +
      tm_fill(col = "region_name",
                  title = "Spatial coverage\n(names as used\nin file names)",
                  palette = distinct_colours) +
      tm_shape(not_covered_areas) +
      tm_fill(col = "same_colour",
              palette = "grey",
              title = "") +
      tm_shape(administrative_borders) +
      tm_borders(col = "white") +
      tm_layout(frame = FALSE, fontfamily = "Corbel")
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
      labs(colour = "DFDD")
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
      labs(x = "Number of geometries [x1000]",
           title = "Histogram of the column DFDD")
  }



