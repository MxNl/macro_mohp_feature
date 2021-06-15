initiate_grass_db_parallel <- 
  function(studyarea, streamorder, crs_reference) {
    
    directory <- glue::glue("{GRASS_DIRECTORY}/{GRASS_STREAMORDER_DIRECTORY}_{streamorder}")
    if(fs::dir_exists(directory)) {
      fs::file_delete(directory) 
    }
    fs::dir_create(directory)
    
    initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.8",
              addon_base = "C:/Noelscher.M/Anwendungsdaten/GRASS7/addons",
              # home parameter necessary under UNIX-based systems
              home = directory,
              gisDbase = directory, 
              location = str_c("streamorder", streamorder, sep = "_"), 
              mapset = "PERMANENT", 
              override = TRUE)
    
    set.defaultFlagsOption("quiet")
    
    execGRASS("g.proj", flags = c("c", "quiet"), 
              proj4 = crs_reference)
    
    b_box = st_bbox(studyarea)
    execGRASS("g.region", flags = c("quiet"),
              n = as.character(b_box["ymax"]), s = as.character(b_box["ymin"]),
              e = as.character(b_box["xmax"]), w = as.character(b_box["xmin"]),
              res = as.character(CELLSIZE))
  }

calculate_mohp_metrics_in_grassdb <- 
  function(table_name, 
           table_name_inland_waters,
           studyarea, 
           streamorder, 
           coastline, 
           depends_on = NULL) {
    
    length(depends_on)
    
    test <- FALSE
    if (test) {
      table_name <- LINES_MERGED
      table_name_inland_waters <- INLAND_WATERS_STRAHLER
      # reference_raster <- tar_read(reference_raster)
      # filepaths_reference_raster <- tar_read(filepaths_reference_raster_write)
      studyarea <- tar_read(selected_studyarea)
      streamorder <- 3
      coastline <- tar_read(coastline_watershed)
    }
    
    # Initiate GRASS database
    crs_reference <- st_crs(studyarea)$proj4string
    initiate_grass_db_parallel(studyarea, streamorder, crs_reference)
    
    # Get river network from PostGIS databse
    table_name <- 
      table_name %>% 
      composite_name(streamorder)
    
    lines <- 
      st_read(
        connect_to_database(), 
        query = str_glue("SELECT feature_id, geometry FROM {table_name}")
      ) %>% 
      mutate(feature_id = as.integer(feature_id))
    
    # Add ID column to coastline
    coastline <- 
      coastline %>% 
      add_id_column_and_sync_with_rivers(lines)
    
    lines_to_remove_from_catchments <-
      coastline %>%
      pull(feature_id) %>%
      unique()
    
    lines_to_remove_from_rivers <-
      coastline %>%
      filter(type == "watershed") %>% 
      pull(feature_id) %>%
      unique()
    
    use_sf()
    lines %>% 
      arrange(-st_length(geometry)) %>% 
      bind_rows(select(coastline, -type)) %>% 
      st_transform(crs_reference) %>% 
      # mutate(feature_id = as.character(feature_id)) %>%
      writeVECT("river_network", 
                v.in.ogr_flags = c("overwrite"))
    print("river_network")
    
    grass_rasterize_and_clean_rivers()
    
    has_inland_waters <- 
      add_inland_waters_to_rivers_raster(table_name_inland_waters, streamorder, crs_reference)
    
    studyarea <- 
      studyarea %>% 
      st_cast("POLYGON") %>% 
      filter_intersecting_features(lines) %>% 
      st_transform(crs_reference) %>%
      rowwise() %>% 
      group_split()
    
    n_studyareas <- length(studyarea)
    
    studyarea %>% 
      imap(
        calculate_mohp_per_polygon,
        lines_to_remove_from_catchments, 
        lines_to_remove_from_rivers, 
        has_inland_waters,
        streamorder,
        n_studyareas
      )
    
    Sys.time()
  }

calculate_mohp_per_polygon <- 
  function(studyarea, 
           current_studyarea,
           lines_to_remove_from_catchments, 
           lines_to_remove_from_rivers, 
           has_inland_waters,
           streamorder,
           n_studyareas) {
    
    b_box = st_bbox(studyarea)
    execGRASS("g.region", flags = c("quiet"),
              n = as.character(b_box["ymax"]), s = as.character(b_box["ymin"]),
              e = as.character(b_box["xmax"]), w = as.character(b_box["xmin"]),
              res = as.character(CELLSIZE))
    
    studyarea %>%
      grass_calculations(
        lines_to_remove_from_catchments,
        lines_to_remove_from_rivers,
        has_inland_waters)
    
    region_name <- studyarea %>% pull(region_name)
    
    FEATURE_NAMES %>%
      map(write_raster_mohp_features, region_name, streamorder)
    
    print(str_glue("{region_name} ({current_studyarea} of {n_studyareas})"))
  }

grass_calculations <- 
  function(studyarea, 
           lines_to_remove_from_catchments, 
           lines_to_remove_from_rivers, 
           has_inland_waters) {
    
    # feature_layernames <- feature_layernames %>% chuck(1)
    # studyarea <- studyarea %>% chuck(1)
    
    use_sf()
    studyarea %>%
      writeVECT("mask",
                v.in.ogr_flags = c("overwrite"))
    execGRASS("r.mask",
              vector = "mask",
              flags = c("overwrite"))
    print("r.mask")
    
    execGRASS("r.null",
              map = "river_network_raster",
              setnull = as.character(lines_to_remove_from_rivers))
    print("r.null")
    
    execGRASS("r.grow.distance",
              input = "river_network_raster",
              distance = "river_network_distance_raster", 
              value = "river_network_value_raster", 
              flags = c("overwrite", "m"))
    print("r.grow.distance")
    
    
    # Generate thiessen catchments
    execGRASS("r.null",
              map = "river_network_value_raster",
              setnull = as.character(lines_to_remove_from_catchments))
    
    execGRASS("r.to.vect",
              input = "river_network_value_raster",
              output = "thiessen_catchments", 
              type = "area", 
              flags = c("overwrite"))
    
    execGRASS("v.clean",
              input = "thiessen_catchments",
              output = "thiessen_catchments_clean",
              tool = "rmdangle",
              flags = c("overwrite"))
    
    execGRASS("v.to.lines",
              input = "thiessen_catchments_clean",
              output = "thiessen_catchments_outline", 
              flags = c("overwrite"))
    
    execGRASS("v.category", 
              input = "thiessen_catchments_outline", 
              output = "thiessen_catchments_outline_cat", 
              layer = c("2", "1"),
              option = "chlayer",
              flags = c("overwrite"))
    
    execGRASS("v.db.connect", 
              map = "thiessen_catchments_outline_cat", 
              table = "thiessen_catchments_outline_cat", 
              flags = c("o"))
    
    execGRASS("v.to.rast", 
              input = "thiessen_catchments_outline_cat", 
              output = "thiessen_catchments_lines_raster", 
              use = "val",
              flags = c("overwrite"),
              memory = GRASS_MAX_MEMORY)
    
    execGRASS("r.mapcalc",
              expression = "thiessen_catchments_lines_raster = round(thiessen_catchments_lines_raster)",
              flags = c("overwrite"))
    
    execGRASS("r.thin",
              input = "thiessen_catchments_lines_raster",
              output = "thiessen_catchments_lines_raster_thin",
              iterations = 200,
              flags = c("overwrite"))
    
    
    # Calculate distance from watershed/catchment boundary
    max_distance <- 1E9 %>% format(scientific = FALSE)
    execGRASS("r.mapcalc",
              expression = str_glue("friction = if(!isnull(river_network_raster), {max_distance}, 0)"),
              flags = c("overwrite"))
    
    execGRASS("r.walk",
              elevation = "river_network_distance_raster",
              friction = "friction",
              output = "thiessen_catchments_distance_raster",
              start_raster = "thiessen_catchments_lines_raster_thin",
              walk_coeff = "1,0,0,0",
              lambda = 1,
              memory = GRASS_MAX_MEMORY,
              flags = c("overwrite"))
    print("walk")
    execGRASS("r.mapcalc",
              expression = str_glue("thiessen_catchments_distance_raster = if(thiessen_catchments_distance_raster >= {max_distance}, null(), thiessen_catchments_distance_raster)"),
              flags = c("overwrite"))
    
    # execGRASS("r.neighbors",
    #           input = "thiessen_catchments_distance_raster",
    #           selection = "river_network_raster",
    #           output = "thiessen_catchments_distance_raster",
    #           method = "minimum",
    #           flags = c("overwrite"))
    # print("neighbors")
    
    execGRASS("r.grow.distance",
              input = "thiessen_catchments_lines_raster_thin",
              distance = "lake_replacement",
              flags = c("overwrite", "m"))
    
    execGRASS("r.mapcalc",
              expression = 
                str_glue("thiessen_catchments_distance_raster = if(!isnull(river_network_raster), lake_replacement, thiessen_catchments_distance_raster)"),
              flags = c("overwrite"))
    
    # execGRASS("r.surf.idw",
    #           input = "thiessen_catchments_distance_raster",
    #           output = "thiessen_catchments_distance_raster",
    #           flags = c("overwrite"))
    
    print("thiessen_catchments_distance_raster")
    
    # Final MOHP calculation
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[1]} = (river_network_distance_raster + thiessen_catchments_distance_raster)"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[2]} = round((river_network_distance_raster/{FEATURE_NAMES[1]})*10000)"),
              flags = c("overwrite"))
    
    # execGRASS("r.null",
    #           map = FEATURE_NAMES[2],
    #           null = 0)
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[1]} = round({FEATURE_NAMES[1]})"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[3]} = round(river_network_distance_raster)"),
              flags = c("overwrite"))
  }


grass_rasterize_and_clean_rivers <- 
  function() {
    execGRASS("v.to.rast", 
              input = "river_network", 
              output = "river_network_raster",
              type = "line",
              use = "attr",
              attribute_column = "feature_id",
              # label_column = "feature_id",
              flags = c("overwrite", "d"),
              memory = GRASS_MAX_MEMORY)
    print("v.to.rast")
    
    execGRASS("r.mapcalc",
              expression = "river_network_raster = round(river_network_raster)",
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = "river_network_raster")
    
    execGRASS("r.neighbors",
              input = "river_network_raster",
              selection = "river_network_raster",
              output = "river_network_raster",
              method = "minimum",
              flags = c("overwrite", "c"))
    
    # execGRASS("r.thin",
    #           input = "river_network_raster",
    #           output = "river_network_value_raster_thin",
    #           flags = c("overwrite"))
    # print("r.thin")
  }

add_inland_waters_to_rivers_raster <- 
  function(table_name_inland_waters, streamorder, crs_reference) {
    table_name_inland_waters <- 
      table_name_inland_waters %>% 
      composite_name(streamorder)
    
    inland_waters <- 
      st_read(
        connect_to_database(), 
        query = str_glue("SELECT * FROM {table_name_inland_waters}")
      ) %>% 
      # mutate(feature_id = as.character(feature_id)) %>% 
      select(-streamorder)
    
    if(nrow(inland_waters) != 0) {
      use_sf()
      inland_waters %>% 
        st_transform(crs_reference) %>% 
        writeVECT("inland_waters", 
                  v.in.ogr_flags = c("overwrite"))
      
      execGRASS("v.to.rast", 
                input = "inland_waters", 
                output = "inland_waters_raster", 
                type = "area",
                use = "attr",
                attribute_column = "feature_id",
                # label_column = "feature_id",
                flags = c("overwrite"),
                memory = GRASS_MAX_MEMORY)
      
      has_inland_waters <- TRUE
    } else {
      has_inland_waters <- FALSE
    }
    
    if(has_inland_waters) {
      execGRASS("r.patch",
                input = c("river_network_raster", "inland_waters_raster"),
                output = "river_network_raster",
                flag = c("overwrite"))
    }
    
    has_inland_waters
  }
