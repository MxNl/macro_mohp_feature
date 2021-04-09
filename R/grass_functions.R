initiate_grass_db <- 
  function(studyarea) {
    
    
    initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.8",
              addon_base = "C:/Noelscher.M/Anwendungsdaten/GRASS7/addons",
              # home parameter necessary under UNIX-based systems
              home = tempdir(),
              gisDbase = tempdir(), 
              location = "test", 
              mapset = "PERMANENT", 
              override = TRUE)
    
    execGRASS("g.proj", flags = c("c", "quiet"), 
              proj4 = st_crs(studyarea)$proj4string)
    # b_box = st_bbox(studyarea) 
    # execGRASS("g.region", flags = c("quiet"), 
    #           n = as.character(b_box["ymax"]), s = as.character(b_box["ymin"]), 
    #           e = as.character(b_box["xmax"]), w = as.character(b_box["xmin"]), 
    #           res = as.character(CELLSIZE))
  }

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
      table_name <- LINES_STUDYAREA
      table_name_inland_waters <- INLAND_WATERS_STRAHLER
      # reference_raster <- tar_read(reference_raster)
      # filepaths_reference_raster <- tar_read(filepaths_reference_raster_write)
      studyarea <- tar_read(selected_studyarea)
      streamorder <- 1
      coastline <- tar_read(coastline_unioned_all)
    }

    crs_reference <- st_crs(studyarea)$proj4string
    
    initiate_grass_db_parallel(studyarea, streamorder, crs_reference)
    # initiate_grass_db(studyarea)
    
    lines <- 
      st_read(
        connect_to_database(), 
        query = str_glue("SELECT * FROM {table_name} WHERE strahler >= {streamorder}")
      ) %>% 
      mutate(feature_id = as.integer(feature_id))
    
    coastline <- 
      coastline %>% 
      st_as_sf() %>% 
      st_cast("MULTILINESTRING") %>% 
      st_cast("LINESTRING") %>% 
      st_cast("MULTILINESTRING") %>% 
      rename(geometry = x) %>% 
      select(geometry) %>% 
      mutate(feature_id = row_number() + max(lines$feature_id), strahler = 0L)
    
    lines_to_remove_from_catchments <-
      coastline %>%
      pull(feature_id) %>%
      unique()
    
    use_sf()
    lines %>% 
      bind_rows(coastline) %>% 
      st_transform(crs_reference) %>% 
      writeVECT("river_network", 
                v.in.ogr_flags = c("overwrite"))
    print("river_network")
    
    inland_waters <- 
      st_read(
      connect_to_database(), 
      query = str_glue("SELECT * FROM {INLAND_WATERS_STRAHLER} WHERE strahler >= {streamorder}")
      ) %>% 
      mutate(feature_id = as.integer(feature_id))
    
    if(nrow(inland_waters) != 0) {
      use_sf()
      inland_waters %>% 
        st_transform(crs_reference) %>% 
        writeVECT("inland_waters", 
                  v.in.ogr_flags = c("overwrite"))
      
      execGRASS("v.to.rast", 
                input = "inland_waters", 
                output = "inland_waters_raster", 
                use = "attr",
                attribute_column = "feature_id",
                flags = c("overwrite"),
                memory = GRASS_MAX_MEMORY)
      
      has_inland_waters <- TRUE
    } else {
      has_inland_waters <- FALSE
    }
    
    
    # use_sp()
    # execGRASS("r.import",
    #           input = FILEPATH_REFERENCE_RASTER_OUTPUT,
    #           output = "reference_raster")
    # 
    # print("reference_raster")
    # 
    # execGRASS("g.region", flags = c("quiet"),
    #           raster = "reference_raster"
    # )
    
    studyarea <- 
      studyarea %>% 
      st_cast("POLYGON") %>% 
      st_transform(crs_reference) %>%
      rowwise() %>% 
      group_split()
    
    feature_referenceareas_table <- 
      studyarea %>% 
      generate_feature_referenceareas_table()
    
    feature_layernames <- 
      feature_referenceareas_table %>% 
      group_by(reference_areas) %>% 
      group_split() %>% 
      map(pull, layer_name)
    
    feature_layernames %>% 
      map2(studyarea, grass_calculations, lines_to_remove_from_catchments, has_inland_waters)
    
    use_sf()
    studyarea %>% 
      reduce(bind_rows) %>% 
      writeVECT("mask", 
                v.in.ogr_flags = c("overwrite"))
    
    execGRASS("r.mask",
              vector = "mask",
              flags = c("overwrite"))
    
    feature_layernames_patch <- 
      feature_referenceareas_table %>% 
      group_by(feature) %>% 
      group_split() %>% 
      map(pull, layer_name)
    
    feature_layernames_patch %>% 
      map2(FEATURE_NAMES,
           ~execGRASS("r.patch",
              input = .x,
              output = .y,
              flag = c("overwrite", "z")))
    
    FEATURE_NAMES %>%
      map(write_raster_mohp_features, streamorder)
  }

generate_feature_referenceareas_table <- 
  function(studyarea) {
    feature_rasters <- 
      FEATURE_NAMES %>% 
      tibble(feature = .)
    
    reference_areas_table <- 
      studyarea %>% 
      map(generate_extent_suffix) %>% 
      tibble(reference_areas = .)
    
    crossing(feature_rasters, reference_areas_table) %>% 
      mutate(layer_name = str_c(feature, reference_areas, sep = "_")) %>% 
      mutate(reference_areas = factor(reference_areas, 
                                      levels = pull(reference_areas_table, reference_areas)),
             feature = factor(feature, levels = FEATURE_NAMES))
  }

generate_extent_suffix <- 
  function(studyarea) {
    studyarea %>% 
      extent() %>% 
      as.vector() %>% 
      as.integer() %>% 
      as.character() %>% 
      str_c(collapse = "")
  }

grass_calculations <- 
  function(feature_layernames, studyarea, lines_to_remove_from_catchments, has_inland_waters) {

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
    
    execGRASS("v.to.rast", 
              input = "river_network", 
              output = "river_network_raster", 
              use = "attr",
              attribute_column = "feature_id",
              flags = c("overwrite"),
              memory = GRASS_MAX_MEMORY)
    print("v.to.rast")
    
    execGRASS("r.mapcalc",
              expression = "river_network_raster = round(river_network_raster)",
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = "river_network_raster")
    
    execGRASS("r.thin",
              input = "river_network_raster",
              output = "river_network_value_raster_thin",
              flags = c("overwrite"))
    print("r.thin")

    print(has_inland_waters)
    if(has_inland_waters) {
      execGRASS("r.patch",
                input = c("river_network_value_raster_thin", "inland_waters_raster"),
                output = "river_network_value_raster_thin",
                flag = c("overwrite"))
    }

    execGRASS("r.grow.distance",
              input = "river_network_value_raster_thin",
              distance = "river_network_distance_raster", 
              value = "river_network_value_raster", 
              flags = c("overwrite", "m"))
    print("r.grow.distance")
    
    execGRASS("r.null",
              map = "river_network_value_raster",
              setnull = as.character(lines_to_remove_from_catchments))
    
    execGRASS("r.to.vect",
              input = "river_network_value_raster",
              output = "thiessen_catchments", 
              type = "area", 
              flags = c("overwrite"))
    
    execGRASS("v.to.lines",
              input = "thiessen_catchments",
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
    
    execGRASS("r.grow.distance",
              input = "thiessen_catchments_lines_raster_thin",
              distance = "thiessen_catchments_distance_raster",
              flags = c("overwrite", "m"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{feature_layernames[1]} = (river_network_distance_raster + thiessen_catchments_distance_raster)"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{feature_layernames[2]} = round((river_network_distance_raster/{feature_layernames[1]})*10000)"),
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = feature_layernames[2],
              null = 0)
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{feature_layernames[1]} = round({feature_layernames[1]})"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{feature_layernames[3]} = round(river_network_distance_raster)"),
              flags = c("overwrite"))
    
    execGRASS("r.mask",
              flags = c("r"))
  }
