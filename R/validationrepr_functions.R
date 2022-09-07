read_nhd_lines <- function(filepath) {
  print(CRS_VALIDATION)
  
  result <- read_sf(
    filepath,
    query = str_glue(
      "SELECT
              OBJECTID,
              FLOWDIR,
              FTYPE,
              FCODE,
              LevelPathI, 
              StreamLeve, 
              StreamCalc, 
              StreamOrde 
             FROM NHDFlowline_Network
              WHERE StreamCalc >= {HYDROLOGICORDER_VALIDATION}"
    )
  ) %>%
    # st_drop_geometry() %>%
    clean_names() %>%
    st_zm() %>% 
    st_transform(CRS_VALIDATION)
  
  print(result %>% st_crs())
  
  return(result)
}

# read_nhdplus_lines <- function(filepath) {
#   read_sf(
#     filepath,
#     layer = "BurnLineEvent",
#     query = "SELECT
#               OBJECTID
#            FROM BurnLineEvent"
#   ) %>%
#     clean_names() %>%
#     st_zm()
# }

# join_nhdplus_data <- function(attributes, lines) {
#   lines %>%
#     inner_join(
#       st_drop_geometry(attributes),
#       by = "objectid"
#     )
# }

read_nhd_coastline <- function(filepath) {
  read_sf(
    filepath,
    query = "SELECT
              OBJECTID,
              FLOWDIR
           FROM NHDFlowline_Network
              WHERE FTYPE = 'Coastline'"
  ) %>%
    clean_names() %>%
    st_zm() %>%
    summarise() %>% 
    st_transform(CRS_VALIDATION)
}

linemerge_ala_horton <- function(x) {
  x %>%
    group_by(level_path_i) %>%
    summarise() %>%
    select(-level_path_i) %>%
    mutate(
      feature_id = row_number(),
      streamorder = HYDROLOGICORDER_VALIDATION
    ) %>% 
    rename(geometry = Shape) %>% 
    st_cast("MULTILINESTRING") %>% 
    st_transform(CRS_VALIDATION)
}

read_nhd_studyarea <- function(filepath) {
  
  read_sf(
    filepath,
    layer = "HUC12"
  ) %>%
    clean_names() %>%
    filter(!(states %in% c("CAN", "MEX", "GU", "HI", "PR", "TT", "CM", "AS", "VI"))) %>%
    filter(!(hu_12_type %in% c("W", "I"))) %>%
    # filter(!(hu_12_ds %in% c("OCEAN"))) %>%
    rename(geometry = Shape) %>% 
    st_make_valid() %>% 
    st_transform(CRS_VALIDATION)
}

make_coastline_clean <- function(x) {
  
  ne_contiguous_us <- rnaturalearth::ne_countries(
    scale = "medium", 
    country = "United States of America", 
    returnclass = "sf"
  ) %>% 
    st_cast("POLYGON") %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    filter(area == max(area)) %>% 
    st_transform(st_crs(x))
  
  x %>% 
    st_cast("LINESTRING") %>% 
    filter_intersecting_features(ne_contiguous_us) %>% 
    mutate(type = "coastline", .before = 1) %>% 
    rename(geometry = Shape) %>% 
    st_transform(CRS_VALIDATION)
}

make_contiguous_us_single_polygon <- function(x) {
  x %>%
    st_cast("POINT") %>%
    st_convex_hull()
}

read_waterbodies_validation <- function(filepath) {
  read_sf(
    filepath,
    layer = "NHDWaterbody"
  ) %>%
    clean_names() %>% 
    st_transform(CRS_VALIDATION)
}

filter_intersecting_waterbodies <- function(waterbodies, rivers) {
  # sf::sf_use_s2(FALSE)
  # crs_origin <- st_crs(waterbodies)$epsg
  
  waterbodies %>%
    filter(areasqkm > 1) %>%
    st_make_valid() %>%
    # st_transform(CRS_VALIDATION) %>% 
    filter(
      st_intersects(
        .,
        summarise(rivers) %>%
          # rename(geometry = Shape) %>%
          st_make_valid(),
          # st_transform(CRS_VALIDATION),
        sparse = FALSE
      ) %>% apply(1, any)
    )
    # st_transform(crs_origin)
}

make_watershed_boundary <- function(studyarea, coastline) {
  studyarea %>% 
    st_cast("LINESTRING") %>% 
    # st_transform(st_crs(tar_read(nhdplus_coastline_contiguous))) %>% 
    st_difference(summarise(coastline))
}

union_nhd_studyarea_in_db <- function(river_basins, table_name_destination) {
  
  river_basins %>%
    st_make_valid() %>%
    write_to_table(table_name_destination)
  
  table_name_destination_union <- str_c(table_name_destination, "_union")
  
  connection <- connect_to_database()
  
  query <-
    glue::glue(
      "-- Change the storage type
      ALTER TABLE {table_name_destination}
      ALTER COLUMN geometry
      SET STORAGE EXTERNAL;"
    )
  
  DBI::dbExecute(connection, query)
  
  query <-
    glue::glue(
      "-- Force the column to rewrite
      UPDATE {table_name_destination}
      SET geometry = ST_SetSRID(geometry, {CRS_VALIDATION});"
    )
  
  DBI::dbExecute(connection, query)
  
  query <-
    glue::glue(
      "CREATE TABLE {table_name_destination_union} AS (
        WITH studyarea_unioned AS (
	        SELECT
		  ST_MakePolygon(ST_ExteriorRing((ST_Dump(ST_Union(geometry))).geom)) AS geometry
	      FROM {table_name_destination}
	    ) 
	    SELECT 
		    *
	    FROM studyarea_unioned
      )"
    )
  
  create_table(query, table_name_destination_union)
  
  river_basins <- read_sf(connection, table_name_destination_union)
  
  DBI::dbDisconnect(connection)
  
  river_basins
}

initiate_grass_db_parallel_validation <- 
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
              res = as.character(CELLSIZE_VALIDATION))
  }

read_nhdplus_lines <- function(filepath) {
  read_sf(
    filepath,
    query = str_glue(
      "SELECT
              OBJECTID,
              FLOWDIR,
              FTYPE,
              FCODE,
              LevelPathI, 
              StreamLeve, 
              StreamCalc, 
              StreamOrde 
             FROM NHDFlowline_Network
              WHERE StreamCalc >= {HYDROLOGICORDER_VALIDATION}"
    )
  ) %>%
    # st_drop_geometry() %>%
    clean_names() %>%
    st_zm()
}

# read_nhdplus_lines <- function(filepath) {
#   read_sf(
#     filepath,
#     layer = "BurnLineEvent",
#     query = "SELECT
#               OBJECTID
#            FROM BurnLineEvent"
#   ) %>%
#     clean_names() %>%
#     st_zm()
# }

# join_nhdplus_data <- function(attributes, lines) {
#   lines %>%
#     inner_join(
#       st_drop_geometry(attributes),
#       by = "objectid"
#     )
# }

read_nhdplus_coastline <- function(filepath) {
  read_sf(
    filepath,
    query = "SELECT
              OBJECTID,
              FLOWDIR
           FROM NHDFlowline_Network
              WHERE FTYPE = 'Coastline'"
  ) %>%
    clean_names() %>%
    st_zm() %>%
    summarise()
}

linemerge_a_la_horton <- function(x) {
  x %>%
    group_by(level_path_i) %>%
    summarise() %>%
    select(-level_path_i) %>%
    mutate(
      feature_id = row_number(),
      streamorder = HYDROLOGICORDER_VALIDATION
    )
}

read_studyarea_validation <- function(filepath) {
  read_sf(
    filepath,
    layer = "HUC12"
  ) %>%
    clean_names() %>%
    filter(!(states %in% c("CAN", "MEX", "GU", "HI", "PR", "TT", "CM", "AS", "VI"))) %>%
    filter(!(hu_12_type %in% c("W", "I"))) %>%
    rename(geometry = Shape) %>% 
    st_make_valid()
}

make_coastline_contiguous_us <- function(x) {
  
  ne_contiguous_us <- rnaturalearth::ne_countries(
    scale = "medium", 
    country = "United States of America", 
    returnclass = "sf"
  ) %>% 
    st_cast("POLYGON") %>% 
    mutate(area = as.numeric(st_area(geometry))) %>% 
    filter(area == max(area)) %>% 
    st_transform(st_crs(x))
  
  x %>% 
    st_cast("LINESTRING") %>% 
    filter_intersecting_features(ne_contiguous_us)
}

make_contiguous_us_single_polygon <- function(x) {
  x %>%
    st_cast("POINT") %>%
    st_convex_hull()
}

read_water_bodies_validation <- function(filepath) {
  read_sf(
    filepath,
    layer = "NHDWaterbody"
  ) %>%
    clean_names()
}

filter_and_intersect_waterbodies <- function(waterbodies, rivers) {
  # sf::sf_use_s2(FALSE)
  
  waterbodies %>%
    filter(areasqkm > 1) %>%
    st_make_valid() %>%
    filter(
      st_intersects(
        .,
        summarise(rivers) %>%
          rename(geometry = Shape) %>%
          st_make_valid(),
        sparse = FALSE
      )[1, ]
    )
}

make_watershed_boundary <- function(studyarea, coastline) {
  studyarea %>% 
    st_cast("LINESTRING") %>% 
    # st_transform(st_crs(tar_read(nhdplus_coastline_contiguous))) %>% 
    st_difference(summarise(coastline))
}

adaptor_studyarea_validation <- function(x) {
  x %>%
    st_cast("POLYGON") %>%
    filter(st_area(geometry) == max(st_area(geometry))) %>%
    st_transform(CRS_VALIDATION) %>%
    mutate(region_name = "us_validation")
}

add_inland_waters_to_rivers_raster_validation <- 
  function(inland_waters, streamorder) {
    if(!is.null(inland_waters)) {
      use_sf()
      inland_waters %>% 
        st_transform(CRS_VALIDATION) %>% 
        writeVECT("inland_waters", 
                  v.in.ogr_flags = c("overwrite", "o"))
      
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
                output = "river_network_raster_inland_waters",
                flag = c("overwrite"))
    }
    
    has_inland_waters
  }

grass_calculations_validation <- 
  function(studyarea, 
           lines_to_remove_from_catchments, 
           lines_to_remove_from_rivers, 
           has_inland_waters) {
    
    # feature_layernames <- feature_layernames %>% chuck(1)
    # studyarea <- studyarea %>% chuck(1)
    
    use_sf()
    studyarea %>%
      writeVECT("mask",
                v.in.ogr_flags = c("overwrite", "o"))
    execGRASS("r.mask",
              vector = "mask",
              flags = c("overwrite"))
    print("r.mask")
    
    execGRASS("r.null",
              map = "river_network_raster",
              setnull = as.character(lines_to_remove_from_rivers))
    print("r.null")
    
    
    if(has_inland_waters){
      execGRASS("r.null",
                map = "river_network_raster_inland_waters",
                setnull = as.character(lines_to_remove_from_rivers))
      print("r.null")
      
      execGRASS("r.grow.distance",
                input = "river_network_raster_inland_waters",
                distance = "river_network_distance_raster", 
                value = "river_network_value_raster", 
                flags = c("overwrite", "m")) 
    } else {
      execGRASS("r.grow.distance",
                input = "river_network_raster",
                distance = "river_network_distance_raster", 
                value = "river_network_value_raster", 
                flags = c("overwrite", "m")) 
    }
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
    
    execGRASS("r.neighbors",
              input = "thiessen_catchments_distance_raster",
              selection = "river_network_raster",
              output = "thiessen_catchments_distance_raster",
              method = "minimum",
              flags = c("overwrite"))
    print("neighbors")
    
    execGRASS("r.grow.distance",
              input = "thiessen_catchments_lines_raster_thin",
              distance = "lake_replacement",
              flags = c("overwrite", "m"))
    
    print("r.grow.distance2")
    execGRASS("r.patch",
              input = c("thiessen_catchments_distance_raster", "lake_replacement"),
              output = "thiessen_catchments_distance_raster",
              flag = c("overwrite"))
    
    
    print("thiessen_catchments_distance_raster")
    
    # Final MOHP calculation
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[1]} = (river_network_distance_raster + thiessen_catchments_distance_raster)"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[2]} = round((river_network_distance_raster/{FEATURE_NAMES[1]})*10000)"),
              flags = c("overwrite"))
    
    execGRASS("r.null",
              map = FEATURE_NAMES[2],
              null = 0)
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[1]} = round({FEATURE_NAMES[1]})"),
              flags = c("overwrite"))
    
    execGRASS("r.mapcalc",
              expression = glue::glue("{FEATURE_NAMES[3]} = round(river_network_distance_raster)"),
              flags = c("overwrite"))
  }

calculate_mohp_per_polygon_validation <- 
  function(studyarea, 
           current_studyarea,
           lines_to_remove_from_catchments, 
           lines_to_remove_from_rivers, 
           has_inland_waters,
           streamorder,
           n_studyareas) {
    
    test <- FALSE
    if (test) {
      studyarea <- studyarea %>% chuck(1)
    }
    
    b_box = st_bbox(studyarea)
    execGRASS("g.region", flags = c("quiet"),
              n = as.character(b_box["ymax"]), s = as.character(b_box["ymin"]),
              e = as.character(b_box["xmax"]), w = as.character(b_box["xmin"]),
              res = as.character(CELLSIZE_VALIDATION))
    
    studyarea %>%
      grass_calculations_validation(
        lines_to_remove_from_catchments,
        lines_to_remove_from_rivers,
        has_inland_waters)
    
    region_name <- studyarea %>% pull(region_name)
    
    FEATURE_NAMES %>%
      map(write_raster_mohp_features, region_name, streamorder)
    
    print(str_glue("{region_name} ({current_studyarea} of {n_studyareas})"))
  }

calculate_mohp_metrics_in_grassdb_validation <- 
  function(lines, 
           inland_waters,
           studyarea, 
           streamorder, 
           coastline, 
           depends_on = NULL) {
    
    length(depends_on)
    
    test <- FALSE
    if (test) {
      lines <- tar_read(nhdplus_hortonmerge)
      inland_waters <- NULL
      # reference_raster <- tar_read(reference_raster)
      # filepaths_reference_raster <- tar_read(filepaths_reference_raster_write)
      studyarea <- tar_read(studyarea_validation)
      streamorder <- 7
      inland_waters <- tar_read(water_bodies_validation_intersected)
      coastline <- tar_read(nhdplus_coastline_contiguous)
    }
    # Initiate GRASS database
    crs_reference <- st_crs(studyarea)$proj4string
    initiate_grass_db_parallel_validation(studyarea, streamorder, crs_reference)
    
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
      st_transform(CRS_VALIDATION) %>% 
      # mutate(feature_id = as.character(feature_id)) %>%
      writeVECT("river_network", 
                v.in.ogr_flags = c("overwrite", "o"))
    print("river_network")
    
    grass_rasterize_and_clean_rivers()
    
    has_inland_waters <- 
      add_inland_waters_to_rivers_raster_validation(inland_waters, streamorder)
    
    studyarea <- 
      studyarea %>% 
      st_cast("POLYGON") %>% 
      filter_intersecting_features(lines) %>% 
      st_transform(CRS_VALIDATION) %>%
      rowwise() %>% 
      group_split()
    
    n_studyareas <- length(studyarea)
    
    studyarea %>% 
      imap(
        calculate_mohp_per_polygon_validation,
        lines_to_remove_from_catchments, 
        lines_to_remove_from_rivers, 
        has_inland_waters,
        streamorder,
        n_studyareas
      )
    
    Sys.time()
  }

make_validation_sampling_plot <- function(studyarea_validation, sampling_area, sampling_points) {
    studyarea_validation %>%
    ggplot() +
    geom_sf(colour = NA, fill = "grey90") +
    geom_sf(data = sampling_area, colour = NA, fill = "grey60") +
    geom_sf(data = sampling_points %>% slice_sample(prop = 0.5), colour = "#ffcf46", size = 0.5, shape = 16) +
    geom_curve(
      data = data.frame(x = -1214426.15813222, y = 2989370.97235883, xend = -1260843.29606433, yend = 2701574.11938406),
      mapping = aes(x = x, y = y, xend = xend, yend = yend),
      curvature = 0L, arrow = arrow(
        30L, unit(.07L, "inches"),
        "both", "closed"
      ),
      size = 0.8,
      colour = "darkgrey",
      inherit.aes = FALSE
    ) +
      geom_curve(data = data.frame(x = -1177292.44778652, y = 2859398.20004764, xend = -536735.944323317, yend = 3072924.89741602),
                 mapping = aes(x = x, y = y, xend = xend, yend = yend),
                 angle = 123L, curvature = 0.385, arrow = arrow(30L, unit(0L, "inches"),
                                                                "last", "closed"),
                 colour = "darkgrey",
                 inherit.aes = FALSE) +
      geom_text(data = data.frame(x = -202541.323113007, y = 3165929.9185647, label = "Negative Buffer (~480 km)"),
                mapping = aes(x = x, y = y, label = label),
                colour = "darkgrey",
                family = "Corbel",
                inherit.aes = FALSE) +
    theme_void() +
    theme(text = element_text(family = "Corbel"))
}

make_raster_difference <- function(x, y, as_percentage = FALSE) {
  x <- st_as_stars(x)

  if (as_percentage) {
    (x - y) / x * 100
  } else {
    (x - y) / 1E4
  }
}

make_raster_difference_plot <- function(x) {
  ggplot() +
    geom_stars(data = x, downsample = 50) +
    scico::scale_fill_scico(palette = "broc", na.value = NA) +
    coord_sf() +
    theme_void() +
    theme(
      legend.position = "top",
      text = element_text(family = "Corbel", size = 10)
    ) +
    labs(fill = "Difference between original and reproduced LP7") +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        barheight = 0.5,
        barwidth = 20
      )
    )
}

make_raster_difference_perc_plot <- function(x) {
  ggplot() +
    geom_stars(data = x, downsample = 100) +
    scico::scale_fill_scico(
      palette = "broc",
      na.value = NA,
      limits = c(-100, 100),
      oob = scales::oob_squish
    ) +
    coord_sf() +
    theme_void() +
    theme(
      legend.position = "top",
      text = element_text(family = "Corbel", size = 10)
    ) +
    labs(fill = "Absolute difference between Original LP7 and Reproduced LP7") +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5,
        barheight = 0.5,
        barwidth = 20
      )
    )
}

read_lp7_reproduced <- function(filepath, target_raster, depends_on = NULL) {
  length(depends_on)
  
  read_stars(filepath, proxy = TRUE) %>%
    st_warp(target_raster)
}

waterbodies_add_riverid <- function(x, y) {
  x %>% 
    rename(geometry = Shape) %>% 
    select(geometry) %>% 
    st_cast("MULTIPOLYGON") %>% 
    mutate(waterbody_id = row_number()) %>% 
    # slice_sample(n = 3) %>% 
    st_join(y) %>% 
    group_by(waterbody_id) %>% 
    arrange(feature_id) %>% 
    slice_head(n = 1) %>% 
    ungroup()
}

extract_raster_values <- function(x, y, column_name_extracted) {
  y <- y %>% 
    st_transform(crs = st_crs(x))
  
  x %>% 
    st_extract(at = y) %>% 
    st_drop_geometry() %>%  
    set_names(column_name_extracted) %>% 
    as_tibble()
}

make_lm_plot <- function(x, y) {
  combined <- x %>% 
    bind_cols(y)
  
  r2 <- combined %>% 
    lm(formula = .$original ~ .$reproduced) %>% 
    rsq::rsq() %>% 
    round_half_up(digits = 3)
  
  combined %>% 
    ggplot() +
    aes(original, reproduced) +
    geom_pointdensity(
      alpha = .3,
      show.legend = FALSE
      ) +
    geom_abline(
      slope = 1, 
      intercept = 0, 
      colour = "white", 
      size = .7, 
      linetype = "dashed", 
      alpha = .5
    ) +
    annotate(
      "label", 
      x = 1200, 
      y = 9500, 
      label = paste("R2 =", r2), 
      colour = "grey", 
      family = "Corbel",
      size = 5,
      label.size = NA
    ) +
    scale_color_viridis_c(
      limits = c(0, 5), 
      oob = scales::oob_squish
    ) +
    scale_x_continuous(label = scales::label_number(scale = 1E-4)) +
    scale_y_continuous(label = scales::label_number(scale = 1E-4)) +
    coord_equal() +
    theme_minimal() +
    theme(
      line = element_line(lineend='round'),
      text = element_text(family = "Corbel"),
      axis.title.x = element_text(vjust = -4),
      axis.title.y = element_text(vjust = 4),
      plot.margin = margin(0, 0, 0, 20)
    ) +
    labs(
      x = "Original LP7",
      y = "Reproduced LP7"
    )
}

make_comparison_plot <- function(x, y, quantiles_breaks, hydrologic_orders, binned_colour_scale = TRUE) {
    eumohp_measures <- filename_placeholders_values[
      names(filename_placeholders_values) == "abbreviation_measure"
    ]
    
    selection_suffix <- hydrologic_orders %>% 
      as.character() %>% 
      str_c("hydrologicorder", .)
    
    single_plots <- list(
      x,
      y
    ) %>% 
      set_names(c("lp_hydrologicorder7_90m", "lp_hydrologicorder7_90m")) %>% 
      map(st_downsample, n = 10) %>%
      tidyselect:::select(dplyr::contains(selection_suffix)) %>%
      purrr::imap(
        plot_single_order_validation, 
        downsample = 1, 
        quantiles_breaks = quantiles_breaks,
        binned_colour_scale
      )
    
    single_plots %>%
      split(f = str_remove(names(single_plots), "hydrologicorder\\d_")) %>%
      map(.patchwork_measures_validation) %>%
      patchwork_all_validation()
}

.patchwork_measures_validation <- function(plot_list) {
  patch <- plot_list %>%
    patchwork::wrap_plots(nrow = 1)
  
  patch +
    patchwork::guide_area() +
    patchwork::plot_layout(
      design = "
      AB
      CC
      ",
      ncol = 1, 
      nrow = 2,
      guides = "collect",
      # tag_level = "new",
      heights = c(10, 1)
    )
}

plot_single_order_validation <- function(stars_object, name, downsample = 50, quantiles_breaks, binned_colour_scale) {
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
                              barwidth = ggplot2::unit(100, units = "mm"),
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
                                      barwidth = ggplot2::unit(100, units = "mm"),
                                      draw.ulim = F,
                                      title.position = "top",
                                      title.hjust = 0.5,
                                      label.hjust = 0.5,
                                      order = 1
                                    ))
  }
}

patchwork_all_validation <- function(plot_list) {
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
      plot.tag = element_text(size = 10, family = "Corbel")
      )
}
