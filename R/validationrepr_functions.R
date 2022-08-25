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
    st_transform(CRS_REFERENCE)
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

make_coastline_contiguous <- function(x) {
  
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
    st_transform(CRS_REFERENCE)
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
  sf::sf_use_s2(FALSE)
  crs_origin <- st_crs(waterbodies)$epsg
  
  waterbodies %>%
    filter(areasqkm > 1) %>%
    st_make_valid() %>%
    st_transform(CRS_REFERENCE) %>% 
    filter(
      st_intersects(
        .,
        summarise(rivers) %>%
          rename(geometry = Shape) %>%
          st_make_valid() %>% 
          st_transform(CRS_REFERENCE),
        sparse = FALSE
      )[1, ]
    ) %>% 
    st_transform(crs_origin)
}

make_watershed_boundary <- function(studyarea, coastline) {
  studyarea %>% 
    st_cast("LINESTRING") %>% 
    # st_transform(st_crs(tar_read(nhdplus_coastline_contiguous))) %>% 
    st_difference(summarise(coastline))
}

union_studyarea_in_db <- function(river_basins, table_name_destination, crs_target) {
  
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
      SET geometry = ST_SetSRID(geometry, {crs_target});"
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
    st_transform(CRS_REFERENCE) %>%
    mutate(region_name = "us_validation")
}

union_studyarea_in_db <- function(river_basins, table_name_destination, crs_target) {
  
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
      SET geometry = ST_SetSRID(geometry, {crs_target});"
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

add_inland_waters_to_rivers_raster_validation <- 
  function(inland_waters, streamorder) {
    if(!is.null(inland_waters)) {
      use_sf()
      inland_waters %>% 
        st_transform(CRS_REFERENCE) %>% 
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
      lines <- tar_read(nhdplus_hortonmerge) %>% st_cast("MULTILINESTRING")
      inland_waters <- NULL
      # reference_raster <- tar_read(reference_raster)
      # filepaths_reference_raster <- tar_read(filepaths_reference_raster_write)
      studyarea <- tar_read(studyarea_validation)
      streamorder <- 7
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
      st_transform(CRS_REFERENCE) %>% 
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
      st_transform(CRS_REFERENCE) %>%
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
