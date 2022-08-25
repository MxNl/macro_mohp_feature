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
    st_cast("LINESTRING")
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
    rename(geometry = Shape)
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

calculate_mohp_metrics_in_grassdb <- 
  function(rivers_tbl, 
           table_name_inland_waters,
           studyarea, 
           streamorder, 
           coastline, 
           depends_on = NULL) {
    
    length(depends_on)
    
    test <- FALSE
    if (test) {
      rivers_tbl <- tar_read(nhdplus_horton_merge)
      table_name_inland_waters <- NULL
      # reference_raster <- tar_read(reference_raster)
      # filepaths_reference_raster <- tar_read(filepaths_reference_raster_write)
      studyarea <- tar_read(studyarea_validation)
      streamorder <- 7
      coastline <- tar_read(nhdplus_coastline_contiguous)
    }
    
    # Initiate GRASS database
    crs_reference <- st_crs(studyarea)$proj4string
    initiate_grass_db_parallel_validation(studyarea, streamorder, crs_reference)
    
    # Get river network from PostGIS databse
    rivers_tbl <- 
      rivers_tbl %>% 
      as
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
crs_targetread_nhdplus_lines <- function(filepath) {
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

calculate_mohp_metrics_in_grassdb_validation <- 
  function(lines, 
           table_name_inland_waters,
           studyarea, 
           streamorder, 
           coastline, 
           depends_on = NULL) {
    
    length(depends_on)
    
    test <- FALSE
    if (test) {
      lines <- tar_read(nhdplus_hortonmerge) %>% st_cast("LINESTRING")
      table_name_inland_waters <- NULL
      # reference_raster <- tar_read(reference_raster)
      # filepaths_reference_raster <- tar_read(filepaths_reference_raster_write)
      studyarea <- tar_read(studyarea_validation)
      streamorder <- 7
      coastline <- tar_read(nhdplus_coastline_contiguous) %>% 
        mutate(type = "coastline") %>% 
        rename(geometry = Shape)
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
