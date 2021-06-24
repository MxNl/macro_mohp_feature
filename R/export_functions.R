write_raster_mohp_features <-
  function(feature_name, region_name, streamorder) {

    if (!fs::dir_exists(OUTPUT_DIRECTORY)) {
      fs::dir_create(OUTPUT_DIRECTORY) 
    }
    
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

    if (!fs::dir_exists(directory)) {
      fs::dir_create(directory)
    }
    
    filepath_prefix_streamorder <- streamorder
    filepath_prefix_spatial_resolution <- CELLSIZE

    filepath <-
      str_c(
        FILEPATH_PREFIX_SPATIAL_COVERAGE,
        "_",
        region_name,
        "_",
        filepath_prefix_feature_name,
        "_",
        "streamorder",
        filepath_prefix_streamorder,
        "_",
        filepath_prefix_spatial_resolution,
        "m",
        ".tif"
      ) %>%
      fs::path(directory, .)
    
    if (fs::file_exists(filepath)) {
      fs::file_delete(filepath)  
    }
    
    execGRASS("r.out.gdal",
              input = feature_name,
              output = filepath,
              type = "Int32")
  }

modify_tex_file <- 
  function(path, depends_on = NULL) {
    
    length(depends_on)
    
    path %>%
      readLines() %>% 
      str_replace_all("data_descriptor/tex/", "") %>% 
      writeLines(path)
  }

copy_bib_file <- 
  function() {
    fs::file_copy("eu-mohp.bib", here::here("data_descriptor", "tex"), overwrite = TRUE)
  }