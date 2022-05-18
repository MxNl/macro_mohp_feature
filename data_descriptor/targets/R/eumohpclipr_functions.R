filename_placeholders_values <- c(region_name_spatcov = "europemainland", region_name_spatcov = "finland-norway-sweden", 
     region_name_spatcov = "france", region_name_spatcov = "greece", 
     region_name_spatcov = "iceland", region_name_spatcov = "italy1", 
     region_name_spatcov = "italy2", region_name_spatcov = "turkey", 
     region_name_spatcov = "unitedkingdom", region_name_spatcov = "unitedkingdom-ireland", 
     abbreviation_measure = "dsd", abbreviation_measure = "lp", abbreviation_measure = "sd", 
     hydrologic_order = "streamorder1", hydrologic_order = "streamorder2", 
     hydrologic_order = "streamorder3", hydrologic_order = "streamorder4", 
     hydrologic_order = "streamorder5", hydrologic_order = "streamorder6", 
     hydrologic_order = "streamorder7", hydrologic_order = "streamorder8", 
     hydrologic_order = "streamorder9", spatial_resolution = "30m"
)

filename_placeholders <- c(
  "region_name_spatcov",
  "abbreviation_measure",
  "hydrologic_order",
  "spatial_resolution"
)

eumohp_versions <- c("v013.1.0", "v013.1.1")

eea39_countries <-
  c(
    "Austria",
    "Belgium",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czech Rep.",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Iceland",
    "Ireland",
    "Italy",
    "Latvia",
    "Liechtenstein",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Netherlands",
    "Norway",
    "Poland",
    "Portugal",
    "Romania",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden",
    "Switzerland",
    "Turkey",
    "Albania",
    "Bosnia and Herz.",
    "Kosovo",
    "Montenegro",
    "Macedonia",
    "Serbia",
    "United Kingdom"
  )

.is_valid_custom_sf_polygon <- function(custom_sf_polygon) {
  if (!is.null(custom_sf_polygon)) {
    c(
      any(class(custom_sf_polygon) == "sf"),
      nrow(custom_sf_polygon) == 1,
      sf::st_is_simple(custom_sf_polygon),
      sf::st_is(
        sf::st_geometry(custom_sf_polygon),
        c("MULTIPOLYGON", "POLYGON")
      )
    )
  }
}
.is_valid_mode_selection <- function(countries,
                                     custom_sf_polygon,
                                     region_name_spatcov) {
  list(countries, custom_sf_polygon, region_name_spatcov) %>%
    map(~ !is.null(.x)) %>%
    unlist() %>%
    sum() %>%
    magrittr::equals(1)
}
.is_valid_countries <- function(countries) {
  eea39_countries <- eea39_countries %>% str_to_lower()
  countries %in% eea39_countries
}
.is_valid_eumohp_version <- function(eumohp_version) {
  eumohp_version %in% c("v013.1.0", "v013.1.1")
}
.is_valid_placeholders_value <- function(filename_placeholder) {
  filename_placeholders_values <- filename_placeholders_values %>%
    str_remove("streamorder") %>%
    purrr::set_names(names(filename_placeholders_values))
  filename_placeholder %in%
    filename_placeholders_values[
      names(filename_placeholders_values) ==
        deparse(substitute(filename_placeholder))]
}
.with_order_extension <- function(spec_list, order_name) {
  spec_list$hydrologic_order <- str_c(
    order_name,
    spec_list$hydrologic_order
  )
  spec_list
}
.specs_to_pattern <- function(subset_list) {
  subset_list %>%
    map(str_c, collapse = "|") %>%
    map(~ if (str_detect(.x, "\\|")) {
      str_c("(", .x, ")")
    } else {
      .x
    }) %>%
    str_c(collapse = "_") %>% 
    str_c("_", ., ".tif")
}
.get_missing_speclist_elem <- function(missing_placeholder_spec) {
  filename_placeholders_values %>% 
    keep(., names(.) == missing_placeholder_spec) %>%
    as.vector()
}
.complete_subsetspecs <- function(subset_specs) {
  missing_placeholder_spec <- filename_placeholders %>%
    discard(filename_placeholders %in% names(subset_specs))
  missing_placeholder_spec %>%
    map(.get_missing_speclist_elem) %>%
    purrr::set_names(missing_placeholder_spec) %>%
    c(subset_specs) %>%
    magrittr::extract(filename_placeholders)
}
.generate_error_message <- function(argument) {
  eea39_countries <- eea39_countries %>% str_to_lower()
  argument_name <- deparse(substitute(argument))
  if (argument_name == "countries") {
    wrong_strings <- str_c(
      argument[!.is_valid_countries(argument)],
      collapse = ", ")
    correct_strings <- str_c(eea39_countries, collapse = ", ")
  } else if (argument_name %in% filename_placeholders) {
    correct_strings <- filename_placeholders_values %>%
      gsub(pattern = "streamorder", replacement = "") %>% 
      keep(., names(.) == argument_name) %>%
      as.vector()
    wrong_strings <- correct_strings %>% 
      discard(argument, magrittr::is_in(argument, .)) %>%
      str_c(collapse = ", ")
    correct_strings <- correct_strings %>%
      str_c(collapse = ", ")
  }
  abort(paste0(
    "Invalid values provided to the argument ",
    crayon::red(argument_name),
    ":\n ",
    crayon::red(wrong_strings),
    "\nPlease check if your provided value(s) is/are one of:\n",
    crayon::green(correct_strings)
  ))
}
.subset_filepaths <- function(filepaths, subset_list) {
  filepaths %>%
    dplyr::as_tibble() %>%
    mutate(filename = basename(.data$value)) %>%
    filter(str_detect(.data$filename, .specs_to_pattern(subset_list))) %>%
    mutate(filename_specs = str_remove(.data$filename, "mohp_europe_")) %>%
    mutate(filename_specs = str_remove(.data$filename_specs, ".tif")) %>%
    tidyr::separate(.data$filename_specs,
                    into = filename_placeholders,
                    sep = "_")
}
.starsproxylist_as_mosaic <- function(starsproxylist) {
  starsproxylist %>%
    purrr::reduce(.f = stars::st_mosaic)
}
.read_starsproxy_aslist <- function(filepaths) {
  filepaths %>%
    map(stars::read_stars, proxy = TRUE)
}
.read_and_clip_stars <- function(filepaths_subset, clip_layer) {
  filepaths_subset <- filepaths_subset %>%
    group_by(dplyr::across(dplyr::all_of(filename_placeholders[2:4])))
  
  if (is.null(clip_layer)) {
    spatial_prefix <- filepaths_subset %>%
      arrange(.data$region_name_spatcov) %>%
      dplyr::pull(.data$region_name_spatcov) %>%
      unique() %>%
      str_c(collapse = "-")
  } else {
    spatial_prefix <- clip_layer$name
  }
  
  mosaic_names <- filepaths_subset %>%
    summarise(.groups = "drop") %>%
    tidyr::unite(mosaic_names) %>%
    dplyr::pull(mosaic_names) %>% 
    str_c(spatial_prefix, ., sep = "_")
  
  all_regions <- filepaths_subset %>%
    dplyr::group_map(~ .x$value %>% .read_starsproxy_aslist()) %>%
    map(.starsproxylist_as_mosaic) %>%
    purrr::set_names(mosaic_names)
  
  if (!is.null(clip_layer)) {
    all_regions <- all_regions %>%
      map(~ .x %>% sf::st_crop(sf::st_transform(clip_layer, sf::st_crs(.x))))
  }
  return(all_regions)
}
.eumohp_covered_countries <- function() {
  system.file("extdata",
              "eumohp_covered_countries.rds",
              package = "eumohpclipr",
              mustWork = TRUE) %>%
    readRDS()
}
.test_custom_sf_polygon <- function() {
  system.file("extdata",
              "test_custom_sf_polygon.rds",
              package = "eumohpclipr",
              mustWork = TRUE) %>%
    readRDS()
}
.check_args <- function(
  countries,
  custom_sf_polygon,
  region_name_spatcov,
  hydrologic_order,
  abbreviation_measure,
  spatial_resolution,
  eumohp_version,
  buffer,
  order_name
) {
  if (list(countries,
           custom_sf_polygon,
           region_name_spatcov) %>%
      purrr::map_lgl(is.null) %>%
      all()
  ) {
    abort(
      paste0(
        "You have to provide an argument for the spatial coverage. ",
        "\nPlease provide exactly one of the following three arguments: ",
        crayon::green(str_c(c(
          "countries",
          "custom_sf_polygon",
          "region_name_spatcov"
        ),
        collapse = ", "
        ))
      )
    )
  }
  if (!.is_valid_mode_selection(
    countries,
    custom_sf_polygon,
    region_name_spatcov
  )) {
    abort(
      paste0(
        "You provided more than one argument for the spatial coverage. ",
        "Please provide exactly one of the following three arguments: ",
        crayon::green(str_c(c(
          "countries",
          "custom_sf_polygon",
          "region_name_spatcov"
        ),
        collapse = ", "
        ))
      )
    )
  }
  if (!is.null(countries) & !(.is_valid_countries(countries) %>% all())) {
    .generate_error_message(countries)
  }
  if (!is.null(custom_sf_polygon) &
      !(.is_valid_custom_sf_polygon(custom_sf_polygon) %>%
        all())) {
    abort(paste0(
      "Invalid sf object provided to the argument ",
      crayon::red("custom_sf_polygon"),
      ".",
      "\nCheck if your provided sf object has just a single feature. ",
      "If not, please use the function summarise from the sf package ",
      "to merge the features."
    ))
  }
  if (!is.null(region_name_spatcov) &
      !(.is_valid_placeholders_value(region_name_spatcov) %>%
        all())) {
    .generate_error_message(region_name_spatcov)
  }
  if (!is.null(hydrologic_order) &
      !(.is_valid_placeholders_value(hydrologic_order) %>%
        all())) {
    .generate_error_message(hydrologic_order)
  } else if (!is.null(hydrologic_order)) {
    hydrologic_order <- str_c(order_name, hydrologic_order)
  }
  if (!is.null(abbreviation_measure) &
      !(.is_valid_placeholders_value(abbreviation_measure) %>%
        all())) {
    .generate_error_message(abbreviation_measure)
  }
  if (!is.null(spatial_resolution) &
      !(.is_valid_placeholders_value(spatial_resolution) %>%
        all())) {
    .generate_error_message(spatial_resolution)
  }
  if (!.is_valid_eumohp_version(eumohp_version)) {
    abort(paste0(
      "Invalid eumohp version provided to the argument ",
      crayon::red("eumohp_version"),
      ":\n",
      crayon::red(eumohp_version),
      "\nPlease check if your provided value is one of:\n",
      crayon::green(str_c(c("v013.1.0", "v013.1.1"),
                          collapse = ", "
      ))
    ))
  }
  if (!is.null(region_name_spatcov) &
      !is.null(buffer)) {
    abort(paste("Please don't provide the argument buffer",
                "when using the region_name_spatcov argument!"))
  }
}

.generate_clip_layer <- function(
  countries,
  custom_sf_polygon,
  buffer
) {
  
  if (!is.null(countries) & (.is_valid_countries(countries) %>% all())) {
    clip_layer <- .eumohp_covered_countries() %>%
      filter(str_to_lower(.data$name) %in% countries) %>%
      arrange(.data$name) %>%
      group_by(name = str_to_lower(str_c(.data$name, collapse = "-"))) %>%
      summarise()
  } else if (!is.null(custom_sf_polygon) &
             (.is_valid_custom_sf_polygon(custom_sf_polygon) %>%
              all())) {
    clip_layer <-
      custom_sf_polygon %>%
      sf::st_geometry() %>%
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON") %>%
      dplyr::rename(geometry = .data$x) %>%
      mutate(name = "custompolygon", .before = 1)
  } else {
    clip_layer <- NULL
  }
  
  if (is.numeric(buffer)) {
    clip_layer <- clip_layer %>%
      sf::st_buffer(dist = buffer) %>%
      mutate(name = str_c(.data$name, "-b", buffer))
  }
  clip_layer
}

eumohp_clip <- function(directory_input,
                        countries = NULL,
                        custom_sf_polygon = NULL,
                        region_name_spatcov = NULL,
                        hydrologic_order = 1:9,
                        abbreviation_measure = c("dsd", "lp", "sd"),
                        spatial_resolution = "30m",
                        eumohp_version = "v013.1.0",
                        buffer = NULL) {
  
  eea39_countries <- eea39_countries %>% str_to_lower()
  
  filename_placeholders_values <- filename_placeholders_values %>%
    str_remove("streamorder") %>%
    purrr::set_names(names(filename_placeholders_values))
  
  order_name <- ifelse(eumohp_version == eumohp_versions[1],
                       "streamorder",
                       "hydrologicorder"
  )
  
  .check_args(
    countries,
    custom_sf_polygon,
    region_name_spatcov,
    hydrologic_order,
    abbreviation_measure,
    spatial_resolution,
    eumohp_version,
    buffer,
    order_name
  )
  
  clip_layer <- .generate_clip_layer(
    countries,
    custom_sf_polygon,
    buffer
  )
  
  filepaths <- list.files(
    directory_input,
    full.names = TRUE,
    recursive = TRUE,
    pattern = "mohp_europe_*.*tif"
  )
  
  subset_specs <-
    as.list(environment()) %>%
    magrittr::extract(filename_placeholders) %>%
    purrr::compact() %>%
    .complete_subsetspecs() %>%
    .with_order_extension(order_name)
  
  filepaths_subset <- filepaths %>% .subset_filepaths(subset_specs)
  filepaths_subset %>%
    .read_and_clip_stars(clip_layer)
}
