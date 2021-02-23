library(targets)
library(raster)
library(sf)
library(lwgeom)
library(stars)
library(furrr)
library(janitor)
library(patchwork)
library(assertr)
library(tarchetypes)
library(tidyverse)
library(here)



tar_read(filepath_canals_to_reclassify)
tar_read(river_networks_clip)
tar_read(features_ids_to_reclassify)
studyarea_subset_plots <- tar_read(studyarea_subset_plots)
studyarea <- tar_read(studyarea)
river_networks_clip <- tar_read(river_networks_clip)
river_networks_clean <- tar_read(river_networks_clean)
river_networks_split <- tar_read(river_networks_split)
river_networks_strahler_merge <- tar_read(river_networks_strahler_merge)
streamorders <- tar_read(streamorders)
base_grid <- tar_read(base_grid)
thiessen_catchments <- tar_read(thiessen_catchments)
river_network_by_streamorder <- tar_read(river_network_by_streamorder)
thiessen_catchments_centroids <- tar_read(thiessen_catchments_centroids)
centroids_stream_distance <- tar_read(centroids_stream_distance)
centroids_divide_distance <- tar_read(centroids_divide_distance)
centroids_divide_distance <- tar_read(centroids_divide_distance)
filepaths_lateral_position <- tar_read(filepaths_lateral_position)
grid_lateral_position <- tar_read(grid_lateral_position)
grid_stream_divide_distance <- tar_read(grid_stream_divide_distance)
tar_read(plot_subset_all_steps)
tar_read(test_processed_river_network_plot)
tar_read(test_catchments_plot)
tar_read(grid_lateral_position)
tar_read(river_networks_clean)

test <- tar_read(river_networks) %>% 
  as_tibble()

test %>% 
  filter(dfdd == "BH140" & strahler == -9999)

tar_read(river_network_by_streamorder) %>% 
  chuck(4) %>%
  mutate(feature_id = as.character(feature_id)) %>% 
  mutate(strahler = as.character(strahler)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)


tar_read(lateral_position_stream_divide_distance) %>% 
  chuck(4) %>% 
  select(all_of("lateral_position")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()

NN_GRID_RIVERS_TABLE %>% 
  composite_name(tar_read(streamorders) %>% chuck(4)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  select(all_of("distance_meters")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()

NN_GRID_CATCHMENTS_TABLE %>% 
  composite_name(tar_read(streamorders) %>% chuck(4)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  select(all_of("distance_meters")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()

THIESSEN_CATCHMENTS_TABLE %>% 
  composite_name(tar_read(streamorders) %>% chuck(2)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  mapview()

sequential_nearest_neighbours_with_maxdist <- 
  function(
    table_name_destination = "nn_test_maxdist",
    left_table,
    left_columns,
    right_table,
    right_columns = NULL,
    streamorders = NULL,
    n = 1,
    depends_on
  ) {
    test <- FALSE
    if(test){
      table_name_destination <- "nn_test_maxdist"
      left_table <- GRID_CENTROIDS
      left_columns <- c("grid_id", "geometry")
      right_table <- composite_name(LINES_BY_STREAMORDER, streamorders)
      right_columns <- c("feature_id", "strahler", "stream_order_id")
      streamorders <- 1:6
      n <- 1
    }
    
    max_distance <- FALSE
    
    for (i in sort(streamorders, decreasing = TRUE)) {
      print(i)
      
      nearest_neighbours_between(
        table_name_destination,
        left_table,
        right_table,
        left_columns,
        right_columns,
        stream_order_id = i,
        max_distance = max_distance
      )
    }
    
  }


nearest_neighbours_between <- function(
  table_name_destination,
  left_table,
  left_columns,
  right_table,
  right_columns = NULL,
  stream_order_id = NULL,
  n = 1,
  max_distance
) {
  length(depends_on)
  connection <- connect_to_database()
  
  if (length(intersect(left_columns, right_columns)) > 0) {
    stop('Ambiguous column names provided.')
  }
  
  format_select <- function(table, columns) {
    validate(table, columns)
    select_statements <- paste0(table, '.', columns)
    paste0(select_statements, collapse = ', ')
  }
  
  left_select <- format_select(left_table, left_columns) %>%
    str_replace("grid_grid_id", "grid_id") #TODO how can we make this more elegant?
  right_select <- NULL
  if (!is.null(right_columns)) {
    right_select <- format_select(right_table, right_columns)
  }
  select <- list(left_select, right_select) %>%
    compact() %>%
    str_c(collapse = ', ')
  
  left_geometry <- geometry_of(left_table)
  right_geometry <- geometry_of(right_table)
  by_streamorder <- ifelse(is.null(stream_order_id), FALSE, TRUE)
  
  distance <- glue::glue('ST_Distance({left_geometry}, {right_geometry}) AS distance_meters')
  #by_streamorder_clause <- glue::glue("AND {right_table}.stream_order_id = {stream_order_id}")
  table_destination <- composite_name(table_name_destination, stream_order_id)
  
  query <- glue::glue("
    CREATE TABLE {table_destination} AS (
      SELECT
        {select},
        {distance}
      FROM
        {left_table}
        CROSS JOIN LATERAL (
          SELECT
            *
          FROM
            {right_table}
          ORDER BY
            {left_geometry} <-> {right_geometry}
          LIMIT
            {n}
        ) AS {right_table}
     );
  ")
  print(query)
  create_table(query, table_destination, index_column = c("feature_id", "grid_id"))
  Sys.time()
}

composite_name <- function(table_name, stream_order_id) {
  if(is.null(stream_order_id)){
    table_name
  } else {
    glue::glue('{table_name}_id_{stream_order_id}')
  }
}


#TODO filter by streamorder
rivers_test <- tar_read(river_networks_only_connected)
streamorders <- tar_read(streamorders)
# rivers_merge_test <- 
#   get_table_from_postgress("merge_test") %>% 
#   query_result_as_sf() %>% 
#   add_feature_index_column()

# rivers_test %>% 
#   st_cast("MULTILINESTRING") %>% 
#   group_by(strahler) %>% 
#   summarise()

river_network <- rivers_test
streamorder <- 1
table_name_destination_prefix <- "river_networks_merge_single_streamorder"
table_name_read <- LINES_CONNECTED_ID



merge_connected_lines_by_streamorder <- 
  function(table_name_read,
           table_name_destination_prefix,
           streamorder) {
    
    table_name_destination <- 
      composite_name(table_name_destination_prefix, streamorder)
    
    run_query_connected_single_streamorder(
      table_name_read,
      table_name_destination,
      streamorder
    )
  }

run_query_connected_single_streamorder <- 
  function(table_name_read, 
           table_name_destination, streamorder) {
    query <- glue::glue("
      CREATE TABLE {table_name_destination} AS (
	      WITH single_streamorders AS (
	        SELECT
	          *
	        FROM {table_name_read}
	        WHERE strahler = {streamorder}
	      ), endpoints AS (
	        SELECT
	          ST_Collect(ST_StartPoint(geometry),
	          ST_EndPoint(geometry)) AS geometry
	        FROM single_streamorders
	      ), clusters AS (
	        SELECT
	          unnest(ST_ClusterWithin(geometry, 1e-8)) AS geometry
	        FROM endpoints
	      ), clusters_with_ids AS (
	        SELECT
	          row_number() OVER () AS connected_id,
	          ST_CollectionHomogenize(geometry) AS geometry
	        FROM clusters
	      )
      	SELECT
		      connected_id,
		      ST_Collect(single_streamorders.geometry) AS geometry
	      FROM
	        single_streamorders
	        LEFT JOIN
	        clusters_with_ids
	        ON ST_Intersects(single_streamorders.geometry, clusters_with_ids.geometry)
	      GROUP BY connected_id
	      )
    ")
    print(query)
    create_table(query, table_name_destination)
  }

streamorders %>% 
  map(~merge_connected_lines_by_streamorder(LINES_CONNECTED_ID, "river_networks_merge_single_streamorder", .x))





rivers_merge_test %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)

rivers_test %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) +
  facet_wrap(~strahler)

endpoints_one_side <-
  rivers_test %>%
  st_endpoint() %>%
  st_as_sf()

endpoints_other_side <-
  rivers_test %>%
  st_startpoint() %>%
  st_as_sf()

endpoints <-
  endpoints_one_side %>%
  bind_rows(endpoints_other_side) %>% 
  distinct()

rivers_test_filter <- 
  rivers_test %>% 
  filter_intersecting_features(endpoints)

# lines_canals_endpoints_join <-
endpoints %>%
  add_feature_index_column(column_name = "endpoint_id") %>% 
  st_join(rivers_test) %>% 
  group_by(endpoint_id) %>%
  mutate(n_features = n()) %>% 
  filter(n_features > 2) %>% 
  mutate(n_distint_strahler = length(unique(strahler))) %>% 
  filter(n_distint_strahler > 1)

group_split() %>%
  map_dfr(~st_join(., lines_filter)) %>% 
  filter(feature_id.x != feature_id.y) %>% 
  group_by(feature_id.x) %>% 
  group_split() %>% 
  map(add_side_column)

















tar_read(selected_studyarea) %>% 
  ggplot() +geom_sf()

tar_read(lateral_position_stream_divide_distance) %>%
  pluck(3) %>%
  select(all_of("lateral_position")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>%
  write_stars("output_data/300m_test.tiff")

test <- tar_read(river_networks_only_rivers)
studyarea <- tar_read(selected_studyarea)

test <- 
  test %>% 
  mutate(strahler = if_else(feature_id %in% c(212, 113, 114, 202, 89), -9999, strahler))

{test %>% 
    ggplot() +
    geom_sf(aes(colour = as.character(strahler))) +
    # geom_sf_label(data = st_centroid(test), 
    #               aes(label = feature_id, 
    #                   colour = is.element(strahler, INVALID_STRAHLER_VALUES)), 
    #               label.size = 0, 
    #               alpha =.6) +
    geom_sf(data = studyarea, fill = NA) +
    theme(legend.position = "none")} %>% 
  plotly::ggplotly()


{test %>% 
    impute_streamorder(st_cast(studyarea, "LINESTRING")) %>% 
    ggplot() +
    geom_sf(aes(colour = as.character(strahler))) +
    # geom_sf_label(data = st_centroid(test),
    #               aes(label = feature_id,
    #                   colour = is.element(strahler, INVALID_STRAHLER_VALUES)),
    #               label.size = 0,
    #               alpha =.6) +
    geom_sf(data = studyarea, fill = NA)} %>% 
  plotly::ggplotly()


test %>% 
  impute_streamorder(st_cast(studyarea, "LINESTRING")) %>% 
  filter(feature_id == 113)


test %>% 
  impute_streamorder(studyarea)






glue::glue("
        CREATE EXTENSION postgis_raster;
        CREATE TABLE {GRID_POLYGONS_TABLE} AS (
          with grid AS (
          	SELECT 
          	  (ST_PixelAsCentroids(ST_AsRaster(ST_Union(ST_MakePolygon(geometry)), {CELLSIZE}.0,{CELLSIZE}.0))).geom AS geometry
          	FROM {SELECTED_STUDYAREA_TABLE}
          ), with_id AS (
          	SELECT 
            	geometry, 
            	row_number() OVER (ORDER BY geometry) AS grid_id 
          	FROM grid
          )
        )
        ")

tar_read(base_grid_centroids) %>% 
  as_tibble()

get_table_from_postgress("merge_test") %>% 
  query_result_as_sf() %>% 
  add_feature_index_column() %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>% 
  plotly::ggplotly()


test <- 
  river_networks %>% 
  group_by(strahler) %>%
  summarise()

river_networks %>% 
  slice(1:5) %>% 
  st_cast("LINESTRING")



tar_read(river_networks_only_connected) 
tar_read(river_networks_dissolved_junctions) 
tar_read(river_networks_without_brackets) 
test <- tar_read(river_networks_only_connected) %>% 
  # as_tibble() %>% 
  slice(1:5)

tar_read(river_networks_only_connected) %>% st_geometry_type()

st_write("output_data/germany_rivers_only_connected.shp")

# rivers before calculation
test <- 
  LINES_BY_STREAMORDER %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf()

test %>% 
  group_by(stream_order_id) %>% 
  group_split() %>% 
  map(plot_lines_coloured_by_categorical_attribute, feature_id)


# nearest neighbours to rivers
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_NN_STREAMORDER, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(1) %>% 
  select(grid_geometry, distance_meters) %>% 
  rename(geometry = grid_geometry) %>% 
  query_result_as_sf() %>% 
  mapview::mapview(alpha = 0, zcol = "distance_meters")


# thiessen catchments
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_THIESSEN_CATCHMENTS, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(6) %>%
  query_result_as_sf() %>%
  mapview::mapview(alpha = 1)

# nearest neighbours to divide
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_NN_GRID_TO_CATCHMENTS, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(2) %>%
  rename(geometry = grid_geometry) %>% 
  query_result_as_sf() %>%
  mapview::mapview(alpha = 0, zcol = "distance_meters")



# lateral position
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_LATERAL_POSITION_STREAM_DIVIDE_DISTANCE, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(1) %>% 
  select(geometry, divide_stream_distance) %>%
  query_result_as_sf() %>% 
  mapview::mapview(alpha = 0, zcol = "divide_stream_distance")


tar_read(lateral_position_stream_divide_distance)

tar_meta() %>% 
  select(name, seconds, type) %>% 
  filter(type == "stem") %>% 
  arrange(-seconds) %>% 
  View()





tar_read(river_basins) %>%
  slice(1:3) %>% 
  # summarise() %>% 
  st_difference(tar_read(coastline) %>% st_zm()) %>%
  st_cast("POLYGON") %>% 
  select(geometry)
tar_read(river_networks_clip)
tar_read(river_basins) %>% 
  st_intersects(tar_read(selected_studyarea))

tar_read(river_networks_files)

river_basins <- tar_read(river_basins)
studyarea <- tar_read(selected_studyarea)
river_networks <- tar_read(river_networks)

relevant_river_basins <- 
  river_basins %>%
  filter(as.vector(st_intersects(., studyarea, sparse = FALSE))) %>% 
  pull(river_basin_name)


river_networks %>%
  # slice_sample(prop = .6) %>%
  filter(river_basin_name %in% relevant_river_basins) %>%
  filter(as.vector(st_intersects(., studyarea, sparse = FALSE))) %>%
  st_intersection(studyarea) %>% 
  st_cast("MULTILINESTRING") %>% 
  add_feature_index_column()

tar_read(lateral_position_stream_divide_distance) %>% 
  pluck(3) %>% 
  select(lateral_position) %>% 
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()



test_rivers <- 
  tar_read(river_networks_files_files) %>% 
  magrittr::extract(str_detect(., "rhine")) %>% st_read("River_Net_l")

test_rivers %>% 
  st_intersects(tar_read(studyarea_subset_plots), sparse = FALSE) %>% 
  as.vector() %>% 
  magrittr::extract(. == FALSE)




tar_read(river_networks)

is_db_hash_outdated(tar_read(db_river_networks_clean), LINES_CLEAN) | is_db_hash_outdated(tar_read(db_connected_but_merged_river_networks), LINES_CONNECTED_ID)

tar_read(river_network_by_streamorder) %>% 
  filter(stream_order_id == 1)

test <- 
  get_table_from_postgress("nearest_neighbours_streamorder_id_1") %>% 
  rename(geometry = grid_geometry) %>% 
  query_result_as_sf()

thiessen_catchments <- 
  test %>% 
  rename(nearest_feature = river_network_by_streamorder_feature_id) %>% 
  make_thiessen_catchments(tar_read(base_grid), .)

thiessen_catchments %>% 
  mutate(feature_id = as.character(row_number())) %>% 
  ggplot() +
  geom_sf(fill = NA)



test %>% 
  select(distance_meters) %>% 
  st_intersection(st_buffer(sample_n(., 1), dist = 1E4)) %>% 
  mapview::mapview(zcol = "distance_meters", alpha = 0)


area_test <- 
  studyarea_subset_plots %>% 
  st_area() %>% 
  as.numeric()

area_test / 1E6

6E6 / (area_test / 1E6)
tar_read(river_networks_only_connected) %>% 
  as_tibble() %>% 
  mutate(geometry = as.character(geometry)) %>% 
  distinct(geometry)
tar_read(river_networks_clean)


tar_read(river_networks_only_connected) %>% 
  mutate(strahler = as.character(strahler)) %>% 
  plot_lines_coloured_by_categorical_attribute(strahler) %>% 
  plotly::ggplotly()

list(
  tar_read("river_networks_clip"),
  tar_read(river_networks_only_rivers),
  tar_read(river_networks_valid_strahler),
  tar_read(river_networks_clean),
  tar_read(river_networks_only_connected),
  tar_read(river_networks_dissolved_junctions),
  tar_read(river_networks_without_brackets),
  tar_read(river_networks_dissolved_junctions_after)
) %>%
  map(plot_lines_coloured_by_categorical_attribute, feature_id) %>% 
  pluck(1) %>% 
  list() %>% 
  cowplot::plot_grid(plotlist = .)


test <- 
  get_table_from_postgress(connect_to_database(), "dc_segments") %>%
  query_result_as_sf() %>%
  add_feature_index_column()


connection <- 
  connect_to_database()

tar_read(river_networks_clean) %>% 
  st_cast("LINESTRING") %>% 
  write_to_table(
    connection = connection,
    table_name = "test_lines_clean"
  )




tar_read(river_networks_dissolved_brackets) %>% 
  st_cast("LINESTRING") %>% 
  add_feature_index_column() %>% 
  # dissolve_line_features_between_junctions() %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>%
  plotly::ggplotly()

tar_read(river_networks_dissolved_brackets) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>%
  plotly::ggplotly()


strahler_error <- 
  tar_read(river_networks_clip) %>% 
  filter(abs(strahler) >= 10 & dfdd == "BH140") %>% 
  select(strahler)


tar_read(river_networks_clip) %>% 
  filter(dfdd == "BH140") %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = strahler_error, colour = "red")

tar_read(river_networks_only_rivers) %>% 
  mutate(strahler = as.character(strahler)) %>% 
  # distinct(strahler)
  plot_lines_coloured_by_categorical_attribute(strahler) +
  geom_sf(data = tar_read(studyarea_subset_plots), fill = NA) +
  theme(legend.position = "bottom")

plot_before_vs_after(tar_read(river_networks_clean), tar_read(river_networks_strahler_merge))

log(1:6)+1


tar_read(river_networks_strahler_merge) %>% 
  ggplot() +
  geom_sf()

plot_b <- 
  tar_read(river_networks_strahler_merge) %>% 
  ggplot() +
  geom_sf(aes(colour = feature_id, size = log(as.numeric(strahler)+1))) +
  scale_colour_manual(values = generate_discrete_colour_values(tar_read(river_networks_strahler_merge), feature_id)) +
  theme(legend.position = "none") +
  labs(title = "Colour represents line features")




river_networks_strahler_merge %>% 
  st_sf()

tar_read(river_networks_split) %>% 
  bind_rows()

sf_lines %>% 
  st_cast("MULTILINESTRING")


river_networks_strahler_merge %>% 
  as_tibble() %>% 
  distinct(strahler) %>% 
  pull(strahler) %>% 
  as.numeric() %>%
  sort()

base_grid %>% 
  ggplot() +
  geom_sf()


st_layers("J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/european_catchments_and_rivers_network_system_(ecrins)/data/EcrRiv.sqlite")
st_read("J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/european_catchments_and_rivers_network_system_(ecrins)/data/EcrRiv.sqlite",
        layer = "c_tr")

values <-
  tibble(
    output_function = rlang::syms("calculate_lateral_position_grid"),
    streamorders = 1:6,
    # centroids_stream_distance = rlang::syms("centroids_stream_distance"),
    # centroids_divide_distance = rlang::syms("centroids_divide_distance"),
    grid =  rlang::syms("base_grid"),
    field_name = "lateral_position",
    data_source = str_c(field_name, "_", streamorders)
  )


targets_output <- 
  tar_map(
    
    # tar_target(
    #   files_lateral_position,
    #   generate_filepaths(
    #     streamorders,
    #     "lp"
    #   ),
    #   format = "file"
    # ),
    values = values,
    names = streamorders,
    tar_target(
      lateral_position,
      # list(
      #   centroids_stream_distance,
      #   centroids_divide_distance,
      #   streamorders  
      # ),
      output_function(
        centroids_stream_distance,
        centroids_divide_distance,
        streamorders,
        grid,
        field_name
      ),
      format = "file"
    )
  )

tar_pattern(
  map(x),
  x = 5,
  y = 1
)
targets::tar_make_future(workers = future::availableCores())
tar_read(unique_feature_ids)


river_networks_clean %>% 
  as_tibble() %>% 
  distinct(feature_id) %>% 
  pull(feature_id)

river_networks_clean <- 
  river_networks_clean %>% 
  st_cast("MULTILINESTRING") %>%
  rename(geometry = x)

river_networks_strahler_merge %>% 
  ggplot() +
  geom_sf(aes(colour = feature_id))

st_distance(
  filter(river_networks_clean, feature_id == 2) %>% summarise(),
  filter(river_networks_clean, feature_id == 12) %>% summarise()
)

# tar_read(coastline) %>% 
#   # slice(1:5) %>% 
#   st_union()

tar_read(river_networks_strahler_merge) %>% 
  plot_test_processed_river_network(tar_read(studyarea_subset_plots))


river_networks_strahler_merge %>% 
  st_as_sf() %>%
  mutate(id = as.character(1:n())) %>%
  ggplot() +
  geom_sf(aes(colour = id))
ggplot()

testplot <-
  ggplot() +
  geom_sf(
    data = thiessen_catchments[[1]],
    colour = "white",
    fill = "grey70"
  ) +
  geom_sf(
    data = river_network_by_streamorder[[1]],
    aes(colour = as.factor(strahler))
  )

testplot %>% 
  plotly::ggplotly()

river_network_by_streamorder[[1]] <- 
  river_network_by_streamorder[[1]] %>% 
  mutate(segment_id = 1:n())

segment_colours <- 
  river_network_by_streamorder[[1]] %>%
  # distinct(nearest_feature) %>% 
  nrow() %>% 
  hues::iwanthue(lmin = 40,
                 cmax = 70)

testplot1 <- 
  river_network_by_streamorder[[1]] %>% 
  ggplot() +
  geom_sf(
    data = river_network_by_streamorder[[1]],
    aes(colour = as.factor(strahler)),
    size = 2
  ) +
  theme(legend.position = "none") +
  labs(title = "Farbecode repräsentiert Strahler order")

testplot2 <- 
  river_network_by_streamorder[[1]] %>% 
  ggplot() +
  geom_sf(aes(colour = as.character(segment_id)),
          size = 2) +
  scale_colour_manual(values = segment_colours) +
  theme(legend.position = "none") +
  labs(title = "Farbecode repräsentiert Features/Liniensegmente")

testplot1 + testplot2






river_network_by_streamorder[[1]] %>% 
  mutate(test = river_network_by_streamorder[[1]] %>% 
           st_touches(sparse = FALSE))

test <- river_network_by_streamorder[[1]] %>% 
  st_touches()

key <- 
  test %>% 
  map(as.vector) %>% 
  # imap(~c(.x, .y)) %>% 
  map(sort) %>% 
  map(as.character) %>% 
  map(str_c, collapse = "") %>% 
  unlist()

lines_dissolved <- 
  river_network_by_streamorder[[1]] %>% 
  mutate(key = key, .before = 2) %>% 
  mutate(key = str_c(key, strahler)) %>% 
  group_by(key) %>% 
  summarise()

segment_colours <- 
  lines_dissolved %>%
  # distinct(nearest_feature) %>% 
  nrow() %>% 
  hues::iwanthue(lmin = 40,
                 cmax = 70)

testplot3 <-
  lines_dissolved %>%  
  ggplot() +
  geom_sf(aes(colour = as.character(key)),
          size = 2) +
  scale_colour_manual(values = segment_colours) +
  theme(legend.position = "none") +
  labs(title = "Farbecode repräsentiert Features/Liniensegmente")


testplot3 + testplot2

river_network_by_streamorder[[1]] %>% 
  st_touches(sparse = FALSE) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  filter(value == TRUE) %>% 
  mutate(name = as.numeric(str_sub(name, 2L))) %>% 
  mutate(name2 = 1:n(), .before = 1)


river_network_by_streamorder[[1]] %>% 
  st_intersects(river_network_by_streamorder[[1]])


lines_dissolved <- 
  river_network_by_streamorder[[1]] %>% 
  group_by(strahler) %>% 
  summarise()

lines_singlefeature <- 
  river_network_by_streamorder[[1]] %>% 
  # group_by(strahler) %>% 
  summarise()

river_network_by_streamorder[[1]] %>%
  group_by(strahler) %>%
  summarise() %>%
  st_line_merge()

st_union(lines_dissolved, lines_singlefeature, by_feature = TRUE, is_coverage = TRUE)

st_line_merge()
lwgeom::st_split()
st_combine()
st_intersection()
st_union()
st_collection_extract()



lines_dissolved %>% 
  st_line_merge()


river_networks_dissolved <- 
  river_networks_merge %>%
  group_by(strahler) %>%
  summarise() %>%
  st_line_merge()

result_intersection <- 
  lines_singlefeature %>%
  st_intersection(lines_dissolved %>% 
                    # select(-strahler) %>%
                    st_cast("MULTILINESTRING"))

result_intersection_points <-
  result_intersection %>% 
  filter(st_is(., "POINT")) %>% 
  distinct(geometry)














ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
            st_linestring(rbind(c(0,0),c(10,0))))
st_line_sample(ls, density = 1)



lwgeom::st_split(lines_singlefeature, lines_dissolved)

lines_singlefeature %>% 
  st_intersection(lines_dissolved)

testplot1 + testplot2

testplot %>% 
  plotly

grid_stream_divide_distance[[1]] %>% 
  sfpolygon_to_raster("test")

grid_stream_divide_distance[[1]] %>% 
  pull(stream_divide_distance) %>% 
  range()

grid_stream_divide_distance[[1]] %>% 
  # mutate(stream_divide_distance = stream_divide_distance/1E5) %>% 
  stars::st_rasterize() %>% 
  write_stars("asda.tiff")


grid_lateral_position

tar_read(streamorders) %>% 
  as.vector() %>% 
  as.numeric() %>% 
  future_map(
    ~stream_order_filter(
      river_network = river_networks_clean,
      stream_order = .x
    )
  )

streamorders %>% 
  as.vector() %>% 
  as.numeric() %>% 
  future_map(
    ~stream_order_filter(
      river_network = river_networks_clean,
      stream_order = .x
    )
  )


studyarea %>% 
  ggplot() +
  geom_sf()

centroids_lateral_position %>% 
  slice(1:1E5) %>% 
  ggplot() +
  geom_sf(aes(fill = lateral_position))

centroids_stream_divide_distance %>% 
  pull(stream_divide_distance) %>% 
  range()

centroids_stream_divide_distance %>% 
  select(stream_divide_distance) %>% 
  ggplot() +
  geom_histogram(aes(stream_divide_distance))


test_plot <- centroids_lateral_position %>% 
  st_intersection(studyarea_subset_plots) %>% 
  ggplot() +
  geom_sf(aes(fill = lateral_position),
          colour = NA) +
  scale_fill_viridis_c()

test_plot

test_plot <- centroids_stream_divide_distance %>% 
  st_intersection(studyarea_subset_plots) %>% 
  ggplot() +
  geom_sf(aes(fill = stream_divide_distance),
          colour = NA) +
  scale_fill_viridis_c()

test_plot

test <- centroids_lateral_position %>% 
  st_intersection(studyarea_subset_plots) %>% 
  st_rasterize()

test %>%
  write_stars("test.tiff")


rivers <- tar_read(river_network_by_streamorder)

rivers %>% 
  as_tibble() %>% 
  distinct(strahler)

rivers %>% 
  filter(strahler == 6) %>% 
  ggplot() +
  geom_sf()

360E3 * 1E6 / 300^2

targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)

tar_read(centroids_stream_divide_distance) %>% 
  st_intersection(tar_read(studyarea_subset_plots)) %>% 
  ggplot() +
  geom_sf(aes(fill = stream_divide_distance),
          colour = NA) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  # scale_fill_gradientn(colours = c("royalblue", "purple", "magenta3", "orange", "cornsilk")) +
  geom_sf(data = tar_read(thiessen_catchments),
          fill = NA,
          colour = "white") +
  geom_sf(data = tar_read(river_network_by_streamorder),
          fill = NA,
          colour = "black")


test_intersection <- 
  tar_read(centroids_lateral_position) %>% 
  st_intersection(tar_read(studyarea_subset_plots)) %>% 
  sf::st_transform(crs = 32632)


grid_lateral_position[[1]] %>% 
  sfpolygon_to_raster %>% 
  writeRaster(str_c("output_data/", "mohp_germany_", "order", 1, "_", CELLSIZE, "m_res", ".tif"))

fasterize::fasterize(raster = raster::raster(., res = CELLSIZE),
                     field = "lateral_position") %>% 
  raster::plot()

test_raster <- 
  test_intersection %>% 
  fasterize::fasterize(raster = raster::raster(., res = 100),
                       field = "lateral_position")
# raster::projectRaster(crs = "+init=epsg:25832")
raster::plot(test_raster)
raster::plot(tar_read(river_network_by_streamorder) %>% sf::st_transform(crs = sf::st_crs(test)), add = TRUE)

# stars::st_rasterize()
# stars::write_stars("output/test.tiff")
# raster::as.raster() %>% 
# raster::writeRaster("output/test.tif")
plot()

raster::raster("output/test.tiff") %>% 
  
  ggplot() +
  geom_raster()


segment_colours <- 
  tar_read(thiessen_catchments) %>% 
  pluck(1) %>% 
  # distinct(nearest_feature) %>% 
  nrow() %>% 
  hues::iwanthue(lmin = 40,
                 cmax = 70)

tar_read(thiessen_catchments) %>% 
  mutate(id = as.character(1:n())) %>% 
  ggplot() +
  # geom_sf(aes(fill = id),
  #         colour = "white") +
  # scale_fill_manual(values = segment_colours) +
  geom_sf(data = tar_read(river_network_by_streamorder) %>% mutate(strahler = as.numeric(strahler)/2),
          aes(size = strahler),
          lineend = "round",
          linejoin = "round") +
  scale_size_identity() +
  theme(legend.position = "none")



polygon <- 
  tibble(x = 1,
         y = 1) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_buffer(2) %>% 
  st_cast("MULTILINESTRING")

point <- 
  tibble(x = 1.5,
         y = 2) %>% 
  st_as_sf(coords = c("x", "y"))

st_distance(polygon, point, by_element = TRUE)


polygon %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = point)



# features_to_reclassify <- 
#   features_to_reclassify %>% 
# mutate(unique_feature_id = str_c(source_id, objectid, sep = "_"),
#        .before = 1) %>% 


add_source_id <- 
  function(x, index) {
    x %>% 
      mutate(source_id = as.character(index),
             .before = 1)
  }


cyls <- 4
mtcars %>%
  # filter(cyl == cyls) %>%
  group_by(vs) %>%
  do({
    assert_that(
      length(na.omit(.$mpg)) > 1,
      msg = "I cannot grok the data"
    )
    .
  }) %>%
  summarize(z = max(density(mpg)$y))








tar_read(river_networks_clean) %>% 
  dissolve_line_features_between_junctions() %>% 
  add_feature_index_column() %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)





tar_read(river_networks_dissolved) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>% 
  plotly::ggplotly()

tar_read(river_networks_dissolved) %>% 
  filter(feature_id %in% c(80, 12, 83, 81)) %>% 
  st_touches()


{tar_read(river_networks_dissolved) %>% 
    ggplot() +
    geom_sf() +
    geom_sf(data = split_points, size = 3)} %>% plotly::ggplotly()
