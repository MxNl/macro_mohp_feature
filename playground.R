library(targets)
library(raster)
library(sf)
library(stars)
library(furrr)
library(patchwork)
library(assertr)
library(tarchetypes)
library(tidyverse)
library(here)

tar_read(river_networks_files) %>%
  map_dfr(read_sf) %>%
  names()

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


tar_read(river_networks_strahler_merge) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>% 
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
