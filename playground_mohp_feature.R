library(targets)
library(raster)
library(sf)
library(stars)
library(furrr)
library(tarchetypes)
library(tidyverse)

studyarea_subset_plots <- tar_read(studyarea_subset_plots)
studyarea <- tar_read(studyarea)
river_networks_clean <- tar_read(river_networks_clean)
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
  distinct(nearest_feature) %>% 
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
