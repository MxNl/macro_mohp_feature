library(targets)
library(tidyverse)




targets::tar_visnetwork(label = "time")

tar_read(centroids_stream_divide_distance) %>% 
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


test <- tar_read(centroids_lateral_position) %>% 
  sf::st_transform(crs = 32632) %>%
  fasterize::fasterize(raster = raster::raster(., res = 100),
                       field = "lateral_position")
  # raster::projectRaster(crs = "+init=epsg:25832")
  raster::plot(test)
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
