library(sf)
library(stars)
library(raster)
library(rgrass7)
library(tidyverse)

raster <- tar_read(reference_raster)
studyarea <- tar_read(selected_studyarea)
lines <- LINES_BY_STREAMORDER %>% 
  composite_name(tar_read(streamorders) %>% chuck(1)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  add_feature_index_column() %>% 
  mutate(feature_id = as.integer(feature_id))

lines %>% plot()

raster %>% plot()
link2GI::findGRASS()
Sys.setenv(GRASS_ADDON_PATH="C:\\Noelscher.M\\Anwendungsdaten\\GRASS7\\addons")
initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.8",
          addon_base = "C:/Noelscher.M/Anwendungsdaten/GRASS7/addons",
          mapset = "PERMANENT",
          override = TRUE)
link2GI::linkGRASS7(
  raster,
  default_GRASS7 = c(
    "C:\\Program Files\\GRASS GIS 7.8",
    "GRASS GIS 7.8",
    "NSIS"
  )
)
use_sf()
writeVECT(st_transform(lines, crs(raster)), "river_network", v.in.ogr_flags = c("overwrite"))
# writeVECT(st_transform(studyarea, crs(raster)), "studyarea", v.in.ogr_flags = c("overwrite"))
use_sp()
writeRAST(as(raster, "SpatialGridDataFrame"), "reference_raster")

execGRASS("v.to.rast", 
          input = "river_network", 
          output = "river_network_raster", 
          use = "attr",
          attribute_column = "feature_id",
          flags = c("overwrite"))

execGRASS("r.mapcalc",
          expression = "river_network_raster = round(river_network_raster)",
          flags = c("overwrite"))

execGRASS("r.null",
          map = "river_network_raster")

execGRASS("r.thin",
          input = "river_network_raster",
          output = "river_network_value_raster_thin",
          flags = c("overwrite"))

# execGRASS("r.mask",
#           raster = "reference_raster",
#           maskcats = 1)

# use_sp()
# readRAST("reference_raster") %>% plot()

execGRASS("r.grow.distance",
          input = "river_network_value_raster_thin",
          distance = "river_network_distance_raster", 
          value = "river_network_value_raster", 
          flags = c("overwrite"))

execGRASS("r.watershed",
          elevation = "river_network_distance_raster_carved",
          drainage = "test_direction_carved",
          stream = "test_river_carved",
          threshold = 100,
          flags = c("overwrite"))



execGRASS("r.to.vect",
          input = "river_network_value_raster_thin",
          output = "river_network_value_raster_thin_vect",
          type = "line",
          flags = c("overwrite"))

execGRASS("r.carve",
          raster = "river_network_distance_raster",
          vector = "river_network_value_raster_thin_vect",
          output = "river_network_distance_raster_carved",
          depth = 200,
          flags = c("overwrite"))


execGRASS("r.stream.extract",
          elevation = "river_network_distance_raster",
          threshold = 10,
          stream_raster = "test_river",
          direction = "test_direction",
          flags = c("overwrite"))

execGRASS("r.mapcalc",
          expression = "raster_intersection = if((test_river) && (river_network_value_raster_thin))",
          flags = c("overwrite"))

use_sp()
readRAST("test_river_carved") %>%
  raster() %>% 
  plot()


execGRASS("r.patch",
          input = c("test_direction", "river_network_value_raster_thin"),
          output = "test_direction_patched",
          flags = c("overwrite"))

execGRASS("r.stream.order",
          stream_rast = "test_river",
          direction = "test_direction",
          hack = "test_hack",
          flags = c("overwrite"))

use_sp()
readRAST("test_hack") %>%
  raster() %>% 
  plot()

# execGRASS("r.null",
#           map = "river_network_value_raster")
# 
# execGRASS("r.thin",
#           input = "river_network_value_raster",
#           output = "river_network_value_raster_thin",
#           iterations = 300,
#           flags = c("overwrite"))

execGRASS("r.to.vect",
          input = "river_network_value_raster",
          output = "thiessen_catchments", 
          type = "area", 
          flags = c("overwrite"))

# execGRASS("v.to.lines",
#           input = "thiessen_catchments",
#           output = "thiessen_catchments_outline",
#           flags = c("overwrite"))

use_sf()
thiessen_catchments <- readVECT("thiessen_catchments")
thiessen_catchments %>% 
  st_intersection(st_transform(studyarea, crs=st_crs(thiessen_catchments))) %>%
  st_cast("MULTILINESTRING") %>% 
  writeVECT("thiessen_catchments_lines", v.in.ogr_flags = c("overwrite"))

execGRASS("v.to.rast", 
          input = "thiessen_catchments_lines", 
          output = "thiessen_catchments_lines_raster", 
          use = "attr",
          attribute_column = "value",
          flags = c("overwrite"))

execGRASS("r.mapcalc",
          expression = "thiessen_catchments_lines_raster = round(thiessen_catchments_lines_raster)",
          flags = c("overwrite"))

execGRASS("r.null",
          map = "thiessen_catchments_lines_raster")

execGRASS("r.thin",
          input = "thiessen_catchments_lines_raster",
          output = "thiessen_catchments_lines_raster_thin",
          flags = c("overwrite"))

execGRASS("r.clip",
          input = "thiessen_catchments_lines_raster_thin",
          output = "thiessen_catchments_lines_raster_clip",
          flags = c("overwrite"))

execGRASS("r.grow.distance",
          input = "thiessen_catchments_lines_raster_thin",
          distance = "thiessen_catchments_distance_raster",
          flags = c("overwrite"))

execGRASS("r.mapcalc",
          expression = "divide_stream_distance = river_network_distance_raster + thiessen_catchments_distance_raster",
          flags = c("overwrite"))

execGRASS("r.mapcalc",
          expression = "lateral_position = river_network_distance_raster/divide_stream_distance",
          flags = c("overwrite"))

rgrass7::use_sp()
rgrass7::readRAST("lateral_position") %>% 
  # raster() %>% 
  # raster::mask(raster) %>% 
  plot()


use_sp()
readRAST("river_test") %>%
  raster() %>% 
  plot()

use_sf()
readVECT("thiessen_catchments") %>% 
  as_tibble() %>% 
  distinct(value) %>% 
  arrange(value) %>% 
  anti_join(tibble(value = 1:106), .)

{lines %>% 
  # filter(feature_id == 55) %>% 
  ggplot() +
  geom_sf(aes(colour = feature_id == 55)) +
  geom_sf_label(aes(label = feature_id))} %>% plotly::ggplotly()

use_sf()
catchments <- readVECT("thiessen_catchments")
test_size <- 
  raster %>% 
  st_as_stars() %>% 
  st_as_sf() %>% 
  slice(1:1000)
  

{lines %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) +
  geom_sf(data = test_size, fill = NA, colour = "green") +
  geom_sf(data = catchments, fill = NA)
  } %>% plotly::ggplotly()

use_sp()
readRAST("river_network_value_raster") %>% plot()

raster_spat <- rast(raster)
lines_spat <- vect(as_Spatial(lines))
distance_raster <- terra::distance(raster_spat, lines_spat) %>% 
  raster::raster()

crs(distance_raster)


use_sf()

{lines %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) +
  geom_sf(data = outlet_points)} %>% plotly::ggplotly()

outlet_points_coords <- 
  outlet_points %>%
  slice(1:5) %>% 
  st_coordinates()


distance_raster %>% plot()

rgrass7::writeRAST(as(distance_raster, "SpatialGridDataFrame"), "distance_raster")
execGRASS("r.watershed", 
          elevation="distance_raster", 
          basin="thiessen_catchments", 
          threshold=1, 
          flags = c("overwrite"))
execGRASS("r.watershed", 
          elevation="distance_raster", 
          drainage="drainage_direction", 
          # threshold=1, 
          flags = c("overwrite"))
execGRASS("r.to.vect", 
          input="thiessen_catchments", 
          output="thiessen_catchments_polygon", 
          type="area")
# execGRASS("r.terraflow", 
#           elevation="distance_raster", 
#           direction="flowdir", 
#           flags = c("overwrite", "s"))
execGRASS("r.water.outlet", 
          input="drainage_direction", 
          output="watersheds", 
          coordinates=c(4350255, 3292290), 
          flags = c("overwrite"))
execGRASS("r.to.vect", 
          input="watersheds", 
          output="watersheds_polygon", 
          type="area",
          flags = c("overwrite"))

execGRASS("r.water.outlet", 
          input="drainage_direction", 
          output="watersheds", 
          coordinates=c(4339077, 3296370), 
          flags = c("overwrite"))
execGRASS("r.to.vect", 
          input="watersheds", 
          output="watersheds_polygon_add", 
          type="area",
          flags = c("overwrite"))
execGRASS("v.overlay", 
          ainput="watersheds_polygon", 
          binput="watersheds_polygon_add", 
          output="watersheds_union", 
          operator="or",
          flags = c("overwrite"))
thiessen_catchments <- readVECT("watersheds_union")
thiessen_catchments <- readVECT("watersheds_polygon")
thiessen_catchments <- readRAST("thiessen_catchments")
flowdir <- readRAST("flowdir") %>% raster::raster()

flowdir %>% plot()
thiessen_catchments %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf()

distance_raster %>% 
  plot()

