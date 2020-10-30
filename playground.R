library(corrr)
library(sf)
library(leaflet)
library(leafgl)
library(raster)
library(rgdal)
library(targets)
library(tidyverse)

studyarea <- tar_read(studyarea)
studyarea_subset_plots <- tar_read(studyarea_subset_plots)
grid_studyarea <- tar_read(grid_studyarea)
grid_studyarea_centroids <- tar_read(grid_studyarea_centroids)
before_preprocessing_overview_map <- tar_read(before_preprocessing_overview_map)
back_vals_2005 <- tar_read(back_vals_2005)
back_vals_2005_clean_sf <- tar_read(back_vals_2005_clean_sf)
back_vals_2005_filter_sf <- tar_read(back_vals_2005_filter_sf)
feature_lulc_raw <- tar_read(feature_lulc_raw)
feature_lulc <- tar_read(feature_lulc)
feature_gw_recharge_raw <- tar_read(feature_gw_recharge_raw)
feature_gw_recharge <- tar_read(feature_gw_recharge)
feature_hydrounits_raw <- tar_read(feature_hydrounits_raw)
feature_hydrounits <- tar_read(feature_hydrounits)
feature_geology_raw <- tar_read(feature_geology_raw)
feature_geology <- tar_read(feature_geology)
feature_soilunits_raw <- tar_read(feature_soilunits_raw)
feature_soilunits <- tar_read(feature_soilunits)
feature_hydrogeology_raw <- tar_read(feature_hydrogeology_raw)
feature_hydrogeology_kf <- tar_read(feature_hydrogeology_kf)
feature_seepage_raw <- tar_read(feature_seepage_raw)
feature_seepage <- tar_read(feature_seepage)
feature_precipitation <- tar_read(feature_precipitation)
feature_humus_raw <- tar_read(feature_humus_raw)
influential_polygons <- tar_read(influential_polygons)
back_vals_2005_influential_polygons <- tar_read(back_vals_2005_influential_polygons)
interactive_schematic_lulc_feature_extraction_map <- tar_read(interactive_schematic_lulc_feature_extraction_map)
interactive_schematic_hydrounits_feature_extraction_map <- tar_read(interactive_schematic_hydrounits_feature_extraction_map)
interactive_correlation_plot <- tar_read(interactive_correlation_plot)
schematic_feature_extraction_plot <- tar_read(schematic_feature_extraction_plot)
all_features_model <- tar_read(all_features_model)
all_data_model <- tar_read(all_data_model)
all_data_model_wo_orientations <- tar_read(all_data_model_wo_orientations)


tar_read(feature_temperature_raw) %>% 
  plot()

studyarea_subset_plots %>% 
  st_buffer(dist = -5*INFLUENTIAL_RADIUS) %>%
  ggplot() + 
    geom_sf()


back_vals_2005 %>% 
  select(contains("ca")) %>% 
  ggplot() +
  aes(CA_mgL) +
  geom_histogram() +
  scale_y_log10()


back_vals_2005 %>% 
  select(contains("na_")) %>% 
  filter(NA_mgL < 0)


tar_meta() %>% 
  arrange(-seconds) %>% 
  select(name, seconds)

"feature_soilunits" %>% 
  tar_read("feature_soilunits")


result <- all_data_model %>% 
  pivot_longer(cols = -c("station_id", "ca_mg_l", "x_coord", "y_coord"),
               names_to = c("name_feature", "name_class", "orientation"),
               names_pattern = "(.*)_(.*)_(.*)") %>% 
  group_by(name_feature, orientation) %>% 
  summarise(number_na = sum(is.na(value)))


make_interactive_schematic_map(
  feature_lulc_raw,
  back_vals_2005_influential_polygons,
  back_vals_2005_filter_sf,
  studyarea_subset_plots,
  "lulc_class"
)











schematic_feature_extraction_plot
make_schematic_feature_extraction_plot(
  feature_lulc_raw,
  back_vals_2005_filter_sf,
  influential_polygons
)


make_interactive_schematic_map(
    feature_hydrounits_raw,
    grid_studyarea_centroids,
    back_vals_2005_influential_polygons,
    back_vals_2005_filter_sf,
    studyarea_subset_plots,
    "r_name"
)





studyarea %>% 
  st_geometry() %>% 
  ggplot() +
  geom_sf()

studyarea %>% 
  st_geometry()

here::here("J:/NUTZER/Noelscher.M/Studierende/Daten/digital_elevation_model/germany/time_invariant/gridded_grid/eudem_slope/data/EUD_CP-SLOP_4500035000-AA/EUD_CP-SLOP_4500035000-AA.tif")

here::set_here("J:/NUTZER/Noelscher.M/Studierende/Daten/")

here::dr_here()

here::here("EUD_CP-SLOP_4500035000-AA.tif")

studyarea %>% 
  ggplot() +
  geom_sf()

test <- "J:/NUTZER/Noelscher.M/Studierende/Daten/digital_elevation_model/germany/time_invariant/gridded_grid/eudem_slope/data/EUD_CP-SLOP_4500035000-AA/EUD_CP-SLOP_4500035000-AA.tif" %>% 
  raster()

test <- acos(test/250)*180/pi

test %>% 
  plot()


tibble(x = 0L:250L) %>% 
  mutate(y = acos(x/250)*180/pi) %>% 
  ggplot(aes(x, y)) +
  geom_line()

all_data_model %>% 
  select(ca_mg_l, contains("_45"), -contains(c("soil", "geology"))) %>% 
  cor() %>% 
  round(1) %>% 
  ggcorrplot::ggcorrplot()


feature_hydrogeology_raw %>% 
  select(kf_bez) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  distinct(kf_bez)



feature_humus_raw %>% 
  st_drop_geometry() %>%
  distinct(gehalt)


feature_soilunits %>% 
  mutate(test = 0) %>% 
  # select_if(~ !is.numeric(.) || sum(.) != 0)
  select(where(~ !is.numeric(.) || sum(.) != 0))

test <- "J:/NUTZER/Noelscher.M/Studierende/Daten/hydrogeological_map/germany/time_invariant/shape/huek200_bgr_topocheck_test/data/huek200_topo_test.shp" %>% 
  st_read() %>% 
  st_transform(crs = CRS_REFERENCE)

test %>% 
  st_make_valid() %>% 
  st_intersection(st_buffer(studyarea, STUDYAREA_BUFFER)) %>% 
  janitor::clean_names() %>% 
  st_sf()


feature_soilunits_raw %>% 
  ggplot() +
  geom_sf(aes(fill = symbol))

make_interactive_schematic_map(
  feature_soilunits_raw,
  grid_studyarea_centroids,
  back_vals_2005_influential_polygons,
  back_vals_2005_filter_sf,
  studyarea,
  "symbol"
)

feature_hydrounits_raw %>% 
  ggplot(aes(fill = r_name)) +
  geom_sf()

feature_hydrounits %>% 
  select(where(is.numeric)) %>%
  mutate(sum = rowSums(.)) %>% 
  select(sum) %>% 
  filter(sum < 8)

rasf::is_raster(feature_gw_recharge)

back_vals_2005_filter_sf %>% 
  # filter(al_mg_l == "NaN")
  mutate(across(where(is.numeric), na_if, "NaN"))

back_vals_2005_clean_sf %>% 
  filter_background_values(studyarea)


back_vals_2005 %>% 
  mutate(station_id = str_c(!!sym(STATION_ID[1]), !!sym(STATION_ID[2]), sep = "_")) %>% 
  select(contains("id"))
  # janitor::get_dupes(stprj_id, stat_id)
  distinct(stprj_id, stat_id)

  df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
  df %>% replace_na(y, "unknown")
  
  
back_vals_2005 %>% 
  select(Lage) %>% 
  group_by(Lage) %>% 
  count()

back_vals_2005_clean_sf %>% 
  as_tibble() %>% 
  janitor::get_dupes(station_id) %>% 
  distinct(station_id)
  
back_vals_2005_clean_sf %>% 
  pull(stprj_stat_smp_id) %>% 
  unique() %>% 
  length()

back_vals_2005_clean_sf %>% 
  st_drop_geometry() %>% 
  mlr::summarizeColumns()

test <- back_vals_2005_clean_sf %>% 
  st_drop_geometry() %>% 
  group_by(station_id) %>% 
  distinct(sample_depth) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n)


tibble(date = seq.Date(as.Date("1900-01-01"), length.out = 5, by = "days")) %>% 
  sample_n(n()) %>% 
  add_row(date = NA,
          .before = 3) %>% 
  arrange(desc(date))



back_vals_2005_clean_sf %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  distinct(X, Y)


back_vals_2005_clean_sf %>% 
  filter(station_id == 55)

tar_visnetwork()

back_vals_2005 %>% 
  janitor::clean_names() %>% 
  select(ag_mg_l:zn_mg_l) %>% 
  names() %>% 
  paste0(collapse = "', '")


back_vals_2005 %>% 
  distinct(x, y)

back_vals_2005 %>% 
  distinct(s_name)


filepath <- "J:/NUTZER/Noelscher.M/Studierende/Daten/soil_humus_content/germany/time_invariant/shape/humus_content/data/humus1000_ob_v20.shp"


test_humus <- filepath %>%
  read_and_intersect_feature_vector(studyarea)


test_humus %>% 
  mutate(gehalt = str_replace_all(gehalt, "ä", "ae")) %>%
  st_write("J:/NUTZER/Noelscher.M/Studierende/Daten/soil_humus_content/germany/time_invariant/shape/humus_content/data/humus1000_ob_v20_no_umlaute.shp")

test_humus %>% mutate(gehalt = as.character(gehalt)) %>% 
  mutate(gehalt = str_replace_all(gehalt, "Gewaesserflaechen", "0")) %>% 
  mutate(gehalt = str_replace_all(gehalt, "nicht bestimmt", "0")) %>% 
  mutate(gehalt = str_replace_all(gehalt, "%|<| ", "")) %>%
  mutate(gehalt = str_replace_all(gehalt, ">30", "30-100")) %>% 
  mutate(gehalt = str_replace_all(gehalt, ",", ".")) %>% 
  mutate(gehalt = str_replace_all(gehalt, "Abbauflächen|Gewaesserflaechen|nicht bestimmt|Stadtkernbereiche|Wattflächen", "0")) %>% 
  rowwise() %>% 
  mutate(gehalt = str_c(word(gehalt, 1, sep = "-"), "+abs(", gehalt, ")/2")) %>%
  mutate(gehalt = eval(parse(text = gehalt))) %>%
  ungroup() %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  distinct(gehalt)


"J:/NUTZER/Noelscher.M/Studierende/Daten/groundwater_recharge/germany/nearly_time_invariant/raster/bgr/data/GWN1000__3034_v1_raster1.tif" %>% 
  raster::raster() %>% 
  raster::projectRaster(crs = CRS(str_c("EPSG:", CRS_REFERENCE)))

INFLUENTIAL_ORIENTATIONS %>% 
  make_multiple_circle_segments(sf_point, .) %>% 
  ggplot() +
  geom_sf(fill = "white",
          alpha = .6) +
  facet_wrap(~orientation)





back_vals_2005_filter_sf %>%
  sample_n(5) %>% 
  translate_multiple_influential_polygons(influential_polygons, .) %>% 
  ggplot() +
  geom_sf(aes(fill = orientation),
          colour = NA) +
  geom_sf(data = studyarea,
          fill = NA) +
  facet_wrap(~ orientation)



test <- "J:/NUTZER/Noelscher.M/Studierende/Daten/groundwater_recharge/germany/nearly_time_invariant/raster/bgr/data/GWN1000__3034_v1_raster1.tif" %>%
  raster::raster() %>%
  raster::projectRaster(crs = crs(studyarea))

test_test <- test > -Inf

test_test <- test_test %>% 
  rasterToPolygons(dissolve = TRUE)

a <- test_test %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = test_test_b,
          colour = "red")

a %>% 
  plotly::ggplotly()

test_test_b <-
  feature_humus_raw %>% 
  st_union()

test_test %>% 
  st_as_sf() %>% 
  st_intersection(test_test_b)
