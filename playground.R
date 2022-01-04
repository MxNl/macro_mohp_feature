river_network <- 
  LINES_STUDYAREA %>% 
  # composite_name(4) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf()

test <- all_object_ids %>% 
  filter(!(object_id %in% major_path_ids)) %>% 
  pull(object_id)


river_network %>% 
  filter(object_id %in% bracket_start_ids) %>% 
  st_write(here::here("output_data", "river_network_bracketstarts3.shp"))

river_network

tar_read(major_path_ids)
tar_read(distinct_streamorders_in_riverbasins) %>% 
  arrange(river_basin_name, strahler)


test_vec <- 1:6 %>% rev()
test_vec_ref <- c(3,4,6)

test_vec[!(test_vec %in% test_vec_ref)]
setdiff(test_vec, test_vec_ref)


bracket_start_ids <- get_bracket_start_ids(LINES_STUDYAREA, major_path_ids)




river_test <- tar_read(rivernetworks_merged_per_streamorder)
















DBI::dbDisconnect(connect_to_database())




table_name <- LINES_STUDYAREA
# major_path_ids <- tar_read(major_path_ids)
major_path_ids <- tar_read(river_networks) %>% st_drop_geometry() %>% distinct(nextdownid) %>% as_vector()
bracket_start_ids <- tar_read(bracket_start_ids)
distinct_streamorders_in_riverbasins <- tar_read(distinct_streamorders_in_riverbasins)
streamorder <- 2
river_basin_name <- "rhine"

river_network %>% 
  ggplot() +
  geom_sf()

river_network_with_minor_paths %>% 
  ggplot() +
  geom_sf()

river_network_without_minor_paths %>% 
  ggplot() +
  geom_sf()

river_network_with_minor_paths %>%
  st_intersection(river_network_without_minor_paths) %>% 
  # filter(st_equals(river_network_with_minor_paths, river_network_without_minor_paths, sparse = FALSE)[, 1] |
  #          st_overlaps(river_network_with_minor_paths, river_network_without_minor_paths, sparse = FALSE)[, 1]) %>% 
  ggplot() +
  geom_sf()

difference <- st_difference(summarise(river_network_without_minor_paths), 
                            summarise(river_network_with_minor_paths)) %>% 
  mutate(streamorder = streamorder)

river_network_with_minor_paths %>% 
  bind_rows(difference) %>% 
  ggplot() +
  geom_sf()

river_network_with_minor_paths %>%
  filter((st_equals(river_network_with_minor_paths, 
                   river_network_without_minor_paths, sparse = TRUE) %>% 
           map_lgl(is_empty))) %>% 
  ggplot() +
  geom_sf()


st_overlaps(river_network_with_minor_paths, 
          river_network_without_minor_paths, sparse = TRUE) %>%
  map_lgl(is_empty)
  unlist()
