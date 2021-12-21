river_network <- 
  LINES_MERGED %>% 
  composite_name(4) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf()


river_network %>% 
  st_write(here::here("output_data", "river_network_ho4.shp"))
