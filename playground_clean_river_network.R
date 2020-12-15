table_a <- 
  sf_lines %>% 
  group_by(strahler) %>% 
  summarise() %>% 
  mutate(id = 1:n())

table_b <- table_a

splitpoints <- 
  table_a %>% 
  st_join(table_b, .predicate = st_touches) %>% 
  filter(strahler.x >= strahler.y)

table_a %>% 
  lwgeom::st_split(splitpoints)

segment_colours <- 
  splitpoints %>%
  # distinct(nearest_feature) %>% 
  nrow() %>% 
  hues::iwanthue(lmin = 40,
                 cmax = 70)

plotti <- splitpoints %>%
  mutate(id = as.character(1:n())) %>% 
  ggplot() +
  geom_sf(aes(colour = id)) +
  geom_sf(data = start_points) +
  geom_sf(data = end_points) +
  scale_color_manual(values = segment_colours)

plotti %>% 
  plotly::ggplotly()

start_points <- 
  splitpoints %>% 
  lwgeom::st_startpoint()

end_points <- 
  splitpoints %>% 
  lwgeom::st_endpoint()





































river_network <- tar_read(river_network_by_streamorder)[[1]]





segments_to_merge <- 
  list(
    seq_along(adjacent_segments_list),
    adjacent_segments_list,
    adjacent_strahler_list,
    segment_strahler_list
  ) %>% 
  pmap(determine_segments_to_merge) %>% 
  plyr::ldply(rbind) %>% 
  as_tibble() %>% 
  mutate(across(everything(), ~replace(., . == 0, NA))) %>% 
  mutate(index = 1:nrow(.), .before = 1)

segments_to_merge <-
  segments_to_merge %>% 
  sparsewide_denselong_df() %>% 
  distinct_and_clean_segment_pairs()

segments_to_merge_b <- 
  segments_to_merge %>% 
  set_names(c("id_b", "id_a"))

segments_to_merge %>% 
  left_join(segments_to_merge_b, by = "id_a") %>% 
  rename(id_b = id_b.x) %>% 
  left_join(segments_to_merge_b, by = "id_b") %>% 
  View()

index_filter <- 
  segments_to_merge %>% 
  pull(index)

segments_to_merge <- 
  segments_to_merge %>% 
  select(-index)

segments_to_merge %>% 
  plyr::match_df(segments_to_merge)

segments_to_merge %>% 
  lump()





add_index <- 
  function(vector, index) {
    vector <- 
      vector %>% 
      c(index, .)
  }


sparsewide_denselong_df <-
  function(sparse_df) {
    sparse_df %>% 
      filter_at(any_vars(!is.na(.)), .vars = names(sparse_df)[-1]) %>%
      pivot_longer(cols = -"index") %>% 
      select(-name) %>% 
      drop_na(everything())
  }


distinct_and_clean_segment_pairs <- 
  function(segment_pairs_df){
    segment_pairs_df %>% 
      filter(index !=value) %>% 
      mutate(row_number = 1:n()) %>% 
      pivot_longer(cols = -"row_number") %>% 
      select(-name) %>% 
      group_by(row_number) %>% 
      dplyr::arrange(value, .by_group = TRUE) %>% 
      ungroup() %>% 
      mutate(name = rep(c("id_a", "id_b"), n()/2), .before = 2) %>% 
      pivot_wider(id_cols = "row_number") %>% 
      select(-row_number) %>% 
      distinct(id_a, id_b)
  }



