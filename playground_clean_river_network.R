river_network <- tar_read(river_network_by_streamorder)[[1]]

get_adjacent_segments <- 
  function(sf_lines){
    ###### Test
    sf_lines <- river_network
    ###
    
    adjacent_segments_list <- 
      sf_lines %>% 
      st_touches()
    
    adjacent_segments_list <- 
      adjacent_segments_list %>% 
      map(as.vector) %>% 
      # imap(~c(.x, .y)) %>% 
      map(sort)
      # map(as.character)
      # map(str_c, collapse = "-") %>% 
      # unlist()
    
    # sf_lines <- 
    #   sf_lines %>% 
    #   mutate(adja_segments = adjacent_segments_list, .before = 2)
    
    adjacent_strahler_list <- 
      adjacent_segments_list %>% 
      map(~extract_strahler_by_index(sf_lines, .))
    
    segment_strahler_list <- 
      sf_lines %>% 
      rowwise() %>% 
      group_split() %>% 
      imap(~extract_strahler_by_index(sf_lines, .y))
    
    test <- 
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
    
    test <-
      test %>% 
      filter_at(any_vars(!is.na(.)), .vars = names(test)[-1]) %>%
      pivot_longer(cols = -"index") %>% 
      select(-name) %>% 
      drop_na(everything()) %>% 
      mutate()
      filter(index !=value)
    
    
    index_filter <- 
      test %>% 
      pull(index)
    
    test <- 
      test %>% 
      select(-index)
    
    test %>% 
      lump() %>% 
      
      
  }


extract_strahler_by_index <- 
  function(sf_lines, index_vector) {
    sf_lines %>% 
      as_tibble() %>% 
      slice(index_vector) %>% 
      pull(strahler)
  }

determine_segments_to_merge <-
  function(index_vector,
           adjacent_segments_list,
           adjacent_strahler,
           segment_strahler) {
    ##### Test
    # index <- sample(1:162, 1)
    # adjacent_strahler <- adjacent_strahler_list[[index]]
    # segment_strahler <- segment_strahler_list[[index]]
    ###

    if ((segment_strahler %>% is_empty())){
      FALSE
    }
    else if (segment_strahler == max(adjacent_strahler, na.rm = TRUE)) {
      adjacent_segments_list %>%
        # pluck(index) %>%
        magrittr::extract(adjacent_strahler == segment_strahler) %>%
        c(index_vector) %>%
        sort()
    } else {
      FALSE
    }
  }


linked_rows <- function(data){
  ## helper function
  ## returns a _function_ to compare two rows of data
  ##  based on group membership.
  
  ## Use Vectorize so it works even on vectors of indices
  Vectorize(function(i, j) {
    ## numeric: 1= i and j have overlapping group membership
    common <- vapply(names(data), function(name)
      data[i, name] == data[j, name],
      FUN.VALUE=FALSE)
    as.numeric(any(common))
  })
}

lump_links <- function(A) {
  A <- A %*% A
  A[A > 0] <- 1
  A
}

lump <- function(df) {
  rows <- 1:nrow(df)
  A <- outer(rows, rows, linked_rows(df))
  
  oldA <- 0
  while (any(oldA != A)) {
    oldA <- A
    A <- lump_links(A)
  }
  df$combinedGrp <- cutree(hclust(as.dist(1 - A)), h = 0)
  df
}
