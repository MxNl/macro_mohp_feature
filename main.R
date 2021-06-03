library(tidyverse)
library(sf)
library(furrr)
message('Hi!')
message(getwd())
system('whoami')
system('ls -l')

writeLines('Yo!', 'docker-mount/test.txt')

writeLines('Yo!', 'docker-mount/yeehaw.txt')

plan(multisession, workers = 8)

partitions <- 5

df <- tibble(
  a = 1:30e6,
  b = rnorm(length(a))
)

df %>% 
  mutate(partition_id = row_number() %% 5) %>% 
  split(.[["partition_id"]]) %>% 
  future_map_dfr(~{
    .x %>% 
      rowwise() %>% 
      mutate(b = list(tibble(a = b * 2))) %>%
      ungroup()
  })
