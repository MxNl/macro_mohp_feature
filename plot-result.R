library(mapview)
library(targets)
library(tidyverse)
library(sf)
result <- tar_read(nearest_neighbours)

grid <- result %>% 
  select(-river_network_by_streamorder_geometry) %>% 
  st_as_sf(sf_column_name = 'grid_geometry')

linestrings <- result %>% 
  select(-grid_geometry) %>% 
  distinct(river_network_by_streamorder_geometry) %>% 
  st_as_sf(sf_column_name = 'river_network_by_streamorder_geometry')

plt_grid <- mapview::mapview(grid, zcol='river_network_by_streamorder_feature_id', alpha = 0)
plt_ls <- mapview::mapview(linestrings)

plt_grid + plt_ls

test <- tar_read(base_grid_centroids)

mapview(test)

as_tibble(test) %>% anti_join(as_tibble(grid), by = c('id'= 'grid_id'))

# WITH polygons AS (
#   SELECT nn.river_network_by_streamorder_feature_id, ST_ExteriorRing(ST_Union(gp.geometry)) AS geometry
#   FROM
#   grid_polygons gp
#   INNER JOIN
#   nearest_neighbours_6 nn
#   ON gp.id = nn.grid_id
#   GROUP BY
#   1
# )
# select *, GeometryType(geometry) AS geom_type from polygons
