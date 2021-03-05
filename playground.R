library(targets)
library(raster)
library(sf)
library(lwgeom)
library(stars)
library(furrr)
library(janitor)
library(patchwork)
library(assertr)
library(tarchetypes)
library(mapview)
library(tidyverse)
library(here)


tar_read(filepath_canals_to_reclassify)
tar_read(river_networks_clip)
tar_read(features_ids_to_reclassify)
studyarea_subset_plots <- tar_read(studyarea_subset_plots)
studyarea <- tar_read(studyarea)
river_networks_clip <- tar_read(river_networks_clip)
river_networks_clean <- tar_read(river_networks_clean)
river_networks_split <- tar_read(river_networks_split)
river_networks_strahler_merge <- tar_read(river_networks_strahler_merge)
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
tar_read(plot_subset_all_steps)
tar_read(test_processed_river_network_plot)
tar_read(test_catchments_plot)
tar_read(grid_lateral_position)
tar_read(river_networks_clip)
tar_read(river_networks_imputed_streamorder_canals_as_1)








library(leaflet)
library(leafgl)

test_lines <- 
  LINES_MERGED %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf()


test_lines <- 
  test_lines %>% 
  st_transform(4326) %>% 
  st_cast("LINESTRING") %>% 
  mutate(feature_id = as.character(feature_id))

# tar_read(river_networks_clean) %>% 
#   st_write("../lines_before_merge.shp")

options(viewer = NULL)
leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  addGlPolylines(data = test_lines)




selected_studyarea <- tar_read(selected_studyarea) %>% 
  mutate(id = 1L)

rasterized <- 
  selected_studyarea %>% 
  fasterize::raster(resolution = c(25,25)) %>% 
  setValues(1)

rasterized_plus <- 
  selected_studyarea %>% 
  fasterize::fasterize(rasterized)

rasterized_plus %>% plot()
rasterized %>% plot()

asstars <- 
  rasterized %>% 
  st_as_stars()
connection

tictoc::tic()
grid_test <- 
  rasterized %>% 
  st_as_stars() %>% 
  st_transform(CRS_REFERENCE) %>% 
  st_as_sf()
  # select(-values) %>%
  # st_transform(CRS_REFERENCE) 
  # filter_intersecting_features(selected_studyarea)
  # write_to_table("testtest")
tictoc::toc()

grid_test %>% 
  mapview()

grid_test %>% st_crs()
selected_studyarea %>% st_crs()

"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

tictoc::tic()
assf <- asstars %>% 
  st_as_sf(as_points = FALSE)
tictoc::toc()

assf

assf %>% 
  add_feature_index_column()

write_selected_studyarea(
  tar_read(selected_studyarea),
  SELECTED_STUDYAREA_TABLE
)
table <- SELECTED_STUDYAREA_TABLE
connection <- connect_to_database()
db_execute(glue::glue("DROP TABLE IF EXISTS {table};"), connection = connection)
st_write(tar_read(selected_studyarea), dsn = connection, layer = table, append = FALSE)


write_as_lines_to_db(
  tar_read(river_networks_clean),
  LINES_CLEAN
)

connection <- connect_to_database()
table <- LINES_CLEAN

db_execute(glue::glue("DROP TABLE IF EXISTS {table};"), connection = connection)

tar_read(river_networks_clean) %>% 
  st_cast("LINESTRING") %>%
  write_to_table(table_name_destination)

test <- tar_read(river_networks_files) %>% 
  chuck(1) %>% 
  map(
    STREAM_TYPE_TO_INCLUDE,
    read_sf,
    dsn = .
  ) %>%
  reduce(bind_rows) %>%
  st_zm() %>%
  rename(geometry = Shape) %>% 
  clean_names() %>%
  # select(dfdd, inspire_id, strahler) %>%
  transform_crs_if_required()

test %>% 
  as_tibble() %>% 
  filter(hyp %in% 1:3) %>% 
  filter(is.na(nextdownid))

test <- tar_read(river_networks) %>% 
  as_tibble()

test %>% 
  filter(dfdd == "BH140" & strahler == -9999)



tar_read(lateral_position_stream_divide_distance) %>% 
  chuck(4) %>% 
  select(all_of("lateral_position")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()

NN_GRID_RIVERS_TABLE %>% 
  composite_name(tar_read(streamorders) %>% chuck(1)) %>% 
  get_table_from_postgress() %>%
  query_result_as_sf() %>% 
  select(all_of("distance_meters")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()

NN_GRID_CATCHMENTS_TABLE %>% 
  composite_name(tar_read(streamorders) %>% chuck(4)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  select(all_of("distance_meters")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()

tar_read(river_network_by_streamorder) %>% 
  chuck(3) %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  mutate(strahler = as.character(strahler)) %>% 
  ggplot() +geom_sf()

streamorders <- tar_read(streamorders)

thiessen_catchments <- 
  streamorders %>% 
  map(~ composite_name(THIESSEN_CATCHMENTS_TABLE, .x)) %>% 
  map(get_table_from_postgress) %>% 
  map(query_result_as_sf)

river_network_by_streamorder <- tar_read(river_network_by_streamorder)
river_networks_clean <- tar_read(river_networks_clean)
river_networks_strahler_merge <- tar_read(river_networks_strahler_merge)
plot1 <- river_networks_clean %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)
plot3 <- river_networks_clean %>% 
  mutate(strahler = as.character(strahler)) %>% 
  plot_lines_coloured_by_categorical_attribute(strahler)
plot2 <- river_networks_strahler_merge %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)

plot1 | plot3 | plot2


study_area <- tar_read(selected_studyarea)

plot_test_catchments(river_network_by_streamorder, 
                     thiessen_catchments, 
                     streamorders, 
                     study_area)

rivers_plot <- 
  LINES_BY_STREAMORDER %>% 
  composite_name(tar_read(streamorders) %>% chuck(3)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  mutate(strahler = as.character(strahler))

{rivers_plot %>% 
  filter(feature_id %in% c("96", "97")) %>% 
  mutate(feature_id = as.integer(feature_id)) %>% 
  st_cast("MULTIPOINT") %>% 
  ggplot(aes(colour = feature_id)) +
  geom_sf() +
  facet_wrap(~feature_id, ncol = 1)} %>% plotly::ggplotly()


segment_six <- 
  rivers_plot %>% 
  filter(feature_id %in% c("96"))

segment_seven <- 
  rivers_plot %>% 
  filter(feature_id %in% c("97"))

unioned <- 
  st_union(segment_six, segment_seven)

st_difference(unioned, st_intersection(segment_six, segment_seven)) %>% 
  st_cast("LINESTRING") %>% 
  select(contains("feature_id"), geometry) %>% 
  st_contains(segment_seven, ., sparse= FALSE)
  ggplot() + geom_sf()
  
  
tar_read(river_networks_clean) %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>% 
  plotly::ggplotly()
  
st_overlaps(segment_seven, segment_six, sparse = FALSE)
st_overlaps(segment_six, segment_seven, sparse = FALSE)

plotlist <- list()
for(i in tar_read(streamorders)) {

  rivers_plot <- 
  LINES_BY_STREAMORDER %>% 
  composite_name(tar_read(streamorders) %>% chuck(i)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  mutate(strahler = as.character(strahler))
# 
# rivers_plot %>% 
#   mutate(length = st_length(geometry)) %>% 
#   arrange(length) %>% 
#   slice(1:2) %>% 
#   ggplot() +geom_sf()


plotlist[[i]] <- 
  THIESSEN_CATCHMENTS_TABLE %>% 
  composite_name(tar_read(streamorders) %>% chuck(i)) %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf() %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = rivers_plot, aes(colour = feature_id)) +
  theme(legend.position = "none")
}

plotlist[[3]] %>% plotly::ggplotly()

sequential_nearest_neighbours_with_maxdist <- 
  function(
    table_name_destination = "nn_test_maxdist",
    left_table,
    left_columns,
    right_table,
    right_columns = NULL,
    streamorders = NULL,
    n = 1,
    depends_on
  ) {
    test <- FALSE
    if(test){
      table_name_destination <- "nn_test_maxdist"
      left_table <- GRID_CENTROIDS
      left_columns <- c("grid_id", "geometry")
      right_table <- composite_name(LINES_BY_STREAMORDER, streamorders)
      right_columns <- c("feature_id", "strahler", "stream_order_id")
      streamorders <- 1:6
      n <- 1
    }
    
    max_distance <- FALSE
    
    for (i in sort(streamorders, decreasing = TRUE)) {
      print(i)
      
      nearest_neighbours_between(
        table_name_destination,
        left_table,
        right_table,
        left_columns,
        right_columns,
        stream_order_id = i,
        max_distance = max_distance
      )
    }
    
  }

library(leaflet)
library(leafgl)
studyarea_test <- tar_read(selected_studyarea)

raster_test <- 
  studyarea_test %>% 
  raster::raster(crs = 3035, resolution = 100)

points <- 
  raster_test %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("x", "y"))

st_crs(points) <- 3035


points <- 
  points %>% 
  filter_intersecting_features(studyarea_test) %>% 
  st_transform(4326)


studyarea_test <- 
  studyarea_test %>% 
  st_transform(CRS_LEAFLET)

options(viewer = NULL)
leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  # addPolylines(data = studyarea_test) %>%
  addGlPoints(data = points)

"
CREATE TABLE raster_table_id_1 AS (
	WITH raster AS(
	SELECT
		lateral_position,
		geometry
	FROM lateral_position_stream_divide_distance_id_1
	)
	SELECT
	lateral_position,
	ST_AsRaster(geometry, 200, 200)
	FROM raster
	);

--\\lo_export 147303 '/tmp/myraster.tiff'

DROP TABLE IF EXISTS tmp_out ;

CREATE TABLE tmp_out AS
SELECT lo_from_bytea(0,
       ST_AsGDALRaster(ST_UNiON(st_asraster), 'GTIFF')
        ) AS loid
  FROM raster_table_id_1;

SELECT lo_export(loid, 'D:\\Tmp\\test.tiff')
   FROM tmp_out;

SELECT lo_unlink(loid)
  FROM tmp_out;
"




library(tidyverse)
library(sf)
library(mapview)

convert_geometry <- function(x) as_tibble(x) %>% dplyr::mutate(dplyr::across(where(is_pq_geometry), from_wkb))

connection <- connect_to_database()

tar_read(river_networks) %>% 
  mapview(zcol='strahler', lwd = 6)


get_split_points <- 
  function(river_networks) {
    endpoints <- 
      river_networks %>% 
      get_start_and_endpoints()
    
    # river_networks_filter <- 
    #   river_networks %>% 
    #   filter_intersecting_features(endpoints)
    # 
    endpoints %>%
      add_feature_index_column(column_name = "endpoint_id") %>% 
      st_join(river_networks) %>% 
      select(-feature_id) %>% 
      group_by(endpoint_id) %>%
      filter(n() > 2 & 
               length(unique(strahler)) > 1) %>% 
      group_by(endpoint_id, strahler) %>% 
      mutate(n_strahler = n()) %>% 
      group_by(endpoint_id) %>% 
      filter(strahler == min(strahler) & n_strahler > 1) %>% 
      slice(1) %>% 
      select(geometry) %>%
      ungroup()
  }

get_start_and_endpoints <- 
  function(river_networks) {
    
    endpoints_one_side <-
      river_networks %>%
      st_endpoint() %>%
      st_as_sf()
    
    endpoints_other_side <-
      river_networks %>%
      st_startpoint() %>%
      st_as_sf()
    
    endpoints_one_side %>%
      bind_rows(endpoints_other_side) %>% 
      distinct(.keep_all = TRUE) %>%
      rename(geometry = x)
  }


DBI::dbGetQuery(conn = connection, "
SELECT
  strahler,
  ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry,
  GeometryType(ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry)))) 
FROM lines_raw
GROUP BY strahler
") %>% 
  convert_geometry() %>% 
  print() %>% 
  st_as_sf() %>% 
  mapview(zcol='strahler', lwd = 6)

split_points <- 
  DBI::dbGetQuery(conn = connection, "
WITH collected AS (
  SELECT
    strahler,
    ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry,
    GeometryType(ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry)))) 
  FROM lines_clean
  GROUP BY strahler
      ), local_linestrings AS (
      	SELECT strahler, ST_LineMerge(geometry) AS geometry FROM collected
      ), local_linestrings_with_id AS (
      	SELECT row_number() OVER (ORDER BY strahler) AS id, * FROM local_linestrings
      )
      	SELECT
      	  l.id AS id,
      	  l.strahler AS strahler,
      	  ST_AsText(l.geometry) AS geometry,
      	  r.id AS r_id,
      	  r.strahler AS r_strahler,
      	  ST_AsText(r.geometry) AS r_geometry,
      	  ST_Intersection(l.geometry, r.geometry) AS common_geometry
      	FROM
      	  local_linestrings_with_id AS l
      	  CROSS JOIN local_linestrings_with_id AS r
      	  WHERE
      		(
      			l.strahler < r.strahler
      			AND
      			ST_Intersects(l.geometry, r.geometry)
      			AND
      			l.id != r.id
      		)
") %>% 
  convert_geometry() %>% 
  st_as_sf() %>% 
  st_cast("MULTIPOINT") %>% 
  mutate(id = as.character(id))

split_points <- 
  tar_read(river_networks_clean) %>% 
  get_split_points()


test_lines <- 
  DBI::dbGetQuery(conn = connection, "
WITH collected AS (
    SELECT strahler,
        ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry
    FROM lines_clean
    GROUP BY strahler
),
-- base table
clusters AS (
    SELECT row_number() OVER (
            ORDER BY strahler
        ) AS cluster_id,
        strahler,
        CASE
            WHEN GeometryType(ST_LineMerge(geometry)) = 'MULTILINESTRING' THEN true
            ELSE false
        END AS is_multilinestring,
        geometry
    FROM collected
),
-- processing for clusters that are multilinestrings and have splitpoints
cluster_lines_without_id AS (
    SELECT cluster_id,
        (ST_Dump(geometry)).geom AS geometry,
        strahler,
        is_multilinestring
    FROM clusters
),
cluster_lines as (
    SELECT row_number() OVER (
            ORDER BY is_multilinestring
        ) AS line_id,
        *
    FROM cluster_lines_without_id
),
split_points_single AS (
    SELECT DISTINCT l.cluster_id AS cluster_id,
        ST_Intersection(l.geometry, r.geometry) AS geometry
    FROM cluster_lines AS l
        CROSS JOIN cluster_lines AS r
    WHERE (
            l.strahler < r.strahler
            AND ST_Intersects(l.geometry, r.geometry)
            AND l.cluster_id != r.cluster_id
        )
),
--Test for multiple splitpoints per cluster id
split_points AS (
    SELECT
        cluster_id,
        ST_CollectionHomogenize(ST_Collect(geometry)) AS geometry
    FROM split_points_single
    GROUP BY cluster_id
),
lines_touching_splitpoints AS (
    SELECT cluster_lines.line_id,
        cluster_lines.cluster_id,
        cluster_lines.geometry
    FROM cluster_lines
        LEFT JOIN split_points USING (cluster_id)
    WHERE ST_Intersects(cluster_lines.geometry, split_points.geometry)
        AND is_multilinestring
),
lines_not_touching_splitpoints AS (
    SELECT l.line_id,
        l.cluster_id,
        l.geometry AS geometry,
        r.line_id as rid,
        r.cluster_id as r_cl
    FROM cluster_lines AS l
        LEFT JOIN lines_touching_splitpoints AS r ON l.line_id = r.line_id
    WHERE l.cluster_id IN (
            SELECT cluster_id
            FROM lines_touching_splitpoints
        )
        AND r.line_id IS NULL
),
lines_not_touching_splitpoints_clusters AS (
    SELECT cluster_id,
        ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry
    FROM lines_not_touching_splitpoints
    GROUP BY cluster_id
),
-- clusters that are multilinestrings and have splitpoints
multinelinestring_clusters_splitted AS (
    SELECT l.cluster_id,
        ST_CollectionHomogenize(ST_Collect(l.geometry, r.geometry)) AS geometry
    FROM lines_touching_splitpoints l
        INNER JOIN lines_not_touching_splitpoints_clusters r USING(cluster_id)
    WHERE ST_Intersects(l.geometry, r.geometry)
),
-- clusters that are single linestrings
linestrings_clusters_splitted AS (
    SELECT l.cluster_id,
        (ST_Dump(ST_Split(ST_LineMerge(l.geometry), r.geometry))).geom AS geometry
    FROM clusters l
        INNER JOIN split_points r USING(cluster_id)
    WHERE NOT l.is_multilinestring
),
-- clusters that are multilinestrings but do not have splitpoints
multinelinestring_clusters_without_splitpoints AS (
    select cluster_id,
        ST_Multi(ST_CollectionHomogenize(geometry)) AS geometry
    FROM clusters
    WHERE is_multilinestring
        AND cluster_id NOT IN (
            SELECT DISTINCT cluster_id
            FROM multinelinestring_clusters_splitted
        )
),
unioned_without_strahler AS (
    SELECT cluster_id,
        geometry
    FROM multinelinestring_clusters_splitted
    UNION ALL
    SELECT cluster_id,
        geometry
    FROM linestrings_clusters_splitted
    UNION ALL
    SELECT cluster_id,
        geometry
    FROM multinelinestring_clusters_without_splitpoints
),
unioned_without_feature_id AS (
SELECT
	l.cluster_id,
    r.strahler,
	l.geometry
FROM unioned_without_strahler l
    INNER JOIN (
        SELECT DISTINCT ON (cluster_id)
            cluster_id,
            strahler
        FROM clusters
        ORDER BY cluster_id
    ) r USING(cluster_id)
	),
	unioned_without_strahler_unique_cluster_ids AS (
		SELECT
			DISTINCT cluster_id
		FROM unioned_without_strahler
	),
	linestrings_without_splitpoint AS (
	SELECT
		l.cluster_id,
		strahler,
		geometry
	FROM clusters l
		LEFT JOIN unioned_without_strahler_unique_cluster_ids r ON l.cluster_id = r.cluster_id
    	WHERE r.cluster_id IS NULL
	), 
	linestrings_without_splitpoint_inserted AS (
	SELECT
		*
    FROM linestrings_without_splitpoint
    UNION ALL
    SELECT
		*
    FROM unioned_without_feature_id
	),
	linestrings_without_splitpoint_inserted_with_feature_id AS (
		SELECT
			row_number() OVER (
            	ORDER BY strahler
        	) AS feature_id,
			*
		FROM linestrings_without_splitpoint_inserted
	),
	-- next steps are needed for union of overlapping lines that occur due to multipoint splitting
	lines_overlapping AS (
		SELECT
			l.feature_id,
			l.cluster_id
		FROM linestrings_without_splitpoint_inserted_with_feature_id l
		LEFT JOIN linestrings_without_splitpoint_inserted_with_feature_id r
		ON ST_Overlaps(l.geometry, r.geometry)
		WHERE l.cluster_id = r.cluster_id
	), 
	overlap_column AS (
		SELECT 
			*,
			CASE
				WHEN feature_id IN (SELECT feature_id FROM lines_overlapping) THEN true
				ELSE false
			END AS lines_overlap
		FROM linestrings_without_splitpoint_inserted_with_feature_id
	),
	overlapping_lines_merged AS (
		SELECT
			cluster_id,
			strahler,
			ST_Union(geometry) AS geometry
		FROM overlap_column
		WHERE lines_overlap = true
		GROUP BY cluster_id, strahler
	),
	lines_non_overlapping_reunion AS (
		SELECT
			*
		FROM overlapping_lines_merged
		UNION ALL
		(SELECT 
			cluster_id,
			strahler,
			geometry
		FROM overlap_column
		WHERE lines_overlap = false)
	)
	SELECT
		row_number() OVER (
            ORDER BY strahler
        ) AS feature_id,
		strahler,
		ST_AsText(ST_Multi(ST_CollectionHomogenize(geometry))) AS geometry
	FROM lines_non_overlapping_reunion	
") %>% 
  as_tibble() %>%  
  st_as_sf(wkt = "geometry")
mutate(feature_id = as.integer(feature_id))

st_crs(test_lines) <- CRS_REFERENCE

test_lines  %>% 
  # print() %>%
  mapview(zcol='feature_id', lwd = 6)# +
# mapview(split_points, zcol = 'id')


"
WITH collected AS (
  SELECT
    strahler,
    ST_CollectionExtract(unnest(ST_ClusterIntersecting(geometry))) AS geometry
  FROM lines_clean
  GROUP BY strahler
      ), local_linestrings AS (
      	SELECT strahler, ST_LineMerge(geometry) AS geometry FROM collected
      ), local_linestrings_with_id AS (
      	SELECT row_number() OVER (ORDER BY strahler) AS id, * FROM local_linestrings
      ), local_linestrings_with_splitpoints AS (
      	SELECT
      	  l.id AS id,
      	  l.strahler AS strahler,
      	  l.geometry AS geometry,
      	  r.id AS r_id,
      	  r.strahler AS r_strahler,
      	  r.geometry AS r_geometry,
      	  ST_Intersection(l.geometry, r.geometry) AS common_geometry
      	FROM
      	  local_linestrings_with_id AS l
      	  CROSS JOIN local_linestrings_with_id AS r
      	  WHERE
      		(
      			l.strahler < r.strahler
      			AND
      			--ST_Intersects(l.geometry, r.geometry)
				l.id 
      			AND
      			l.id != r.id
      		)
      )
      	SELECT 
      		id AS old_id, 
      		strahler,
      			CASE WHEN ST_Equals(geometry, common_geometry) THEN 
      				ST_ForceCollection(geometry)
      			ELSE
      				ST_Split(geometry, common_geometry)
      			END AS geometry,
				GeometryType(CASE WHEN ST_Equals(geometry, common_geometry) THEN 
      				ST_ForceCollection(geometry)
      			ELSE
      				ST_Split(geometry, common_geometry)
      			END) AS type_geom,
		  CASE WHEN ST_Equals(geometry, common_geometry) THEN 
      				'ST_ForceCollection'
      			ELSE
      				'ST_Split'
      			END AS funtion
      	FROM local_linestrings_with_splitpoints
      
"





#TODO filter by streamorder
rivers_test <- tar_read(river_networks_only_connected)
streamorders <- tar_read(streamorders)
# rivers_merge_test <- 
#   get_table_from_postgress("merge_test") %>% 
#   query_result_as_sf() %>% 
#   add_feature_index_column()

# rivers_test %>% 
#   st_cast("MULTILINESTRING") %>% 
#   group_by(strahler) %>% 
#   summarise()

river_network <- rivers_test
streamorder <- 1
table_name_destination_prefix <- "river_networks_merge_single_streamorder"
table_name_read <- LINES_CONNECTED_ID



merge_connected_lines_by_streamorder <- 
  function(table_name_read,
           table_name_destination_prefix,
           streamorder) {
    
    table_name_destination <- 
      composite_name(table_name_destination_prefix, streamorder)
    
    run_query_connected_single_streamorder(
      table_name_read,
      table_name_destination,
      streamorder
    )
  }

run_query_connected_single_streamorder <- 
  function(table_name_read, 
           table_name_destination, streamorder) {
    query <- glue::glue("
      CREATE TABLE {table_name_destination} AS (
	      WITH single_streamorders AS (
	        SELECT
	          *
	        FROM {table_name_read}
	        WHERE strahler = {streamorder}
	      ), endpoints AS (
	        SELECT
	          ST_Collect(ST_StartPoint(geometry),
	          ST_EndPoint(geometry)) AS geometry
	        FROM single_streamorders
	      ), clusters AS (
	        SELECT
	          unnest(ST_ClusterWithin(geometry, 1e-8)) AS geometry
	        FROM endpoints
	      ), clusters_with_ids AS (
	        SELECT
	          row_number() OVER () AS connected_id,
	          ST_CollectionHomogenize(geometry) AS geometry
	        FROM clusters
	      )
      	SELECT
		      connected_id,
		      ST_Collect(single_streamorders.geometry) AS geometry
	      FROM
	        single_streamorders
	        LEFT JOIN
	        clusters_with_ids
	        ON ST_Intersects(single_streamorders.geometry, clusters_with_ids.geometry)
	      GROUP BY connected_id
	      )
    ")
    print(query)
    create_table(query, table_name_destination)
  }

streamorders %>% 
  map(~merge_connected_lines_by_streamorder(LINES_CONNECTED_ID, "river_networks_merge_single_streamorder", .x))





rivers_merge_test %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)

rivers_test %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) +
  facet_wrap(~strahler)

endpoints_one_side <-
  rivers_test %>%
  st_endpoint() %>%
  st_as_sf()

endpoints_other_side <-
  rivers_test %>%
  st_startpoint() %>%
  st_as_sf()

endpoints <-
  endpoints_one_side %>%
  bind_rows(endpoints_other_side) %>% 
  distinct()

rivers_test_filter <- 
  rivers_test %>% 
  filter_intersecting_features(endpoints)

# lines_canals_endpoints_join <-
endpoints %>%
  add_feature_index_column(column_name = "endpoint_id") %>% 
  st_join(rivers_test) %>% 
  group_by(endpoint_id) %>%
  mutate(n_features = n()) %>% 
  filter(n_features > 2) %>% 
  mutate(n_distint_strahler = length(unique(strahler))) %>% 
  filter(n_distint_strahler > 1)

group_split() %>%
  map_dfr(~st_join(., lines_filter)) %>% 
  filter(feature_id.x != feature_id.y) %>% 
  group_by(feature_id.x) %>% 
  group_split() %>% 
  map(add_side_column)

















tar_read(selected_studyarea) %>% 
  ggplot() +geom_sf()

tar_read(lateral_position_stream_divide_distance) %>%
  pluck(3) %>%
  select(all_of("lateral_position")) %>%
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>%
  write_stars("output_data/300m_test.tiff")

test <- tar_read(river_networks_only_rivers)
studyarea <- tar_read(selected_studyarea)

test <- 
  test %>% 
  mutate(strahler = if_else(feature_id %in% c(212, 113, 114, 202, 89), -9999, strahler))

{test %>% 
    ggplot() +
    geom_sf(aes(colour = as.character(strahler))) +
    # geom_sf_label(data = st_centroid(test), 
    #               aes(label = feature_id, 
    #                   colour = is.element(strahler, INVALID_STRAHLER_VALUES)), 
    #               label.size = 0, 
    #               alpha =.6) +
    geom_sf(data = studyarea, fill = NA) +
    theme(legend.position = "none")} %>% 
  plotly::ggplotly()


{test %>% 
    impute_streamorder(st_cast(studyarea, "LINESTRING")) %>% 
    ggplot() +
    geom_sf(aes(colour = as.character(strahler))) +
    # geom_sf_label(data = st_centroid(test),
    #               aes(label = feature_id,
    #                   colour = is.element(strahler, INVALID_STRAHLER_VALUES)),
    #               label.size = 0,
    #               alpha =.6) +
    geom_sf(data = studyarea, fill = NA)} %>% 
  plotly::ggplotly()


test %>% 
  impute_streamorder(st_cast(studyarea, "LINESTRING")) %>% 
  filter(feature_id == 113)


test %>% 
  impute_streamorder(studyarea)






glue::glue("
        CREATE EXTENSION postgis_raster;
        CREATE TABLE {GRID_POLYGONS_TABLE} AS (
          with grid AS (
          	SELECT 
          	  (ST_PixelAsCentroids(ST_AsRaster(ST_Union(ST_MakePolygon(geometry)), {CELLSIZE}.0,{CELLSIZE}.0))).geom AS geometry
          	FROM {SELECTED_STUDYAREA_TABLE}
          ), with_id AS (
          	SELECT 
            	geometry, 
            	row_number() OVER (ORDER BY geometry) AS grid_id 
          	FROM grid
          )
        )
        ")

tar_read(base_grid_centroids) %>% 
  as_tibble()

get_table_from_postgress("merge_test") %>% 
  query_result_as_sf() %>% 
  add_feature_index_column() %>% 
  mutate(feature_id = as.character(feature_id)) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>% 
  plotly::ggplotly()


test <- 
  river_networks %>% 
  group_by(strahler) %>%
  summarise()

river_networks %>% 
  slice(1:5) %>% 
  st_cast("LINESTRING")



tar_read(river_networks_only_connected) 
tar_read(river_networks_dissolved_junctions) 
tar_read(river_networks_without_brackets) 
test <- tar_read(river_networks_only_connected) %>% 
  # as_tibble() %>% 
  slice(1:5)

tar_read(river_networks_only_connected) %>% st_geometry_type()

st_write("output_data/germany_rivers_only_connected.shp")

# rivers before calculation
test <- 
  LINES_BY_STREAMORDER %>% 
  get_table_from_postgress() %>% 
  query_result_as_sf()

test %>% 
  group_by(stream_order_id) %>% 
  group_split() %>% 
  map(plot_lines_coloured_by_categorical_attribute, feature_id)


# nearest neighbours to rivers
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_NN_STREAMORDER, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(1) %>% 
  select(grid_geometry, distance_meters) %>% 
  rename(geometry = grid_geometry) %>% 
  query_result_as_sf() %>% 
  mapview::mapview(alpha = 0, zcol = "distance_meters")


# thiessen catchments
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_THIESSEN_CATCHMENTS, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(6) %>%
  query_result_as_sf() %>%
  mapview::mapview(alpha = 1)

# nearest neighbours to divide
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_NN_GRID_TO_CATCHMENTS, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(2) %>%
  rename(geometry = grid_geometry) %>% 
  query_result_as_sf() %>%
  mapview::mapview(alpha = 0, zcol = "distance_meters")



# lateral position
test <- 
  tar_read(streamorders) %>%
  map(~composite_name(TABLE_NAME_PREFIX_LATERAL_POSITION_STREAM_DIVIDE_DISTANCE, .)) %>% 
  map(get_table_from_postgress) %>% 
  map(as_tibble)

test %>% 
  pluck(1) %>% 
  select(geometry, divide_stream_distance) %>%
  query_result_as_sf() %>% 
  mapview::mapview(alpha = 0, zcol = "divide_stream_distance")


tar_read(lateral_position_stream_divide_distance)

tar_meta() %>% 
  select(name, seconds, type) %>% 
  filter(type == "stem") %>% 
  arrange(-seconds) %>% 
  View()





tar_read(river_basins) %>%
  slice(1:3) %>% 
  # summarise() %>% 
  st_difference(tar_read(coastline) %>% st_zm()) %>%
  st_cast("POLYGON") %>% 
  select(geometry)
tar_read(river_networks_clip)
tar_read(river_basins) %>% 
  st_intersects(tar_read(selected_studyarea))

tar_read(river_networks_files)

river_basins <- tar_read(river_basins)
studyarea <- tar_read(selected_studyarea)
river_networks <- tar_read(river_networks)

relevant_river_basins <- 
  river_basins %>%
  filter(as.vector(st_intersects(., studyarea, sparse = FALSE))) %>% 
  pull(river_basin_name)


river_networks %>%
  # slice_sample(prop = .6) %>%
  filter(river_basin_name %in% relevant_river_basins) %>%
  filter(as.vector(st_intersects(., studyarea, sparse = FALSE))) %>%
  st_intersection(studyarea) %>% 
  st_cast("MULTILINESTRING") %>% 
  add_feature_index_column()

tar_read(lateral_position_stream_divide_distance) %>% 
  pluck(3) %>% 
  select(lateral_position) %>% 
  st_rasterize(dx = CELLSIZE, dy = CELLSIZE) %>% 
  plot()



test_rivers <- 
  tar_read(river_networks_files_files) %>% 
  magrittr::extract(str_detect(., "rhine")) %>% st_read("River_Net_l")

test_rivers %>% 
  st_intersects(tar_read(studyarea_subset_plots), sparse = FALSE) %>% 
  as.vector() %>% 
  magrittr::extract(. == FALSE)




tar_read(river_networks)

is_db_hash_outdated(tar_read(db_river_networks_clean), LINES_CLEAN) | is_db_hash_outdated(tar_read(db_connected_but_merged_river_networks), LINES_CONNECTED_ID)

tar_read(river_network_by_streamorder) %>% 
  filter(stream_order_id == 1)

test <- 
  get_table_from_postgress("nearest_neighbours_streamorder_id_1") %>% 
  rename(geometry = grid_geometry) %>% 
  query_result_as_sf()

thiessen_catchments <- 
  test %>% 
  rename(nearest_feature = river_network_by_streamorder_feature_id) %>% 
  make_thiessen_catchments(tar_read(base_grid), .)

thiessen_catchments %>% 
  mutate(feature_id = as.character(row_number())) %>% 
  ggplot() +
  geom_sf(fill = NA)



test %>% 
  select(distance_meters) %>% 
  st_intersection(st_buffer(sample_n(., 1), dist = 1E4)) %>% 
  mapview::mapview(zcol = "distance_meters", alpha = 0)


area_test <- 
  studyarea_subset_plots %>% 
  st_area() %>% 
  as.numeric()

area_test / 1E6

6E6 / (area_test / 1E6)
tar_read(river_networks_only_connected) %>% 
  as_tibble() %>% 
  mutate(geometry = as.character(geometry)) %>% 
  distinct(geometry)
tar_read(river_networks_clean)


tar_read(river_networks_only_connected) %>% 
  mutate(strahler = as.character(strahler)) %>% 
  plot_lines_coloured_by_categorical_attribute(strahler) %>% 
  plotly::ggplotly()

list(
  tar_read("river_networks_clip"),
  tar_read(river_networks_only_rivers),
  tar_read(river_networks_valid_strahler),
  tar_read(river_networks_clean),
  tar_read(river_networks_only_connected),
  tar_read(river_networks_dissolved_junctions),
  tar_read(river_networks_without_brackets),
  tar_read(river_networks_dissolved_junctions_after)
) %>%
  map(plot_lines_coloured_by_categorical_attribute, feature_id) %>% 
  pluck(1) %>% 
  list() %>% 
  cowplot::plot_grid(plotlist = .)


test <- 
  get_table_from_postgress(connect_to_database(), "dc_segments") %>%
  query_result_as_sf() %>%
  add_feature_index_column()


connection <- 
  connect_to_database()

tar_read(river_networks_clean) %>% 
  st_cast("LINESTRING") %>% 
  write_to_table(
    connection = connection,
    table_name = "test_lines_clean"
  )




tar_read(river_networks_dissolved_brackets) %>% 
  st_cast("LINESTRING") %>% 
  add_feature_index_column() %>% 
  # dissolve_line_features_between_junctions() %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>%
  plotly::ggplotly()

tar_read(river_networks_dissolved_brackets) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>%
  plotly::ggplotly()


strahler_error <- 
  tar_read(river_networks_clip) %>% 
  filter(abs(strahler) >= 10 & dfdd == "BH140") %>% 
  select(strahler)


tar_read(river_networks_clip) %>% 
  filter(dfdd == "BH140") %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = strahler_error, colour = "red")

tar_read(river_networks_only_rivers) %>% 
  mutate(strahler = as.character(strahler)) %>% 
  # distinct(strahler)
  plot_lines_coloured_by_categorical_attribute(strahler) +
  geom_sf(data = tar_read(studyarea_subset_plots), fill = NA) +
  theme(legend.position = "bottom")

plot_before_vs_after(tar_read(river_networks_clean), tar_read(river_networks_strahler_merge))

log(1:6)+1


tar_read(river_networks_strahler_merge) %>% 
  ggplot() +
  geom_sf()

plot_b <- 
  tar_read(river_networks_strahler_merge) %>% 
  ggplot() +
  geom_sf(aes(colour = feature_id, size = log(as.numeric(strahler)+1))) +
  scale_colour_manual(values = generate_discrete_colour_values(tar_read(river_networks_strahler_merge), feature_id)) +
  theme(legend.position = "none") +
  labs(title = "Colour represents line features")




river_networks_strahler_merge %>% 
  st_sf()

tar_read(river_networks_split) %>% 
  bind_rows()

sf_lines %>% 
  st_cast("MULTILINESTRING")


river_networks_strahler_merge %>% 
  as_tibble() %>% 
  distinct(strahler) %>% 
  pull(strahler) %>% 
  as.numeric() %>%
  sort()

base_grid %>% 
  ggplot() +
  geom_sf()


st_layers("J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/european_catchments_and_rivers_network_system_(ecrins)/data/EcrRiv.sqlite")
st_read("J:/NUTZER/Noelscher.M/Studierende/Daten/waterbodies_streams/europe/time_invariant/vector/european_catchments_and_rivers_network_system_(ecrins)/data/EcrRiv.sqlite",
        layer = "c_tr")

values <-
  tibble(
    output_function = rlang::syms("calculate_lateral_position_grid"),
    streamorders = 1:6,
    # centroids_stream_distance = rlang::syms("centroids_stream_distance"),
    # centroids_divide_distance = rlang::syms("centroids_divide_distance"),
    grid =  rlang::syms("base_grid"),
    field_name = "lateral_position",
    data_source = str_c(field_name, "_", streamorders)
  )


targets_output <- 
  tar_map(
    
    # tar_target(
    #   files_lateral_position,
    #   generate_filepaths(
    #     streamorders,
    #     "lp"
    #   ),
    #   format = "file"
    # ),
    values = values,
    names = streamorders,
    tar_target(
      lateral_position,
      # list(
      #   centroids_stream_distance,
      #   centroids_divide_distance,
      #   streamorders  
      # ),
      output_function(
        centroids_stream_distance,
        centroids_divide_distance,
        streamorders,
        grid,
        field_name
      ),
      format = "file"
    )
  )

tar_pattern(
  map(x),
  x = 5,
  y = 1
)
targets::tar_make_future(workers = future::availableCores())
tar_read(unique_feature_ids)


river_networks_clean %>% 
  as_tibble() %>% 
  distinct(feature_id) %>% 
  pull(feature_id)

river_networks_clean <- 
  river_networks_clean %>% 
  st_cast("MULTILINESTRING") %>%
  rename(geometry = x)

river_networks_strahler_merge %>% 
  ggplot() +
  geom_sf(aes(colour = feature_id))

st_distance(
  filter(river_networks_clean, feature_id == 2) %>% summarise(),
  filter(river_networks_clean, feature_id == 12) %>% summarise()
)

# tar_read(coastline) %>% 
#   # slice(1:5) %>% 
#   st_union()

tar_read(river_networks_strahler_merge) %>% 
  plot_test_processed_river_network(tar_read(studyarea_subset_plots))


river_networks_strahler_merge %>% 
  st_as_sf() %>%
  mutate(id = as.character(1:n())) %>%
  ggplot() +
  geom_sf(aes(colour = id))
ggplot()

testplot <-
  ggplot() +
  geom_sf(
    data = thiessen_catchments[[1]],
    colour = "white",
    fill = "grey70"
  ) +
  geom_sf(
    data = river_network_by_streamorder[[1]],
    aes(colour = as.factor(strahler))
  )

testplot %>% 
  plotly::ggplotly()

river_network_by_streamorder[[1]] <- 
  river_network_by_streamorder[[1]] %>% 
  mutate(segment_id = 1:n())

segment_colours <- 
  river_network_by_streamorder[[1]] %>%
  # distinct(nearest_feature) %>% 
  nrow() %>% 
  hues::iwanthue(lmin = 40,
                 cmax = 70)

testplot1 <- 
  river_network_by_streamorder[[1]] %>% 
  ggplot() +
  geom_sf(
    data = river_network_by_streamorder[[1]],
    aes(colour = as.factor(strahler)),
    size = 2
  ) +
  theme(legend.position = "none") +
  labs(title = "Farbecode repräsentiert Strahler order")

testplot2 <- 
  river_network_by_streamorder[[1]] %>% 
  ggplot() +
  geom_sf(aes(colour = as.character(segment_id)),
          size = 2) +
  scale_colour_manual(values = segment_colours) +
  theme(legend.position = "none") +
  labs(title = "Farbecode repräsentiert Features/Liniensegmente")

testplot1 + testplot2






river_network_by_streamorder[[1]] %>% 
  mutate(test = river_network_by_streamorder[[1]] %>% 
           st_touches(sparse = FALSE))

test <- river_network_by_streamorder[[1]] %>% 
  st_touches()

key <- 
  test %>% 
  map(as.vector) %>% 
  # imap(~c(.x, .y)) %>% 
  map(sort) %>% 
  map(as.character) %>% 
  map(str_c, collapse = "") %>% 
  unlist()

lines_dissolved <- 
  river_network_by_streamorder[[1]] %>% 
  mutate(key = key, .before = 2) %>% 
  mutate(key = str_c(key, strahler)) %>% 
  group_by(key) %>% 
  summarise()

segment_colours <- 
  lines_dissolved %>%
  # distinct(nearest_feature) %>% 
  nrow() %>% 
  hues::iwanthue(lmin = 40,
                 cmax = 70)

testplot3 <-
  lines_dissolved %>%  
  ggplot() +
  geom_sf(aes(colour = as.character(key)),
          size = 2) +
  scale_colour_manual(values = segment_colours) +
  theme(legend.position = "none") +
  labs(title = "Farbecode repräsentiert Features/Liniensegmente")


testplot3 + testplot2

river_network_by_streamorder[[1]] %>% 
  st_touches(sparse = FALSE) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  filter(value == TRUE) %>% 
  mutate(name = as.numeric(str_sub(name, 2L))) %>% 
  mutate(name2 = 1:n(), .before = 1)


river_network_by_streamorder[[1]] %>% 
  st_intersects(river_network_by_streamorder[[1]])


lines_dissolved <- 
  river_network_by_streamorder[[1]] %>% 
  group_by(strahler) %>% 
  summarise()

lines_singlefeature <- 
  river_network_by_streamorder[[1]] %>% 
  # group_by(strahler) %>% 
  summarise()

river_network_by_streamorder[[1]] %>%
  group_by(strahler) %>%
  summarise() %>%
  st_line_merge()

st_union(lines_dissolved, lines_singlefeature, by_feature = TRUE, is_coverage = TRUE)

st_line_merge()
lwgeom::st_split()
st_combine()
st_intersection()
st_union()
st_collection_extract()



lines_dissolved %>% 
  st_line_merge()


river_networks_dissolved <- 
  river_networks_merge %>%
  group_by(strahler) %>%
  summarise() %>%
  st_line_merge()

result_intersection <- 
  lines_singlefeature %>%
  st_intersection(lines_dissolved %>% 
                    # select(-strahler) %>%
                    st_cast("MULTILINESTRING"))

result_intersection_points <-
  result_intersection %>% 
  filter(st_is(., "POINT")) %>% 
  distinct(geometry)














ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
            st_linestring(rbind(c(0,0),c(10,0))))
st_line_sample(ls, density = 1)



lwgeom::st_split(lines_singlefeature, lines_dissolved)

lines_singlefeature %>% 
  st_intersection(lines_dissolved)

testplot1 + testplot2

testplot %>% 
  plotly

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
  pluck(1) %>% 
  # distinct(nearest_feature) %>% 
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



# features_to_reclassify <- 
#   features_to_reclassify %>% 
# mutate(unique_feature_id = str_c(source_id, objectid, sep = "_"),
#        .before = 1) %>% 


add_source_id <- 
  function(x, index) {
    x %>% 
      mutate(source_id = as.character(index),
             .before = 1)
  }


cyls <- 4
mtcars %>%
  # filter(cyl == cyls) %>%
  group_by(vs) %>%
  do({
    assert_that(
      length(na.omit(.$mpg)) > 1,
      msg = "I cannot grok the data"
    )
    .
  }) %>%
  summarize(z = max(density(mpg)$y))








tar_read(river_networks_clean) %>% 
  dissolve_line_features_between_junctions() %>% 
  add_feature_index_column() %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id)





tar_read(river_networks_dissolved) %>% 
  plot_lines_coloured_by_categorical_attribute(feature_id) %>% 
  plotly::ggplotly()

tar_read(river_networks_dissolved) %>% 
  filter(feature_id %in% c(80, 12, 83, 81)) %>% 
  st_touches()


{tar_read(river_networks_dissolved) %>% 
    ggplot() +
    geom_sf() +
    geom_sf(data = split_points, size = 3)} %>% plotly::ggplotly()
