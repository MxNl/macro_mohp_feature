DROP TABLE IF EXISTS thiessen_catchments_1;
CREATE TABLE thiessen_catchments_1 AS (
	WITH right_table AS (
		SELECT
			river_network_by_streamorder_feature_id,
			grid_id
		FROM nearest_neighbours_streamorder_id_1
	), joined_table AS (
		SELECT
			*
		FROM grid_polygons
			LEFT JOIN right_table
			ON grid_id = id
	), catchments AS (
		SELECT 
			river_network_by_streamorder_feature_id,
			ST_Union(geometry)
		FROM joined_table
			GROUP BY 
				river_network_by_streamorder_feature_id
	), catchment_boundary AS(
	SELECT
		river_network_by_streamorder_feature_id,
		ST_InteriorRingN(geometry)
		FROM catchments
	)
	SELECT
		*
	FROM catchment_boundary
)