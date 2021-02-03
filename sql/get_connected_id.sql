CREATE TABLE connected_id AS (
	WITH asdas AS (
		SELECT 
			*
		FROM lines_clean
	), endpoints AS (SELECT ST_Collect(ST_StartPoint(geometry), ST_EndPoint(geometry)) AS geometry FROM asdas),
	     clusters  AS (SELECT unnest(ST_ClusterWithin(geometry, 1e-8)) AS geometry FROM endpoints),
	     clusters_with_ids AS (SELECT row_number() OVER () AS connected_id, ST_CollectionHomogenize(geometry) AS geometry FROM clusters)
		 
	SELECT 
		connected_id,
		ST_Collect(asdas.geometry) AS geometry
	FROM asdas
	LEFT JOIN clusters_with_ids ON ST_Intersects(asdas.geometry, clusters_with_ids.geometry)
	GROUP BY connected_id
)