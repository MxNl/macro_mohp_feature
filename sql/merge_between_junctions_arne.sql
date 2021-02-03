DROP TABLE IF EXISTS splitpoints;
WITH test AS (
SELECT 
	id, 
	geom
FROM (VALUES
        (1, 'MULTILINESTRING((0 10, 10 10))'::Geometry),
        (2, 'MULTILINESTRING((10 10, 10 20))'::Geometry),
        (3, 'MULTILINESTRING((10 10, 10 0))'::Geometry),
        (4, 'MULTILINESTRING((10 0, 20 0))'::Geometry)
        )Lines(id,geom)
), test2 AS (
SELECT
	ST_Startpoint(ST_Geometryn(geom, 1)) AS geom
FROM test
UNION ALL 
	SELECT
		ST_Endpoint(ST_Geometryn(geom, 1)) AS geom
	FROM test
), test3 AS (
SELECT 
	geom, 
	COUNT(geom)
FROM test2
	GROUP BY geom 
)
SELECT
	*
INTO splitpoints
FROM test3
	WHERE count >= 3;
	
	
	
	
WITH test AS (
SELECT 
	id, 
	geom
FROM (VALUES
        (1, 'MULTILINESTRING((0 10, 10 10))'::Geometry),
        (2, 'MULTILINESTRING((10 10, 10 20))'::Geometry),
        (3, 'MULTILINESTRING((10 10, 10 0))'::Geometry),
        (4, 'MULTILINESTRING((10 0, 20 0))'::Geometry)
        )Lines(id,geom)
)
SELECT 
	(ST_DUMP(ST_SPLIT((SELECT ST_Union(geom) FROM test), 
					  (SELECT geom FROM splitpoints)))).geom


