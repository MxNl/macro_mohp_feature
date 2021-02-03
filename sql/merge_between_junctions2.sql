--https://stackoverflow.com/questions/25753348/how-do-i-divide-city-streets-by-intersection-using-postgis
--Get a list of all intersections in city
DROP TABLE IF EXISTS dc_intersections;
DROP TABLE IF EXISTS dc_union;
DROP TABLE IF EXISTS dc_segments;
CREATE TABLE dc_intersections AS 
SELECT DISTINCT (ST_DUMP(ST_INTERSECTION(a.geometry, b.geometry))).geom AS ix 
FROM lines_connected a 
INNER JOIN lines_connected b 
ON ST_INTERSECTS(a.geometry,b.geometry)
WHERE geometrytype(st_intersection(a.geometry,b.geometry)) = 'POINT';

CREATE INDEX ON dc_intersections USING gist(ix);

CREATE TABLE dc_union AS 
SELECT ST_UNION(geometry) as geometry
FROM lines_connected;

CREATE INDEX ON dc_union USING gist(geometry);

CREATE TABLE dc_segments AS
SELECT ST_SPLIT(a.geometry,b.ix)
FROM dc_union a
INNER JOIN dc_intersections b
ON ST_INTERSECTS(a.geometry, b.ix);