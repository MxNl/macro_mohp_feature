CREATE EXTENSION postgis_raster;

with grid as (
	SELECT (ST_PixelAsCentroids(ST_AsRaster(ST_Union(ST_MakePolygon(geometry)),30.0,30.0))).geom AS geometry
	FROM thiessen_catchments_id_4
), with_id AS (
	select geometry, row_number() over (order by geometry) AS id from grid
)

select * from with_id