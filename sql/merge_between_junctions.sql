--https://gis.stackexchange.com/questions/147316/merging-linestrings-in-qgis-using-postgresql-and-postgis
DROP TABLE IF EXISTS lines_merged;
CREATE TABLE lines_merged AS (
	WITH klaro AS (
		SELECT 
			*
		FROM lines_connected
	)
	SELECT
		(ST_Dump(ST_LineMerge(ST_Multi(St_Collect(geometry))))).geom AS geometry
	FROM klaro
	)
--ST_LineMerge(ST_Multi(St_Collect(geometry)))