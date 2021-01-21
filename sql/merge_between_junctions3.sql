----https://gis.stackexchange.com/questions/238329/how-to-break-multilinestring-into-constituent-linestrings-in-postgis
SELECT 
	strahler, 
	(dump_set).path AS line_part, 
	ST_AsEWKT((dump_set).geom) AS line_string
FROM (
    SELECT 
		strahler, 
		ST_Dump(ST_LineMerge(geometry)) AS dump_set
    FROM lines_connected
    ) AS dump_results;