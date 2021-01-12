WITH collected AS (
	SELECT strahler, ST_Collect(geometry) AS geometry
	FROM lines GROUP BY strahler
), local_linestrings AS (
	SELECT strahler, (ST_Dump(ST_LineMerge(geometry))).geom AS geometry FROM collected
), local_linestrings_with_id AS (
	SELECT row_number() OVER (ORDER BY strahler) AS id, * FROM local_linestrings
), local_linestrings_with_splitpoints AS (
	SELECT
		l.id AS id,
		l.strahler AS strahler,
		ST_AsText(l.geometry) AS geometry,
		r.id AS r_id,
		r.strahler AS r_strahler,
		ST_AsText(r.geometry) AS r_geometry,
		ST_AsText(ST_Intersection(l.geometry, r.geometry)) AS common_geometry
	FROM local_linestrings_with_id AS l
	CROSS JOIN local_linestrings_with_id AS r
	WHERE 
		(
			l.strahler < r.strahler
			AND
			ST_Touches(l.geometry, r.geometry)
			AND
			l.id != r.id
		)
), local_linestrings_splitted AS (
	SELECT 
		id AS old_id, 
		strahler,
		(ST_Dump(
			CASE WHEN ST_Equals(geometry, common_geometry) THEN 
				ST_ForceCollection(geometry)
			ELSE
				ST_Split(geometry, common_geometry)
			END
		)).geom
	AS geometry
	FROM local_linestrings_with_splitpoints
), local_linestrings_without_splitpoints AS (
	SELECT * FROM local_linestrings_with_id
	WHERE
		NOT EXISTS (
		SELECT 
			1
		FROM
			local_linestrings_with_splitpoints
		WHERE 
			local_linestrings_with_splitpoints.id = local_linestrings_with_id.id)
)

SELECT * FROM local_linestrings_splitted
	UNION
	SELECT * FROM local_linestrings_without_splitpoints




