--DROP TABLE IF EXISTS brackets_to_drop;
CREATE TABLE brackets_to_drop AS (
	WITH pairs AS (
		SELECT
			l.feature_id,
			l.geometry,
			l.strahler,
			r.feature_id AS r_feature_id,
			r.geometry AS r_geometry,
			r.strahler AS r_strahler
		FROM lines_raw AS l CROSS JOIN lines_raw AS r
		WHERE
			l.feature_id != r.feature_id
			AND
			(ST_Startpoint(l.geometry) = ST_Startpoint(r.geometry) OR ST_Startpoint(l.geometry) = ST_Endpoint(r.geometry))
			AND
			(ST_Endpoint(l.geometry) = ST_Endpoint(r.geometry) OR ST_Endpoint(l.geometry) = ST_Startpoint(r.geometry))
			AND
			ST_Length(l.geometry) < ST_Length(r.geometry)
	) 

	SELECT 
		feature_id,
		strahler,
		CASE WHEN ST_Length(geometry) > ST_Length(r_geometry)
			THEN r_geometry
			ELSE geometry
		END AS shorter_geometry,
		CASE WHEN ST_Length(geometry) < ST_Length(r_geometry)
			THEN feature_id
			ELSE r_feature_id
		END AS bracket_feature_id
	FROM pairs
)

	


