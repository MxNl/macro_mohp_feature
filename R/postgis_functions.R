nearest_neighbours_between <- function(
  left_table,
  left_columns,
  right_table,
  right_columns,
  stream_order_id,
  n = 1,
  as_wkt = TRUE,
  left_target,
  right_target
) {
  class(left_target)
  class(right_target)
  connection <- connect_to_database()
  if (!("PqConnection" %in% class(connection))) { stop("Supply a connection of class 'PqConnection'") }
  if (!(class(n) %in% c('numeric', 'integer'))) { stop('n must be numeric or integer') }
  if (n %% 1 != 0) { stop(paste('n cannot be interpreted as integer, got:', n)) }
  
  number_of_nearest_neighbours <- n
  
  validate <- function(table, columns) {
    if (!DBI::dbExistsTable(connection, table)) { stop(paste("Table", table, "not in database")) }
    
    present_columns <- DBI::dbListFields(connection, name = table)
    if (!('geometry' %in% present_columns)) { stop(paste("Table", table, "does not have column 'geometry'")) }
    
    missing_columns <- setdiff(columns, present_columns)
    
    if (length(missing_columns) != 0) {
      message <- paste('Invalid columns selected:', paste(missing_columns, collapse = ', '), 'not in', table)
      stop(message)
    }
  }
  
  format_select <- function(table, columns) {
    validate(table, columns)
    select_statements <- paste0(table, '.', columns, ' AS ', table, '_', columns)
    paste0(select_statements, collapse = ', ')
  }
  
  geometry_of <- function(table) { paste0(table, '.geometry') }
  
  left_select <- format_select(left_table, left_columns)
  right_select <- format_select(right_table, right_columns)
  left_geometry <- geometry_of(left_table)
  right_geometry <- geometry_of(right_table)
  
  distance <- glue::glue('ST_Distance({left_geometry}, {right_geometry}) AS distance_meters')
  
  query <- glue::glue("
  CREATE_TABLE nearest_neighbours_streamorder_id_{stream_order_id} AS (
    SELECT
      {left_select},
      {right_select},
      {distance}
    FROM
      {left_table}
      CROSS JOIN LATERAL (
        SELECT
          *
        FROM
          {right_table}
        WHERE
          {right_table}.stream_order_id = {stream_order_id}
        ORDER BY
          {left_geometry} <-> {right_geometry}
        LIMIT
          {number_of_nearest_neighbours}
      ) AS {right_table}
    );
  ")
  print(query)
  result <- tibble::as_tibble(DBI::dbGetQuery(connection, query))
  
  if (!as_wkt) { return(result) }
  
  is_pq_geometry <- function(column) { class(column) == "pq_geometry" }
  from_wkb <- purrr::partial(sf::st_as_sfc, EWKB = TRUE)
  dplyr::mutate(result, dplyr::across(where(is_pq_geometry), from_wkb))
}


set_geo_indices <- function(table_names) {
  connection <- connect_to_database()
  map(table_names,  ~ {
    DBI::dbExecute(connection, glue::glue("DROP INDEX IF EXISTS {.}_geometry_idx;"))
    DBI::dbExecute(connection, glue::glue("CREATE INDEX {.}_geometry_idx ON {.} USING GIST (geometry);"))
  })
}
