#' Query helpers for direct DBI access from ontologySpecR metadata
#'
#' These functions build parameterized SQL queries using ontologySpecR
#' metadata (table names, column names, join keys) and execute them
#' via DBI. This is the primary data access layer that works with
#' just ontologySpecR + DBI, without requiring objectSetsR.

#' Fetch objects of a given type with optional filters
#'
#' Builds and executes a SELECT query against the backing table for an
#' object type, applying any filters and free-text search.
#'
#' @param connection A DBI connection object.
#' @param object_type An ontologySpecR object_type.
#' @param filters A named list of filter specifications. Each element should
#'   be a list with `property_id`, `type` (the widget type), and `value`.
#' @param search_text Optional free-text search string. Searches across all
#'   string-type properties using LIKE.
#' @param limit Maximum number of rows to return (default 1000).
#' @return A data.frame of matching objects.
#' @export
fetch_objects <- function(connection, object_type, filters = list(),
                          search_text = NULL, limit = 1000) {
  table_name <- resolve_table_name(object_type)
  columns <- resolve_columns(object_type)

  col_sql <- paste(
    vapply(columns, function(c) quote_id(connection, c), character(1)),
    collapse = ", "
  )

  where_clauses <- list()
  params <- list()

  # Apply property filters
  for (f in filters) {
    result <- build_filter_clause(connection, object_type, f)
    if (!is.null(result$clause)) {
      where_clauses <- c(where_clauses, result$clause)
      params <- c(params, result$params)
    }
  }

  # Apply free-text search across string properties
  if (!is.null(search_text) && nzchar(trimws(search_text))) {
    search_clause <- build_search_clause(connection, object_type, search_text)
    if (!is.null(search_clause$clause)) {
      where_clauses <- c(where_clauses, search_clause$clause)
      params <- c(params, search_clause$params)
    }
  }

  where_sql <- ""
  if (length(where_clauses) > 0) {
    where_sql <- paste(" WHERE", paste(where_clauses, collapse = " AND "))
  }

  sql <- paste0(
    "SELECT ", col_sql,
    " FROM ", quote_id(connection, table_name),
    where_sql,
    " LIMIT ", as.integer(limit)
  )

  if (length(params) > 0) {
    DBI::dbGetQuery(connection, sql, params = params)
  } else {
    DBI::dbGetQuery(connection, sql)
  }
}

#' Fetch linked objects for given source objects
#'
#' Follows a link type from source objects to target objects using a JOIN
#' based on the link's join keys.
#'
#' @param connection A DBI connection object.
#' @param bundle An ontologySpecR bundle.
#' @param link_type An ontologySpecR link_type.
#' @param source_object_ids A vector of primary key values from the source objects.
#' @param direction Either "forward" (from -> to) or "reverse" (to -> from).
#' @return A data.frame of linked objects.
#' @export
fetch_linked <- function(connection, bundle, link_type,
                         source_object_ids, direction = "forward") {
  if (length(source_object_ids) == 0) return(data.frame())

  if (direction == "forward") {
    target_type_id <- link_type$to
    from_keys <- link_type$join$fromKeys
    to_keys <- link_type$join$toKeys
  } else {
    target_type_id <- link_type$from
    from_keys <- link_type$join$toKeys
    to_keys <- link_type$join$fromKeys
  }

  source_type_id <- if (direction == "forward") link_type$from else link_type$to
  source_type <- get_object_type(bundle, source_type_id)
  target_type <- get_object_type(bundle, target_type_id)
  if (is.null(source_type) || is.null(target_type)) return(data.frame())

  source_table <- resolve_table_name(source_type)
  target_table <- resolve_table_name(target_type)
  target_columns <- resolve_columns(target_type)

  target_col_sql <- paste(
    vapply(target_columns, function(c) {
      paste0("t.", quote_id(connection, c))
    }, character(1)),
    collapse = ", "
  )

  # Build join condition
  join_conditions <- vapply(seq_along(from_keys), function(i) {
    fk <- resolve_property_column(source_type, from_keys[[i]])
    tk <- resolve_property_column(target_type, to_keys[[i]])
    paste0("s.", quote_id(connection, fk), " = t.", quote_id(connection, tk))
  }, character(1))
  join_sql <- paste(join_conditions, collapse = " AND ")

  # Build WHERE clause for source object IDs
  pk_cols <- get_pk_columns(source_type)
  if (length(pk_cols) == 0) return(data.frame())

  pk_col <- resolve_property_column(source_type, pk_cols[1])
  placeholders <- paste(rep("?", length(source_object_ids)), collapse = ", ")
  where_sql <- paste0("s.", quote_id(connection, pk_col), " IN (", placeholders, ")")

  sql <- paste0(
    "SELECT DISTINCT ", target_col_sql,
    " FROM ", quote_id(connection, source_table), " s",
    " INNER JOIN ", quote_id(connection, target_table), " t",
    " ON ", join_sql,
    " WHERE ", where_sql
  )

  DBI::dbGetQuery(connection, sql, params = as.list(source_object_ids))
}

#' Get distinct values for a string property
#'
#' Useful for populating dropdown filter choices.
#'
#' @param connection A DBI connection object.
#' @param object_type An ontologySpecR object_type.
#' @param property_id Character string: the property ID.
#' @param limit Maximum number of distinct values (default 100).
#' @return A character vector of distinct values.
#' @export
fetch_distinct_values <- function(connection, object_type, property_id,
                                  limit = 100) {
  table_name <- resolve_table_name(object_type)
  col_name <- resolve_property_column(object_type, property_id)

  sql <- paste0(
    "SELECT DISTINCT ", quote_id(connection, col_name),
    " FROM ", quote_id(connection, table_name),
    " WHERE ", quote_id(connection, col_name), " IS NOT NULL",
    " ORDER BY ", quote_id(connection, col_name),
    " LIMIT ", as.integer(limit)
  )

  result <- DBI::dbGetQuery(connection, sql)
  if (nrow(result) == 0) return(character(0))
  as.character(result[[1]])
}

#' Get min/max values for a numeric property
#'
#' Useful for setting slider range bounds.
#'
#' @param connection A DBI connection object.
#' @param object_type An ontologySpecR object_type.
#' @param property_id Character string: the property ID.
#' @return A named list with `min` and `max` values.
#' @export
fetch_range <- function(connection, object_type, property_id) {
  table_name <- resolve_table_name(object_type)
  col_name <- resolve_property_column(object_type, property_id)

  sql <- paste0(
    "SELECT MIN(", quote_id(connection, col_name), ") AS min_val, ",
    "MAX(", quote_id(connection, col_name), ") AS max_val",
    " FROM ", quote_id(connection, table_name)
  )

  result <- DBI::dbGetQuery(connection, sql)
  list(min = result$min_val[1], max = result$max_val[1])
}

# --- Internal helpers ---

#' Resolve the backing table name for an object type
#' @keywords internal
resolve_table_name <- function(object_type) {
  tbl <- object_type$source$table
  if (!is.null(tbl) && nzchar(tbl)) return(tbl)
  # Fallback: use the object type id as table name
  object_type$id
}

#' Resolve all column names for an object type
#' @keywords internal
resolve_columns <- function(object_type) {
  vapply(object_type$properties, property_column, character(1))
}

#' Resolve a specific property's column name
#' @keywords internal
resolve_property_column <- function(object_type, property_id) {
  for (prop in object_type$properties) {
    if (identical(prop$id, property_id)) {
      return(property_column(prop))
    }
  }
  # Fallback: use property_id directly
  property_id
}

#' Quote a SQL identifier safely
#' @keywords internal
quote_id <- function(connection, name) {
  as.character(DBI::dbQuoteIdentifier(connection, name))
}

#' Build a WHERE clause for a single filter
#' @keywords internal
build_filter_clause <- function(connection, object_type, filter) {
  prop_id <- filter$property_id
  col_name <- resolve_property_column(object_type, prop_id)
  quoted_col <- quote_id(connection, col_name)
  value <- filter$value
  widget_type <- filter$type

  if (is.null(value)) return(list(clause = NULL, params = list()))

  switch(widget_type,
    text = {
      if (!nzchar(trimws(value))) {
        return(list(clause = NULL, params = list()))
      }
      list(
        clause = paste0(quoted_col, " LIKE ?"),
        params = list(paste0("%", value, "%"))
      )
    },
    numeric_range = {
      clauses <- list()
      params <- list()
      if (!is.null(value$min) && !is.na(value$min)) {
        clauses <- c(clauses, paste0(quoted_col, " >= ?"))
        params <- c(params, list(value$min))
      }
      if (!is.null(value$max) && !is.na(value$max)) {
        clauses <- c(clauses, paste0(quoted_col, " <= ?"))
        params <- c(params, list(value$max))
      }
      if (length(clauses) == 0) {
        return(list(clause = NULL, params = list()))
      }
      list(
        clause = paste0("(", paste(clauses, collapse = " AND "), ")"),
        params = params
      )
    },
    checkbox = {
      # Boolean filter: TRUE/FALSE
      if (is.na(value)) {
        return(list(clause = NULL, params = list()))
      }
      list(
        clause = paste0(quoted_col, " = ?"),
        params = list(as.integer(value))
      )
    },
    date_range = {
      clauses <- list()
      params <- list()
      if (!is.null(value$start) && !is.na(value$start)) {
        clauses <- c(clauses, paste0(quoted_col, " >= ?"))
        params <- c(params, list(as.character(value$start)))
      }
      if (!is.null(value$end) && !is.na(value$end)) {
        clauses <- c(clauses, paste0(quoted_col, " <= ?"))
        params <- c(params, list(as.character(value$end)))
      }
      if (length(clauses) == 0) {
        return(list(clause = NULL, params = list()))
      }
      list(
        clause = paste0("(", paste(clauses, collapse = " AND "), ")"),
        params = params
      )
    },
    select = {
      if (is.null(value) || !nzchar(value)) {
        return(list(clause = NULL, params = list()))
      }
      list(
        clause = paste0(quoted_col, " = ?"),
        params = list(value)
      )
    },
    list(clause = NULL, params = list())
  )
}

#' Build a search clause across all string properties
#' @keywords internal
build_search_clause <- function(connection, object_type, search_text) {
  string_props <- Filter(function(p) identical(p$type, "string"), object_type$properties)
  if (length(string_props) == 0) return(list(clause = NULL, params = list()))

  or_clauses <- vapply(string_props, function(p) {
    col <- resolve_property_column(object_type, p$id)
    paste0(quote_id(connection, col), " LIKE ?")
  }, character(1))

  like_val <- paste0("%", search_text, "%")
  params <- rep(list(like_val), length(string_props))

  list(
    clause = paste0("(", paste(or_clauses, collapse = " OR "), ")"),
    params = params
  )
}
