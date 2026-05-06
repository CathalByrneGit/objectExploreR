#' ObjectSetsR helper functions for objectExplorerR
#'
#' These helpers bridge the filter panel specifications and objectSetsR
#' query operations. They translate UI filter specs into os_filter() calls.

#' Apply a filter specification to an ObjectSet
#'
#' Translates a filter spec from the filter panel into an os_filter() call.
#'
#' @param os An ObjectSet from objectSetsR.
#' @param filter_spec A list with `property_id`, `type`, and `value`.
#' @return A filtered ObjectSet.
#' @keywords internal
apply_filter_to_os <- function(os, filter_spec) {
  prop_id <- filter_spec$property_id
  widget_type <- filter_spec$type
  value <- filter_spec$value

  if (is.null(value)) return(os)

  prop_sym <- rlang::sym(prop_id)

  switch(widget_type,
    text = {
      if (!nzchar(trimws(value))) return(os)
      pattern <- paste0("%", value, "%")
      objectSetsR::os_filter(os, !!prop_sym %like% !!pattern)
    },
    select = {
      if (is.null(value) || !nzchar(value)) return(os)
      objectSetsR::os_filter(os, !!prop_sym == !!value)
    },
    select_multi = {
      if (is.null(value) || length(value) == 0) return(os)
      objectSetsR::os_filter(os, !!prop_sym %in% !!value)
    },
    numeric_range = {
      min_val <- value$min
      max_val <- value$max
      if (!is.null(min_val) && !is.na(min_val) &&
          !is.null(max_val) && !is.na(max_val)) {
        objectSetsR::os_filter(os, !!prop_sym >= !!min_val, !!prop_sym <= !!max_val)
      } else if (!is.null(min_val) && !is.na(min_val)) {
        objectSetsR::os_filter(os, !!prop_sym >= !!min_val)
      } else if (!is.null(max_val) && !is.na(max_val)) {
        objectSetsR::os_filter(os, !!prop_sym <= !!max_val)
      } else {
        os
      }
    },
    checkbox = {
      if (is.na(value)) return(os)
      objectSetsR::os_filter(os, !!prop_sym == !!value)
    },
    boolean_tri = {
      if (is.null(value) || value == "any") return(os)
      bool_val <- as.logical(value)
      objectSetsR::os_filter(os, !!prop_sym == !!bool_val)
    },
    date_range = {
      start_val <- value$start
      end_val <- value$end
      if (!is.null(start_val) && !is.na(start_val) &&
          !is.null(end_val) && !is.na(end_val)) {
        objectSetsR::os_filter(os, !!prop_sym >= !!as.character(start_val),
                                !!prop_sym <= !!as.character(end_val))
      } else if (!is.null(start_val) && !is.na(start_val)) {
        objectSetsR::os_filter(os, !!prop_sym >= !!as.character(start_val))
      } else if (!is.null(end_val) && !is.na(end_val)) {
        objectSetsR::os_filter(os, !!prop_sym <= !!as.character(end_val))
      } else {
        os
      }
    },
    os
  )
}

#' Apply free-text search across all string properties
#'
#' Searches for the text in any string-type property using OR logic.
#'
#' @param os An ObjectSet from objectSetsR.
#' @param bundle The ontologySpecR bundle.
#' @param type_id The object type ID.
#' @param search_text The search string.
#' @return A filtered ObjectSet.
#' @keywords internal
apply_search_to_os <- function(os, bundle, type_id, search_text) {
  if (is.null(search_text) || !nzchar(trimws(search_text))) {
    return(os)
  }

  obj_type <- get_object_type(bundle, type_id)
  if (is.null(obj_type)) return(os)

  string_props <- Filter(function(p) identical(p$type, "string"), obj_type$properties)
  if (length(string_props) == 0) return(os)

  pattern <- paste0("%", search_text, "%")

  or_exprs <- lapply(string_props, function(p) {
    prop_sym <- rlang::sym(p$id)
    rlang::expr(!!prop_sym %like% !!pattern)
  })

  combined_expr <- Reduce(function(a, b) rlang::expr(!!a | !!b), or_exprs)

  objectSetsR::os_filter(os, !!combined_expr)
}

#' Perform link traversal using objectSetsR
#'
#' Traverses a link from selected source objects to target objects.
#'
#' @param ctx An OntologyContext from objectSetsR.
#' @param link_type An ontologySpecR link_type object.
#' @param source_ids Character vector of source primary key values.
#' @param direction Either "forward" or "reverse".
#' @return A data.frame of linked objects.
#' @keywords internal
traverse_link_os <- function(ctx, link_type, source_ids, direction = "forward") {
  if (length(source_ids) == 0) return(data.frame())

  bundle <- ctx$bundle
  link_type_id <- link_type$id

  if (direction == "forward") {
    source_type_id <- link_type$from
    target_type_id <- link_type$to
  } else {
    source_type_id <- link_type$to
    target_type_id <- link_type$from
  }

  source_type <- get_object_type(bundle, source_type_id)
  if (is.null(source_type)) return(data.frame())

  pk_cols <- get_pk_columns(source_type)
  if (length(pk_cols) == 0) return(data.frame())
  pk_col <- pk_cols[1]
  pk_sym <- rlang::sym(pk_col)

  source_os <- objectSetsR::object_set(ctx, source_type_id)
  source_os <- objectSetsR::os_filter(source_os, !!pk_sym %in% !!source_ids)

  if (direction == "forward") {
    target_os <- objectSetsR::os_traverse(source_os, link_type_id)
  } else {
    target_os <- objectSetsR::os_search_around(source_os, link_type_id)
  }

  tryCatch(
    objectSetsR::os_collect(target_os),
    error = function(e) data.frame()
  )
}

#' Get distinct values for a property using objectSetsR
#'
#' @param ctx An OntologyContext from objectSetsR.
#' @param type_id Character string: the object type ID.
#' @param property_id Character string: the property ID.
#' @param limit Maximum number of distinct values.
#' @return A character vector of distinct values.
#' @keywords internal
fetch_distinct_os <- function(ctx, type_id, property_id, limit = 100) {
  prop_sym <- rlang::sym(property_id)

  os <- objectSetsR::object_set(ctx, type_id)
  os <- objectSetsR::os_select(os, !!prop_sym)
  os <- objectSetsR::os_distinct(os)

  result <- tryCatch({
    df <- objectSetsR::os_collect(os)
    if (nrow(df) == 0) return(character(0))
    vals <- df[[property_id]]
    vals <- vals[!is.na(vals)]
    vals <- unique(vals)
    vals <- sort(vals)
    if (length(vals) > limit) vals <- vals[seq_len(limit)]
    as.character(vals)
  }, error = function(e) character(0))

  result
}

#' Get min/max range for a numeric property using objectSetsR
#'
#' @param ctx An OntologyContext from objectSetsR.
#' @param type_id Character string: the object type ID.
#' @param property_id Character string: the property ID.
#' @return A named list with `min` and `max` values.
#' @keywords internal
fetch_range_os <- function(ctx, type_id, property_id) {
  prop_sym <- rlang::sym(property_id)

  os <- objectSetsR::object_set(ctx, type_id)

  result <- tryCatch({
    agg <- objectSetsR::os_aggregate(
      os,
      min_val = min(!!prop_sym, na.rm = TRUE),
      max_val = max(!!prop_sym, na.rm = TRUE)
    )
    df <- objectSetsR::os_collect(agg)
    list(min = df$min_val[1], max = df$max_val[1])
  }, error = function(e) {
    list(min = 0, max = 100)
  })

  result
}

#' Custom LIKE operator for objectSetsR filters
#'
#' This is registered so that os_filter can use %like%.
#' @keywords internal
`%like%` <- function(x, pattern) {
  dplyr::sql(paste0(x, " LIKE '", gsub("'", "''", pattern), "'"))
}
