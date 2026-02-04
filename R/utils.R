#' Utility functions for objectExplorerR
#'
#' Display name resolution, type mapping, and link discovery helpers.

#' Get display name for an ontology object
#'
#' Returns the display name if available, otherwise falls back to the id.
#'
#' @param obj An ontologySpecR object with `$display$name` and `$id` fields.
#' @return A character string.
#' @keywords internal
display_name <- function(obj) {

  dn <- obj$display$name
  if (!is.null(dn) && nzchar(dn)) dn else obj$id
}

#' Get the column name for a property
#'
#' Returns the source column if specified, otherwise falls back to the property id.
#'
#' @param prop An ontologySpecR property_def object.
#' @return A character string.
#' @keywords internal
property_column <- function(prop) {
  col <- prop$source$column
  if (!is.null(col) && nzchar(col)) col else prop$id
}

#' Map ontology property type to a Shiny input widget type
#'
#' @param prop_type A character string: one of "string", "integer", "number",
#'   "boolean", "date", "datetime", "json".
#' @return A character string indicating the widget type to use:
#'   "text", "numeric_range", "checkbox", "date_range", or "none".
#' @keywords internal
property_type_to_widget <- function(prop_type) {
  switch(prop_type,
    string   = "text",
    integer  = "numeric_range",
    number   = "numeric_range",
    boolean  = "checkbox",
    date     = "date_range",
    datetime = "date_range",
    json     = "none",
    "none"
  )
}

#' Find all link types connected to an object type
#'
#' Iterates through bundle$links and returns links where the given object type
#' is either the `from` or `to` side of the link.
#'
#' @param bundle An ontologySpecR bundle.
#' @param object_type_id Character string: the object type ID to search for.
#' @return A list with two elements:
#'   \describe{
#'     \item{outgoing}{Links where this type is `from` (forward traversal)}
#'     \item{incoming}{Links where this type is `to` (reverse traversal / search around)}
#'   }
#' @keywords internal
find_links_for_type <- function(bundle, object_type_id) {
  outgoing <- list()
  incoming <- list()
  for (link in bundle$links) {
    if (identical(link$from, object_type_id)) {
      outgoing <- c(outgoing, list(link))
    }
    if (identical(link$to, object_type_id)) {
      incoming <- c(incoming, list(link))
    }
  }
  list(outgoing = outgoing, incoming = incoming)
}

#' Get an object type from a bundle by ID
#'
#' @param bundle An ontologySpecR bundle.
#' @param object_type_id Character string: the object type ID.
#' @return The matching object type, or NULL if not found.
#' @keywords internal
get_object_type <- function(bundle, object_type_id) {
  for (obj in bundle$objects) {
    if (identical(obj$id, object_type_id)) return(obj)
  }
  NULL
}

#' Build a named choices vector for object types
#'
#' Creates a named character vector suitable for selectInput choices,
#' where names are display names and values are IDs.
#'
#' @param bundle An ontologySpecR bundle.
#' @return A named character vector.
#' @keywords internal
object_type_choices <- function(bundle) {
  ids <- vapply(bundle$objects, function(o) o$id, character(1))
  names(ids) <- vapply(bundle$objects, display_name, character(1))
  ids
}

#' Find actions that target a given object type
#'
#' @param bundle An ontologySpecR bundle.
#' @param object_type_id Character string: the object type ID.
#' @return A list of action type objects that target this object type.
#' @keywords internal
find_actions_for_type <- function(bundle, object_type_id) {
  actions <- list()
  for (action in bundle$actions) {
    if (object_type_id %in% action$targets) {
      actions <- c(actions, list(action))
    }
  }
  actions
}

#' Get primary key property IDs for an object type
#'
#' @param object_type An ontologySpecR object_type.
#' @return A character vector of primary key property IDs.
#' @keywords internal
get_pk_columns <- function(object_type) {
  pk <- object_type$primaryKey
  if (is.null(pk)) return(character(0))
  pk$properties
}

#' Format a property value for display
#'
#' @param value The property value.
#' @param prop_type The ontology property type string.
#' @return A formatted character string.
#' @keywords internal
format_value <- function(value, prop_type) {
  if (is.null(value) || (length(value) == 1 && is.na(value))) {
    return(NA_character_)
  }
  switch(prop_type,
    boolean  = if (isTRUE(value)) "Yes" else "No",
    json     = jsonlite::toJSON(value, auto_unbox = TRUE),
    as.character(value)
  )
}
