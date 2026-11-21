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
#' Reads the bundle's link types through ontologySpecR and returns links where the given object type
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
  for (link in unname(ontologySpecR::bundle_link_types(bundle))) {
    ends <- ontologySpecR::link_endpoints(link)
    if (identical(ends$from, object_type_id)) {
      outgoing <- c(outgoing, list(link))
    }
    if (identical(ends$to, object_type_id)) {
      incoming <- c(incoming, list(link))
    }
  }
  list(outgoing = outgoing, incoming = incoming)
}

#' Get an object type from a bundle by ID
#'
#' Unlike `ontologySpecR::bundle_object_type()`, an unknown id returns `NULL`
#' instead of aborting, so UI code can degrade to showing the raw id.
#'
#' @param bundle An ontologySpecR bundle.
#' @param object_type_id Character string: the object type ID.
#' @return The matching object type, or NULL if not found.
#' @keywords internal
get_object_type <- function(bundle, object_type_id) {
  if (is.null(object_type_id) || !nzchar(object_type_id)) return(NULL)
  ontologySpecR::bundle_object_types(bundle)[[object_type_id]]
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
  types <- ontologySpecR::bundle_object_types(bundle)
  ids <- names(types)
  if (is.null(ids)) ids <- character(0)
  labels <- vapply(types, display_name, character(1), USE.NAMES = FALSE)
  stats::setNames(ids, labels)
}

#' Find actions that target a given object type
#'
#' @param bundle An ontologySpecR bundle.
#' @param object_type_id Character string: the object type ID.
#' @return A list of action type objects that target this object type.
#' @keywords internal
find_actions_for_type <- function(bundle, object_type_id) {
  actions <- list()
  for (action in unname(ontologySpecR::bundle_action_types(bundle))) {
    if (object_type_id %in% unlist(action$targets)) {
      actions <- c(actions, list(action))
    }
  }
  actions
}

#' Get primary key property IDs for an object type
#'
#' @param object_type An ontologySpecR object_type.
#' @return A character vector of primary key property IDs (empty if the type
#'   declares no primary key).
#' @keywords internal
get_pk_columns <- function(object_type) {
  tryCatch(
    ontologySpecR::object_pk(object_type),
    error = function(e) character(0)
  )
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
