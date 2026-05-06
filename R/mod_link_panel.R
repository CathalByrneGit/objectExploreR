#' Link Panel Module
#'
#' Shiny module for the link traversal sidebar. Shows available links
#' for the current object type with Traverse and Search Around buttons.

#' Link Panel UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tag list.
#' @keywords internal
link_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Links"),
    shiny::uiOutput(ns("link_controls")),
    shiny::hr(),
    shiny::actionButton(ns("clear_link_filter"), "Clear Link Filter",
                        class = "btn-default btn-sm", width = "100%")
  )
}

#' Link Panel Server
#'
#' @param id Module namespace ID.
#' @param bundle Reactive expression returning the bundle.
#' @param selected_type_id Reactive expression returning selected object type ID.
#' @param selected_row Reactive expression returning the selected row data.
#' @param selected_rows Reactive expression returning selected row indices.
#' @param current_data Reactive expression returning the current result data.frame.
#' @return A list:
#'   \describe{
#'     \item{nav_target}{ReactiveVal - object_type_id to navigate to.}
#'     \item{link_filter}{ReactiveVal - list with link_type, source_ids, direction.}
#'   }
#' @keywords internal
link_panel_server <- function(id, bundle, selected_type_id, selected_row,
                               selected_rows, current_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    nav_target <- shiny::reactiveVal(NULL)
    link_filter <- shiny::reactiveVal(NULL)

    current_links <- shiny::reactive({
      type_id <- shiny::req(selected_type_id())
      find_links_for_type(bundle(), type_id)
    })

    output$link_controls <- shiny::renderUI({
      links <- current_links()
      b <- bundle()
      widgets <- list()

      if (length(links$outgoing) > 0) {
        widgets <- c(widgets, list(shiny::h5("Outgoing Links")))
        for (i in seq_along(links$outgoing)) {
          link <- links$outgoing[[i]]
          target_type <- get_object_type(b, link$to)
          link_label <- display_name(link)
          target_label <- if (!is.null(target_type)) display_name(target_type) else link$to

          btn_id <- paste0("traverse_out_", i)
          widgets <- c(widgets, list(
            shiny::div(
              class = "link-item",
              style = "margin-bottom: 8px;",
              shiny::tags$strong(link_label),
              shiny::tags$br(),
              shiny::tags$small(
                paste0("-> ", target_label, " (", link$cardinality, ")")
              ),
              shiny::tags$br(),
              shiny::actionButton(ns(btn_id), "Traverse ->",
                                  class = "btn-info btn-xs",
                                  style = "margin-top: 4px;")
            )
          ))
        }
      }

      if (length(links$incoming) > 0) {
        widgets <- c(widgets, list(shiny::h5("Incoming Links")))
        for (i in seq_along(links$incoming)) {
          link <- links$incoming[[i]]
          source_type <- get_object_type(b, link$from)
          link_label <- display_name(link)
          source_label <- if (!is.null(source_type)) display_name(source_type) else link$from

          btn_id <- paste0("traverse_in_", i)
          widgets <- c(widgets, list(
            shiny::div(
              class = "link-item",
              style = "margin-bottom: 8px;",
              shiny::tags$strong(link_label),
              shiny::tags$br(),
              shiny::tags$small(
                paste0("<- ", source_label, " (", link$cardinality, ")")
              ),
              shiny::tags$br(),
              shiny::actionButton(ns(btn_id), "<- Search Around",
                                  class = "btn-warning btn-xs",
                                  style = "margin-top: 4px;")
            )
          ))
        }
      }

      if (length(widgets) == 0) {
        widgets <- list(shiny::p("No links for this type.", class = "text-muted"))
      }

      shiny::tagList(widgets)
    })

    shiny::observe({
      links <- current_links()
      for (i in seq_along(links$outgoing)) {
        local({
          idx <- i
          btn_id <- paste0("traverse_out_", idx)
          shiny::observeEvent(input[[btn_id]], {
            link <- current_links()$outgoing[[idx]]
            source_ids <- get_selected_pk_values(
              bundle(), selected_type_id(), selected_row(),
              selected_rows(), current_data()
            )
            if (length(source_ids) > 0) {
              link_filter(list(
                link_type = link,
                source_ids = source_ids,
                direction = "forward"
              ))
              nav_target(link$to)
            }
          }, ignoreInit = TRUE)
        })
      }
    })

    shiny::observe({
      links <- current_links()
      for (i in seq_along(links$incoming)) {
        local({
          idx <- i
          btn_id <- paste0("traverse_in_", idx)
          shiny::observeEvent(input[[btn_id]], {
            link <- current_links()$incoming[[idx]]
            source_ids <- get_selected_pk_values(
              bundle(), selected_type_id(), selected_row(),
              selected_rows(), current_data()
            )
            if (length(source_ids) > 0) {
              link_filter(list(
                link_type = link,
                source_ids = source_ids,
                direction = "reverse"
              ))
              nav_target(link$from)
            }
          }, ignoreInit = TRUE)
        })
      }
    })

    shiny::observeEvent(input$clear_link_filter, {
      link_filter(NULL)
    })

    list(
      nav_target = nav_target,
      link_filter = link_filter
    )
  })
}

#' Get primary key values for selected rows
#' @keywords internal
get_selected_pk_values <- function(bundle, type_id, selected_row,
                                    selected_rows, data) {
  obj_type <- get_object_type(bundle, type_id)
  if (is.null(obj_type)) return(character(0))

  pk_cols <- get_pk_columns(obj_type)
  if (length(pk_cols) == 0) return(character(0))

  pk_col <- pk_cols[1]

  if (!is.null(selected_row) && pk_col %in% names(selected_row)) {
    return(as.character(selected_row[[pk_col]]))
  }

  if (!is.null(selected_rows) && length(selected_rows) > 0 &&
      !is.null(data) && nrow(data) > 0 && pk_col %in% names(data)) {
    return(as.character(data[selected_rows, pk_col]))
  }

  character(0)
}
