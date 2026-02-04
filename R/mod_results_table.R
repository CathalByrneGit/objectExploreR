#' Results Table Module
#'
#' Shiny module for the main results table showing filtered objects.
#' Supports sorting, pagination, row selection, and bulk selection.

#' Results Table UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tag list.
#' @keywords internal
results_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::textOutput(ns("result_info"))),
      shiny::column(6, shiny::textOutput(ns("query_time")))
    ),
    shiny::hr(),
    DT::DTOutput(ns("results_dt"))
  )
}

#' Results Table Server
#'
#' @param id Module namespace ID.
#' @param bundle Reactive expression returning the bundle.
#' @param selected_type_id Reactive expression returning the selected object type ID.
#' @param active_filters Reactive expression returning active filter list.
#' @param search_text Reactive expression returning search text.
#' @param refresh_trigger Reactive expression that triggers refresh.
#' @param connection The DBI connection object.
#' @param link_filter Reactive expression returning a link-based filter
#'   (from traversal). List with `link_type`, `source_ids`, `direction`.
#' @return A list of reactive expressions:
#'   \describe{
#'     \item{selected_row}{The selected row data as a named list, or NULL.}
#'     \item{selected_rows}{Indices of all selected rows (for bulk actions).}
#'     \item{current_data}{The current result data.frame.}
#'   }
#' @keywords internal
results_table_server <- function(id, bundle, selected_type_id, active_filters,
                                  search_text, refresh_trigger, connection,
                                  link_filter) {
  shiny::moduleServer(id, function(input, output, session) {

    # Current object type
    current_type <- shiny::reactive({
      req_type_id <- shiny::req(selected_type_id())
      get_object_type(bundle(), req_type_id)
    })

    # Fetch data reactively
    query_result <- shiny::reactive({
      obj_type <- current_type()
      shiny::req(obj_type)

      # Depend on refresh trigger
      refresh_trigger()

      filters <- active_filters()
      search <- search_text()
      lf <- link_filter()

      start_time <- proc.time()

      # If there's a link filter, use fetch_linked
      if (!is.null(lf)) {
        data <- tryCatch(
          fetch_linked(connection, bundle(), lf$link_type,
                       lf$source_ids, lf$direction),
          error = function(e) {
            data.frame()
          }
        )
      } else {
        data <- tryCatch(
          fetch_objects(connection, obj_type,
                        filters = filters,
                        search_text = search,
                        limit = 1000),
          error = function(e) {
            data.frame()
          }
        )
      }

      elapsed <- (proc.time() - start_time)[["elapsed"]]
      list(data = data, time = round(elapsed, 3))
    })

    # Result info
    output$result_info <- shiny::renderText({
      res <- query_result()
      paste(nrow(res$data), "objects found")
    })

    output$query_time <- shiny::renderText({
      res <- query_result()
      paste("Query time:", res$time, "s")
    })

    # Render DT
    output$results_dt <- DT::renderDT({
      res <- query_result()
      DT::datatable(
        res$data,
        selection = "single",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "ltipr"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # Selected row data
    selected_row <- shiny::reactive({
      idx <- input$results_dt_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      res <- query_result()
      if (nrow(res$data) == 0) return(NULL)
      as.list(res$data[idx[1], , drop = FALSE])
    })

    list(
      selected_row  = selected_row,
      selected_rows = shiny::reactive(input$results_dt_rows_selected),
      current_data  = shiny::reactive(query_result()$data)
    )
  })
}
