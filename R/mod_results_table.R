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
#' @param concept_cols Reactive expression returning concept evaluation columns
#'   (optional). Data frame with `.pk` column and boolean concept columns.
#' @return A list of reactive expressions:
#'   \describe{
#'     \item{selected_row}{The selected row data as a named list, or NULL.}
#'     \item{selected_rows}{Indices of all selected rows (for bulk actions).}
#'     \item{current_data}{The current result data.frame.}
#'   }
#' @keywords internal
results_table_server <- function(id, bundle, selected_type_id, active_filters,
                                  search_text, refresh_trigger, connection,
                                  link_filter,
                                  concept_cols = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Create objectSetsR context (lazy, reused)
    os_ctx <- shiny::reactive({
      b <- bundle()
      tryCatch(
        objectSetsR::ontology_context(b, connection),
        error = function(e) NULL
      )
    })

    # Current object type
    current_type <- shiny::reactive({
      req_type_id <- shiny::req(selected_type_id())
      get_object_type(bundle(), req_type_id)
    })

    # Fetch data reactively using objectSetsR
    query_result <- shiny::reactive({
      obj_type <- current_type()
      type_id <- selected_type_id()
      ctx <- os_ctx()
      shiny::req(obj_type, type_id, ctx)

      refresh_trigger()

      filters <- active_filters()
      search <- search_text()
      lf <- link_filter()

      start_time <- proc.time()

      data <- tryCatch({
        if (!is.null(lf)) {
          traverse_link_os(ctx, lf$link_type, lf$source_ids, lf$direction)
        } else {
          os <- objectSetsR::object_set(ctx, type_id)

          for (f in filters) {
            os <- apply_filter_to_os(os, f)
          }

          if (!is.null(search) && nzchar(search)) {
            os <- apply_search_to_os(os, bundle(), type_id, search)
          }

          df <- objectSetsR::os_collect(os)
          if (nrow(df) > 1000) df <- df[seq_len(1000), , drop = FALSE]
          df
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Query error:", conditionMessage(e)),
          type = "error"
        )
        data.frame()
      })

      elapsed <- (proc.time() - start_time)[["elapsed"]]
      list(data = data, time = round(elapsed, 3))
    })

    # Merge concept columns if provided
    result_with_concepts <- shiny::reactive({
      res <- query_result()
      cpt_cols <- concept_cols()

      if (is.null(cpt_cols) || nrow(cpt_cols) == 0 || nrow(res$data) == 0) {
        return(res$data)
      }

      obj_type <- current_type()
      pk_cols <- get_pk_columns(obj_type)
      if (length(pk_cols) == 0) return(res$data)

      pk_col <- pk_cols[1]
      if (!pk_col %in% names(res$data)) return(res$data)
      if (!".pk" %in% names(cpt_cols)) return(res$data)

      merged <- merge(
        res$data,
        cpt_cols,
        by.x = pk_col,
        by.y = ".pk",
        all.x = TRUE
      )
      merged
    })

    output$result_info <- shiny::renderText({
      res <- query_result()
      paste(nrow(res$data), "objects found")
    })

    output$query_time <- shiny::renderText({
      res <- query_result()
      paste("Query time:", res$time, "s")
    })

    output$results_dt <- DT::renderDT({
      data <- result_with_concepts()
      cpt_cols <- concept_cols()

      col_defs <- list()
      if (!is.null(cpt_cols) && ".pk" %in% names(cpt_cols)) {
        concept_col_names <- setdiff(names(cpt_cols), ".pk")
        for (cn in concept_col_names) {
          if (cn %in% names(data)) {
            col_idx <- which(names(data) == cn) - 1
            col_defs <- c(col_defs, list(
              list(
                targets = col_idx,
                render = DT::JS(
                  "function(data, type, row) {",
                  "  if (type === 'display') {",
                  "    if (data === true || data === 1) {",
                  "      return '<span class=\"badge bg-success\">Yes</span>';",
                  "    } else if (data === false || data === 0) {",
                  "      return '<span class=\"badge bg-danger\">No</span>';",
                  "    }",
                  "  }",
                  "  return data;",
                  "}"
                )
              )
            ))
          }
        }
      }

      DT::datatable(
        data,
        selection = "single",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "ltipr",
          columnDefs = col_defs
        ),
        rownames = FALSE,
        class = "compact stripe hover",
        escape = FALSE
      )
    })

    selected_row <- shiny::reactive({
      idx <- input$results_dt_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      data <- result_with_concepts()
      if (nrow(data) == 0) return(NULL)
      as.list(data[idx[1], , drop = FALSE])
    })

    list(
      selected_row  = selected_row,
      selected_rows = shiny::reactive(input$results_dt_rows_selected),
      current_data  = shiny::reactive(result_with_concepts())
    )
  })
}
