#' Action Form Module
#'
#' Shiny module that renders a form for an action type's parameters
#' and handles submission via actionTypesR (preferred) or direct handlers.

#' Action Form UI
#'
#' @param id Module namespace ID.
#' @param action An ontologySpecR action_type object.
#' @return A Shiny tag list.
#' @keywords internal
action_form_ui <- function(id, action) {
  ns <- shiny::NS(id)
  shiny::wellPanel(
    shiny::h5(display_name(action)),
    if (!is.null(action$display$description)) {
      shiny::p(action$display$description, class = "text-muted")
    },
    shiny::uiOutput(ns("param_inputs")),
    shiny::actionButton(ns("submit_action"), "Submit",
                        class = "btn-success btn-sm"),
    shiny::textOutput(ns("action_result"))
  )
}

#' Action Form Server
#'
#' @param id Module namespace ID.
#' @param action An ontologySpecR action_type object.
#' @param connection The DBI connection object.
#' @param action_ctx ActionContext from actionTypesR, or NULL (actions then
#'   cannot execute and the form says so).
#' @param selected_row Reactive expression for the selected row.
#' @param current_type Reactive expression for the current object type.
#' @keywords internal
action_form_server <- function(id, action, connection, action_ctx,
                                selected_row, current_type) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$param_inputs <- shiny::renderUI({
      params <- action$parameters
      if (length(params) == 0) {
        return(shiny::p("No parameters required.", class = "text-muted"))
      }

      inputs <- lapply(params, function(param) {
        input_id <- paste0("param_", param$id)
        label <- display_name(param)
        if (isTRUE(param$required)) label <- paste0(label, " *")

        widget_type <- property_type_to_widget(param$type)

        switch(widget_type,
          text = shiny::textInput(ns(input_id), label, width = "100%"),
          numeric_range = shiny::numericInput(ns(input_id), label, value = 0, width = "100%"),
          checkbox = shiny::checkboxInput(ns(input_id), label, value = FALSE),
          date_range = shiny::dateInput(ns(input_id), label, width = "100%"),
          shiny::textInput(ns(input_id), label, width = "100%")
        )
      })

      shiny::tagList(inputs)
    })

    shiny::observeEvent(input$submit_action, {
      row <- selected_row()
      obj_type <- current_type()
      if (is.null(row) || is.null(obj_type)) return()

      params <- list()
      for (param in action$parameters) {
        input_id <- paste0("param_", param$id)
        val <- input[[input_id]]
        if (!is.null(val) && nzchar(as.character(val))) {
          params[[param$id]] <- val
        }
      }

      pk_cols <- get_pk_columns(obj_type)
      pk_col <- if (length(pk_cols) > 0) pk_cols[1] else NULL
      target_ids <- if (!is.null(pk_col) && pk_col %in% names(row)) {
        list(as.character(row[[pk_col]]))
      } else {
        list()
      }

      result <- tryCatch({
        if (is.null(action_ctx)) {
          "No action context available. Pass `action_ctx` to explore_ontology()."
        } else {
          res <- actionTypesR::submit_action(
            ctx = action_ctx,
            action_type_id = action$id,
            params = params,
            target_ids = target_ids
          )
          paste("Action completed:", res$status)
        }
      }, error = function(e) {
        paste("Error:", conditionMessage(e))
      })

      output$action_result <- shiny::renderText(result)
    })
  })
}
