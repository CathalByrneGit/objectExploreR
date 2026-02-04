#' Action Form Module
#'
#' Shiny module that renders a form for an action type's parameters
#' and handles submission via actionTypesR.

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
#' @param action_handlers Named list of R handler functions.
#' @param selected_row Reactive expression for the selected row.
#' @param current_type Reactive expression for the current object type.
#' @keywords internal
action_form_server <- function(id, action, connection, action_handlers,
                                selected_row, current_type) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render parameter inputs
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

    # Handle submit
    shiny::observeEvent(input$submit_action, {
      row <- selected_row()
      obj_type <- current_type()
      if (is.null(row) || is.null(obj_type)) return()

      # Collect parameter values
      params <- list()
      for (param in action$parameters) {
        input_id <- paste0("param_", param$id)
        val <- input[[input_id]]
        if (!is.null(val) && nzchar(as.character(val))) {
          params[[param$id]] <- val
        }
      }

      # Get target PK value
      pk_cols <- get_pk_columns(obj_type)
      pk_col <- if (length(pk_cols) > 0) {
        property_column(
          Filter(function(p) p$id == pk_cols[1], obj_type$properties)[[1]]
        )
      } else {
        NULL
      }
      target_ids <- if (!is.null(pk_col)) list(as.character(row[[pk_col]])) else list()

      # Try actionTypesR first, fall back to direct handler
      result <- tryCatch({
        if (requireNamespace("actionTypesR", quietly = TRUE)) {
          ctx <- actionTypesR::action_context(shiny::isolate(shiny::reactiveValuesToList(input)),
                                               connection)
          # Register handlers
          for (h_name in names(action_handlers)) {
            ctx <- actionTypesR::register_handler(ctx, h_name, action_handlers[[h_name]])
          }
          res <- actionTypesR::submit_action(ctx, action$id, params, target_ids)
          paste("Action completed:", res$status)
        } else if (action$id %in% names(action_handlers)) {
          # Direct handler execution
          handler <- action_handlers[[action$id]]
          handler(connection, action, params, target_ids)
          "Action executed successfully."
        } else {
          "No handler registered for this action. Install actionTypesR or provide a handler."
        }
      }, error = function(e) {
        paste("Error:", e$message)
      })

      output$action_result <- shiny::renderText(result)
    })
  })
}
