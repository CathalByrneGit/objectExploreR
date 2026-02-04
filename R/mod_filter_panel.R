#' Filter Panel Module
#'
#' Shiny module that auto-generates filter widgets from an object type's
#' properties. Supports text, numeric range, boolean, and date range filters.

#' Filter Panel UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tag list.
#' @keywords internal
filter_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Filters"),
    shiny::uiOutput(ns("filter_widgets")),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("apply_filters"), "Apply Filters",
                            class = "btn-primary btn-sm", width = "100%")
      ),
      shiny::column(6,
        shiny::actionButton(ns("clear_filters"), "Clear Filters",
                            class = "btn-default btn-sm", width = "100%")
      )
    )
  )
}

#' Filter Panel Server
#'
#' @param id Module namespace ID.
#' @param bundle Reactive expression returning the bundle.
#' @param selected_type_id Reactive expression returning the selected object
#'   type ID.
#' @param connection The DBI connection object.
#' @return A reactive expression returning a list of active filter specs.
#' @keywords internal
filter_panel_server <- function(id, bundle, selected_type_id, connection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Current object type
    current_type <- shiny::reactive({
      req_type_id <- shiny::req(selected_type_id())
      get_object_type(bundle(), req_type_id)
    })

    # Track which filter inputs exist
    filter_registry <- shiny::reactiveVal(list())

    # Render filter widgets dynamically based on object type properties
    output$filter_widgets <- shiny::renderUI({
      obj_type <- current_type()
      if (is.null(obj_type)) return(NULL)

      widgets <- list()
      registry <- list()

      for (prop in obj_type$properties) {
        widget_type <- property_type_to_widget(prop$type)
        if (widget_type == "none") next

        input_id <- paste0("filter_", prop$id)
        label <- display_name(prop)
        registry[[prop$id]] <- list(
          input_id = input_id,
          property_id = prop$id,
          widget_type = widget_type
        )

        widget <- switch(widget_type,
          text = {
            # Try to get distinct values for dropdown
            distinct_vals <- tryCatch(
              fetch_distinct_values(connection, obj_type, prop$id, limit = 50),
              error = function(e) character(0)
            )
            if (length(distinct_vals) > 0 && length(distinct_vals) <= 20) {
              # Use selectInput for few values
              registry[[prop$id]]$widget_type <- "select"
              shiny::selectInput(ns(input_id), label,
                choices = c("All" = "", distinct_vals),
                width = "100%"
              )
            } else {
              shiny::textInput(ns(input_id), label,
                placeholder = paste("Filter", label, "..."),
                width = "100%"
              )
            }
          },
          numeric_range = {
            range_vals <- tryCatch(
              fetch_range(connection, obj_type, prop$id),
              error = function(e) list(min = 0, max = 100)
            )
            min_val <- if (is.null(range_vals$min) || is.na(range_vals$min)) 0 else range_vals$min
            max_val <- if (is.null(range_vals$max) || is.na(range_vals$max)) 100 else range_vals$max
            if (min_val == max_val) max_val <- min_val + 1
            shiny::sliderInput(ns(input_id), label,
              min = min_val, max = max_val,
              value = c(min_val, max_val),
              width = "100%"
            )
          },
          checkbox = {
            shiny::checkboxInput(ns(input_id), label, value = FALSE, width = "100%")
          },
          date_range = {
            shiny::dateRangeInput(ns(input_id), label, width = "100%")
          }
        )
        widgets <- c(widgets, list(widget))
      }

      filter_registry(registry)
      shiny::tagList(widgets)
    })

    # Collect active filters when Apply is clicked
    active_filters <- shiny::reactiveVal(list())

    shiny::observeEvent(input$apply_filters, {
      registry <- filter_registry()
      filters <- list()

      for (reg in registry) {
        val <- input[[reg$input_id]]
        if (is.null(val)) next

        filter_spec <- list(
          property_id = reg$property_id,
          type = reg$widget_type
        )

        filter_spec$value <- switch(reg$widget_type,
          text = val,
          select = val,
          numeric_range = list(min = val[1], max = val[2]),
          checkbox = val,
          date_range = list(start = val[1], end = val[2]),
          NULL
        )

        filters <- c(filters, list(filter_spec))
      }

      active_filters(filters)
    })

    # Clear filters
    shiny::observeEvent(input$clear_filters, {
      active_filters(list())

      # Reset object type to trigger widget re-render
      registry <- filter_registry()
      for (reg in registry) {
        switch(reg$widget_type,
          text = shiny::updateTextInput(session, reg$input_id, value = ""),
          select = shiny::updateSelectInput(session, reg$input_id, selected = ""),
          checkbox = shiny::updateCheckboxInput(session, reg$input_id, value = FALSE),
          NULL
        )
      }
    })

    # Also reset when object type changes
    shiny::observeEvent(selected_type_id(), {
      active_filters(list())
    })

    active_filters
  })
}
