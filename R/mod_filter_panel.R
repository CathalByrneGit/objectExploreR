#' Filter Panel Module
#'
#' Shiny module that auto-generates filter widgets from an object type's
#' properties. Supports text, enum dropdown, numeric range, boolean tri-state,
#' and date range filters.

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

    os_ctx <- shiny::reactive({
      b <- bundle()
      tryCatch(
        objectSetsR::ontology_context(b, connection),
        error = function(e) NULL
      )
    })

    current_type <- shiny::reactive({
      req_type_id <- shiny::req(selected_type_id())
      get_object_type(bundle(), req_type_id)
    })

    filter_registry <- shiny::reactiveVal(list())

    output$filter_widgets <- shiny::renderUI({
      obj_type <- current_type()
      type_id <- selected_type_id()
      ctx <- os_ctx()
      if (is.null(obj_type) || is.null(ctx)) return(NULL)

      widgets <- list()
      registry <- list()

      for (prop in obj_type$properties) {
        prop_type <- prop$type
        input_id <- paste0("filter_", prop$id)
        label <- display_name(prop)

        widget_info <- create_filter_widget(
          ns, input_id, label, prop$id, prop_type, ctx, type_id
        )

        if (!is.null(widget_info)) {
          registry[[prop$id]] <- widget_info$registry
          widgets <- c(widgets, list(widget_info$widget))
        }
      }

      filter_registry(registry)
      shiny::tagList(widgets)
    })

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
          select_multi = val,
          numeric_range = list(min = val[1], max = val[2]),
          checkbox = val,
          boolean_tri = val,
          date_range = list(start = val[1], end = val[2]),
          NULL
        )

        filters <- c(filters, list(filter_spec))
      }

      active_filters(filters)
    })

    shiny::observeEvent(input$clear_filters, {
      active_filters(list())

      registry <- filter_registry()
      for (reg in registry) {
        switch(reg$widget_type,
          text = shiny::updateTextInput(session, reg$input_id, value = ""),
          select = shiny::updateSelectInput(session, reg$input_id, selected = ""),
          select_multi = shiny::updateSelectizeInput(session, reg$input_id, selected = character(0)),
          checkbox = shiny::updateCheckboxInput(session, reg$input_id, value = FALSE),
          boolean_tri = shiny::updateRadioButtons(session, reg$input_id, selected = "any"),
          NULL
        )
      }
    })

    shiny::observeEvent(selected_type_id(), {
      active_filters(list())
    })

    active_filters
  })
}

#' Create a filter widget for a property
#'
#' Determines the appropriate widget type based on property type and data.
#'
#' @param ns Namespace function.
#' @param input_id The input ID.
#' @param label Display label.
#' @param property_id Property ID.
#' @param prop_type Property type string.
#' @param ctx objectSetsR context.
#' @param type_id Object type ID.
#' @return A list with `widget` and `registry`, or NULL if no widget.
#' @keywords internal
create_filter_widget <- function(ns, input_id, label, property_id, prop_type,
                                  ctx, type_id) {
  if (prop_type == "json") return(NULL)

  if (prop_type == "string") {
    distinct_vals <- tryCatch(
      fetch_distinct_os(ctx, type_id, property_id, limit = 50),
      error = function(e) character(0)
    )

    if (length(distinct_vals) > 0 && length(distinct_vals) <= 20) {
      return(list(
        widget = shiny::selectizeInput(
          ns(input_id), label,
          choices = c("All" = "", distinct_vals),
          multiple = TRUE,
          options = list(placeholder = "Select values..."),
          width = "100%"
        ),
        registry = list(
          input_id = input_id,
          property_id = property_id,
          widget_type = "select_multi"
        )
      ))
    } else {
      return(list(
        widget = shiny::textInput(
          ns(input_id), label,
          placeholder = paste("Filter", label, "..."),
          width = "100%"
        ),
        registry = list(
          input_id = input_id,
          property_id = property_id,
          widget_type = "text"
        )
      ))
    }
  }

  if (prop_type %in% c("integer", "number")) {
    range_vals <- tryCatch(
      fetch_range_os(ctx, type_id, property_id),
      error = function(e) list(min = 0, max = 100)
    )
    min_val <- if (is.null(range_vals$min) || is.na(range_vals$min)) 0 else range_vals$min
    max_val <- if (is.null(range_vals$max) || is.na(range_vals$max)) 100 else range_vals$max
    if (min_val == max_val) max_val <- min_val + 1

    step <- if (prop_type == "integer") 1 else (max_val - min_val) / 100

    return(list(
      widget = shiny::sliderInput(
        ns(input_id), label,
        min = min_val, max = max_val,
        value = c(min_val, max_val),
        step = step,
        width = "100%"
      ),
      registry = list(
        input_id = input_id,
        property_id = property_id,
        widget_type = "numeric_range"
      )
    ))
  }

  if (prop_type == "boolean") {
    return(list(
      widget = shiny::radioButtons(
        ns(input_id), label,
        choices = c("Any" = "any", "True" = "TRUE", "False" = "FALSE"),
        selected = "any",
        inline = TRUE,
        width = "100%"
      ),
      registry = list(
        input_id = input_id,
        property_id = property_id,
        widget_type = "boolean_tri"
      )
    ))
  }

  if (prop_type %in% c("date", "datetime")) {
    return(list(
      widget = shiny::dateRangeInput(
        ns(input_id), label,
        width = "100%"
      ),
      registry = list(
        input_id = input_id,
        property_id = property_id,
        widget_type = "date_range"
      )
    ))
  }

  NULL
}
