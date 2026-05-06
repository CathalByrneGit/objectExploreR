#' Launch the interactive Ontology Object Explorer
#'
#' Opens a Shiny application that provides a visual interface for exploring
#' ontology objects. Users can search, filter, traverse links, inspect
#' properties, execute actions, and view graph neighbourhoods — all without
#' writing code.
#'
#' @param bundle An ontologySpecR bundle containing object types, link types,
#'   actions, and other ontology definitions.
#' @param connection A DBI connection object to the backing database.
#' @param action_ctx An ActionContext from actionTypesR (preferred method for
#'   action execution). If provided, actions use `actionTypesR::submit_action()`.
#' @param action_handlers A named list of R handler functions (deprecated).
#'   Names should match action type IDs from the bundle. Each handler receives
#'   `(connection, action, params, target_ids)`. Use `action_ctx` instead.
#' @param concept_cols_fn An optional function that returns concept evaluation
#'   columns for the current object type. Called as `concept_cols_fn(type_id)`
#'   and should return a data frame with `.pk` column and boolean concept columns.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object (returned invisibly). If running interactively,
#'   the app launches in the browser.
#'
#' @examples
#' \dontrun{
#' library(ontologySpecR)
#' library(DBI)
#' library(actionTypesR)
#'
#' b <- read_bundle(system.file("examples", "aviation-demo.json",
#'                               package = "ontologySpecR"))
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' # ... seed data ...
#'
#' # Preferred: use ActionContext
#' action_ctx <- action_context(b, con)
#' action_ctx <- register_handler(action_ctx, "UpdateAirportStatus", function(...) { ... })
#' explore_ontology(b, con, action_ctx = action_ctx)
#'
#' # Deprecated: use action_handlers directly
#' explore_ontology(b, con, action_handlers = list(
#'   UpdateAirportStatus = function(conn, action, params, targets) { ... }
#' ))
#' }
#'
#' @export
explore_ontology <- function(bundle, connection,
                              action_ctx = NULL,
                              action_handlers = list(),
                              concept_cols_fn = NULL,
                              ...) {

  if (!inherits(bundle, "ontology_bundle")) {
    stop("`bundle` must be an ontologySpecR bundle object.", call. = FALSE)
  }
  if (!inherits(connection, "DBIConnection")) {
    stop("`connection` must be a DBI connection object.", call. = FALSE)
  }
  if (length(bundle$objects) == 0) {
    stop("Bundle contains no object types.", call. = FALSE)
  }

  if (length(action_handlers) > 0 && is.null(action_ctx)) {
    warning(
      "The `action_handlers` argument is deprecated. ",
      "Use `action_ctx` (an ActionContext from actionTypesR) instead.",
      call. = FALSE
    )
  }

  if (is.null(action_ctx) && length(bundle$actions) > 0 && length(action_handlers) == 0) {
    warning(
      "Bundle defines actions but no action_ctx or action_handlers provided. ",
      "Actions will not execute.",
      call. = FALSE
    )
  }

  ui <- build_explorer_ui()

  server <- build_explorer_server(bundle, connection, action_ctx, action_handlers,
                                   concept_cols_fn)

  app <- shiny::shinyApp(ui = ui, server = server, ...)
  shiny::runApp(app)
}

#' Build the Explorer UI
#' @keywords internal
build_explorer_ui <- function() {
  shiny::fluidPage(
    theme = if (requireNamespace("bslib", quietly = TRUE)) {
      bslib::bs_theme(version = 5, bootswatch = "flatly")
    } else {
      NULL
    },

    shiny::tags$head(
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('update_input', function(msg) {
          Shiny.setInputValue(msg.id, msg.value);
        });
        Shiny.addCustomMessageHandler('toggle_dark_mode', function(msg) {
          document.documentElement.setAttribute('data-bs-theme', msg.dark ? 'dark' : 'light');
          var icon = document.getElementById('dark_mode_icon');
          if (icon) {
            icon.className = msg.dark ? 'fa fa-sun' : 'fa fa-moon';
          }
        });
      "))
    ),

    shiny::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      shiny::div(
        shiny::icon("search"),
        shiny::tags$strong("Ontology Object Explorer", style = "font-size: 20px; margin-left: 8px;")
      ),
      shiny::actionButton(
        "dark_mode_toggle",
        label = NULL,
        icon = shiny::icon("moon", id = "dark_mode_icon"),
        class = "btn-sm btn-outline-secondary"
      )
    ),

    shiny::div(
      style = "background-color: var(--bs-secondary-bg, #f8f9fa); padding: 10px; border-radius: 4px; margin-bottom: 15px;",
      type_selector_ui("type_selector")
    ),

    shiny::fluidRow(
      shiny::column(3,
        shiny::wellPanel(
          style = "max-height: 80vh; overflow-y: auto;",
          filter_panel_ui("filter_panel"),
          shiny::hr(),
          link_panel_ui("link_panel")
        )
      ),

      shiny::column(9,
        shiny::div(
          style = "min-height: 300px;",
          results_table_ui("results_table")
        ),
        shiny::hr(),
        detail_panel_ui("detail_panel")
      )
    )
  )
}

#' Build the Explorer Server function
#' @keywords internal
build_explorer_server <- function(bundle, connection, action_ctx, action_handlers,
                                   concept_cols_fn) {
  function(input, output, session) {
    bundle_r <- shiny::reactive(bundle)

    nav_target <- shiny::reactiveVal(NULL)

    link_filter_val <- shiny::reactiveVal(NULL)

    dark_mode <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$dark_mode_toggle, {
      dark_mode(!dark_mode())
      session$sendCustomMessage("toggle_dark_mode", list(dark = dark_mode()))
    })

    type_sel <- type_selector_server("type_selector", bundle_r, nav_target)

    active_filters <- filter_panel_server(
      "filter_panel", bundle_r, type_sel$selected_type_id, connection
    )

    concept_cols_r <- shiny::reactive({
      if (is.null(concept_cols_fn)) return(NULL)
      type_id <- type_sel$selected_type_id()
      if (is.null(type_id)) return(NULL)
      tryCatch(
        concept_cols_fn(type_id),
        error = function(e) NULL
      )
    })

    results <- results_table_server(
      "results_table", bundle_r, type_sel$selected_type_id,
      active_filters, type_sel$search_text, type_sel$refresh_trigger,
      connection, link_filter_val, concept_cols_r
    )

    link_panel <- link_panel_server(
      "link_panel", bundle_r, type_sel$selected_type_id,
      results$selected_row, results$selected_rows, results$current_data
    )

    shiny::observe({
      target <- link_panel$nav_target()
      if (!is.null(target)) {
        nav_target(target)
        link_panel$nav_target(NULL)
      }
    })

    shiny::observe({
      lf <- link_panel$link_filter()
      link_filter_val(lf)
    })

    shiny::observeEvent(type_sel$selected_type_id(), {
      if (is.null(nav_target())) {
        link_filter_val(NULL)
      }
    })

    detail_panel_server(
      "detail_panel", bundle_r, type_sel$selected_type_id,
      results$selected_row, connection, action_ctx, action_handlers, nav_target
    )
  }
}
