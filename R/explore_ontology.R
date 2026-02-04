#' Launch the interactive Ontology Object Explorer
#'
#' Opens a Shiny application that provides a visual interface for exploring
#' ontology objects. Users can search, filter, traverse links, inspect
#' properties, execute actions, and view graph neighborhoods — all without
#' writing code.
#'
#' @param bundle An ontologySpecR bundle containing object types, link types,
#'   actions, and other ontology definitions.
#' @param connection A DBI connection object to the backing database.
#' @param action_handlers An optional named list of R handler functions for
#'   executing actions. Names should match action type IDs from the bundle.
#'   Each handler receives `(connection, action, params, target_ids)`.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object (returned invisibly). If running interactively,
#'   the app launches in the browser.
#'
#' @examples
#' \dontrun{
#' library(ontologySpecR)
#' library(DBI)
#'
#' b <- read_bundle(system.file("examples", "aviation-demo.json",
#'                               package = "ontologySpecR"))
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' # ... seed data ...
#'
#' explore_ontology(b, con)
#' }
#'
#' @export
explore_ontology <- function(bundle, connection, action_handlers = list(), ...) {

  if (!inherits(bundle, "ontology_bundle")) {
    stop("`bundle` must be an ontologySpecR bundle object.", call. = FALSE)
  }
  if (!inherits(connection, "DBIConnection")) {
    stop("`connection` must be a DBI connection object.", call. = FALSE)
  }
  if (length(bundle$objects) == 0) {
    stop("Bundle contains no object types.", call. = FALSE)
  }

  # Build the UI
  ui <- build_explorer_ui()

  # Build the server
  server <- build_explorer_server(bundle, connection, action_handlers)

  app <- shiny::shinyApp(ui = ui, server = server, ...)
  shiny::runApp(app)
}

#' Build the Explorer UI
#' @keywords internal
build_explorer_ui <- function() {
  shiny::fluidPage(
    # Use bslib theme if available
    theme = if (requireNamespace("bslib", quietly = TRUE)) {
      bslib::bs_theme(version = 5, bootswatch = "flatly")
    } else {
      NULL
    },

    shiny::titlePanel(
      shiny::div(
        shiny::icon("search"),
        "Ontology Object Explorer",
        style = "font-size: 20px; font-weight: bold;"
      ),
      windowTitle = "Object Explorer"
    ),

    # Top bar: type selector + search
    shiny::div(
      style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
      type_selector_ui("type_selector")
    ),

    # Main layout
    shiny::fluidRow(
      # Left sidebar: filters + links
      shiny::column(3,
        shiny::wellPanel(
          style = "max-height: 80vh; overflow-y: auto;",
          filter_panel_ui("filter_panel"),
          shiny::hr(),
          link_panel_ui("link_panel")
        )
      ),

      # Main content: results + detail
      shiny::column(9,
        # Results table
        shiny::div(
          style = "min-height: 300px;",
          results_table_ui("results_table")
        ),
        shiny::hr(),
        # Detail panel (visible when row selected)
        detail_panel_ui("detail_panel")
      )
    ),

    # Custom JS for conditionalPanel support
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('update_input', function(msg) {
        Shiny.setInputValue(msg.id, msg.value);
      });
    "))
  )
}

#' Build the Explorer Server function
#' @keywords internal
build_explorer_server <- function(bundle, connection, action_handlers) {
  function(input, output, session) {
    # Wrap bundle as reactive (it's static, but modules expect reactive)
    bundle_r <- shiny::reactive(bundle)

    # Navigation target for programmatic type switching
    nav_target <- shiny::reactiveVal(NULL)

    # Link filter for traversal
    link_filter_val <- shiny::reactiveVal(NULL)

    # --- Type Selector ---
    type_sel <- type_selector_server("type_selector", bundle_r, nav_target)

    # --- Filter Panel ---
    active_filters <- filter_panel_server(
      "filter_panel", bundle_r, type_sel$selected_type_id, connection
    )

    # --- Results Table ---
    results <- results_table_server(
      "results_table", bundle_r, type_sel$selected_type_id,
      active_filters, type_sel$search_text, type_sel$refresh_trigger,
      connection, link_filter_val
    )

    # --- Link Panel ---
    link_panel <- link_panel_server(
      "link_panel", bundle_r, type_sel$selected_type_id,
      results$selected_row, results$selected_rows, results$current_data
    )

    # Wire link panel outputs
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

    # Clear link filter when type changes manually
    shiny::observeEvent(type_sel$selected_type_id(), {
      # Only clear if not from traversal
      if (is.null(nav_target())) {
        link_filter_val(NULL)
      }
    })

    # --- Detail Panel ---
    detail_panel_server(
      "detail_panel", bundle_r, type_sel$selected_type_id,
      results$selected_row, connection, action_handlers, nav_target
    )
  }
}
