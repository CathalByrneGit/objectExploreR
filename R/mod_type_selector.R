#' Type Selector Module
#'
#' Shiny module for the top bar: object type dropdown, search box, and refresh.

#' Type Selector UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tag list.
#' @keywords internal
type_selector_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4,
      shiny::selectInput(
        ns("object_type"),
        label = NULL,
        choices = NULL,
        width = "100%"
      )
    ),
    shiny::column(6,
      shiny::textInput(
        ns("search_text"),
        label = NULL,
        placeholder = "Search across all text fields...",
        width = "100%"
      )
    ),
    shiny::column(2,
      shiny::actionButton(
        ns("refresh"),
        label = "Refresh",
        icon = shiny::icon("refresh"),
        width = "100%",
        class = "btn-primary"
      )
    )
  )
}

#' Type Selector Server
#'
#' @param id Module namespace ID.
#' @param bundle Reactive expression returning the ontologySpecR bundle.
#' @param nav_target Reactive value for programmatic type selection (from link
#'   traversal). Should be a reactiveVal containing an object_type_id or NULL.
#' @return A list of reactive expressions:
#'   \describe{
#'     \item{selected_type_id}{The currently selected object type ID.}
#'     \item{search_text}{The current search text.}
#'     \item{refresh_trigger}{A counter incremented on each refresh click.}
#'   }
#' @keywords internal
type_selector_server <- function(id, bundle, nav_target) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate choices from bundle
    shiny::observe({
      b <- bundle()
      choices <- object_type_choices(b)
      shiny::updateSelectInput(session, "object_type", choices = choices)
    })

    # Handle programmatic navigation from link traversal
    shiny::observe({
      target <- nav_target()
      if (!is.null(target)) {
        shiny::updateSelectInput(session, "object_type", selected = target)
        nav_target(NULL)
      }
    })

    # Debounce search text
    search_debounced <- shiny::debounce(
      shiny::reactive(input$search_text),
      millis = 500
    )

    # Refresh counter
    refresh_count <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$refresh, {
      refresh_count(refresh_count() + 1L)
    })

    list(
      selected_type_id = shiny::reactive(input$object_type),
      search_text      = search_debounced,
      refresh_trigger  = refresh_count
    )
  })
}
