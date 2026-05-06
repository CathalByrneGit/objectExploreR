#' Detail Panel Module
#'
#' Shiny module for the detail view shown when a row is selected.
#' Contains tabs for properties, links, actions, and graph neighbourhood.

#' Detail Panel UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tag list.
#' @keywords internal
detail_panel_ui <- function(id) {

  ns <- shiny::NS(id)
  shiny::conditionalPanel(
    condition = paste0("typeof input['", ns("has_selection"), "'] !== 'undefined' && input['", ns("has_selection"), "']"),
    shiny::tabsetPanel(
      id = ns("detail_tabs"),
      shiny::tabPanel("Properties",
        shiny::div(style = "padding: 10px;",
          shiny::tableOutput(ns("properties_table"))
        )
      ),
      shiny::tabPanel("Links",
        shiny::div(style = "padding: 10px;",
          shiny::uiOutput(ns("links_content"))
        )
      ),
      shiny::tabPanel("Actions",
        shiny::div(style = "padding: 10px;",
          shiny::uiOutput(ns("actions_content"))
        )
      ),
      shiny::tabPanel("Graph",
        shiny::div(style = "padding: 10px;",
          shiny::uiOutput(ns("graph_content"))
        )
      )
    )
  )
}

#' Detail Panel Server
#'
#' @param id Module namespace ID.
#' @param bundle Reactive expression returning the bundle.
#' @param selected_type_id Reactive expression returning selected type ID.
#' @param selected_row Reactive expression returning the selected row data.
#' @param connection The DBI connection object.
#' @param action_ctx ActionContext from actionTypesR (preferred) or NULL.
#' @param action_handlers Named list of R handler functions (deprecated fallback).
#' @param nav_target ReactiveVal for programmatic navigation.
#' @keywords internal
detail_panel_server <- function(id, bundle, selected_type_id, selected_row,
                                 connection, action_ctx, action_handlers, nav_target) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    os_ctx <- shiny::reactive({
      b <- bundle()
      tryCatch(
        objectSetsR::ontology_context(b, connection),
        error = function(e) NULL
      )
    })

    shiny::observe({
      has_sel <- !is.null(selected_row())
      session$sendCustomMessage("update_input",
        list(id = ns("has_selection"), value = has_sel))
    })

    has_selection <- shiny::reactive(!is.null(selected_row()))

    current_type <- shiny::reactive({
      type_id <- shiny::req(selected_type_id())
      get_object_type(bundle(), type_id)
    })

    output$properties_table <- shiny::renderTable({
      row <- shiny::req(selected_row())
      obj_type <- current_type()
      if (is.null(obj_type)) return(data.frame())

      props <- data.frame(
        Property = character(0),
        Value = character(0),
        stringsAsFactors = FALSE
      )

      for (prop in obj_type$properties) {
        col_name <- prop$id
        value <- row[[col_name]]
        props <- rbind(props, data.frame(
          Property = display_name(prop),
          Value = format_value(value, prop$type),
          stringsAsFactors = FALSE
        ))
      }

      props
    }, striped = TRUE, hover = TRUE, width = "100%")

    output$links_content <- shiny::renderUI({
      row <- shiny::req(selected_row())
      obj_type <- current_type()
      b <- bundle()
      type_id <- selected_type_id()
      ctx <- os_ctx()
      if (is.null(obj_type) || is.null(ctx)) return(NULL)

      pk_cols <- get_pk_columns(obj_type)
      if (length(pk_cols) == 0) return(shiny::p("No primary key defined."))
      pk_col <- pk_cols[1]
      pk_value <- as.character(row[[pk_col]])

      links <- find_links_for_type(b, type_id)
      sections <- list()

      for (link in links$outgoing) {
        target_type <- get_object_type(b, link$to)
        link_label <- display_name(link)
        target_label <- if (!is.null(target_type)) display_name(target_type) else link$to

        linked_data <- tryCatch(
          traverse_link_os(ctx, link, pk_value, direction = "forward"),
          error = function(e) data.frame()
        )

        section <- shiny::div(
          shiny::h5(paste0(link_label, " -> ", target_label)),
          if (nrow(linked_data) > 0) {
            DT::DTOutput(ns(paste0("link_out_", link$id)))
          } else {
            shiny::p("No linked objects.", class = "text-muted")
          }
        )
        sections <- c(sections, list(section))

        if (nrow(linked_data) > 0) {
          local({
            ld <- linked_data
            lid <- link$id
            output[[paste0("link_out_", lid)]] <- DT::renderDT(
              DT::datatable(
                ld,
                selection = "none",
                options = list(pageLength = 5, dom = "tp", scrollX = TRUE),
                rownames = FALSE,
                class = "compact"
              )
            )
          })
        }
      }

      for (link in links$incoming) {
        source_type <- get_object_type(b, link$from)
        link_label <- display_name(link)
        source_label <- if (!is.null(source_type)) display_name(source_type) else link$from

        linked_data <- tryCatch(
          traverse_link_os(ctx, link, pk_value, direction = "reverse"),
          error = function(e) data.frame()
        )

        section <- shiny::div(
          shiny::h5(paste0(source_label, " <- ", link_label)),
          if (nrow(linked_data) > 0) {
            DT::DTOutput(ns(paste0("link_in_", link$id)))
          } else {
            shiny::p("No linked objects.", class = "text-muted")
          }
        )
        sections <- c(sections, list(section))

        if (nrow(linked_data) > 0) {
          local({
            ld <- linked_data
            lid <- link$id
            output[[paste0("link_in_", lid)]] <- DT::renderDT(
              DT::datatable(
                ld,
                selection = "none",
                options = list(pageLength = 5, dom = "tp", scrollX = TRUE),
                rownames = FALSE,
                class = "compact"
              )
            )
          })
        }
      }

      if (length(sections) == 0) {
        return(shiny::p("No links for this object type.", class = "text-muted"))
      }

      shiny::tagList(sections)
    })

    output$actions_content <- shiny::renderUI({
      row <- shiny::req(selected_row())
      obj_type <- current_type()
      b <- bundle()
      type_id <- selected_type_id()
      if (is.null(obj_type)) return(NULL)

      actions <- find_actions_for_type(b, type_id)
      if (length(actions) == 0) {
        return(shiny::p("No actions available for this type.", class = "text-muted"))
      }

      pk_cols <- get_pk_columns(obj_type)
      pk_col <- if (length(pk_cols) > 0) pk_cols[1] else NULL
      pk_value <- if (!is.null(pk_col)) as.character(row[[pk_col]]) else NULL

      action_widgets <- lapply(seq_along(actions), function(i) {
        action <- actions[[i]]
        action_form_ui(ns(paste0("action_", i)), action)
      })

      shiny::tagList(action_widgets)
    })

    shiny::observe({
      b <- bundle()
      type_id <- selected_type_id()
      if (is.null(type_id)) return()

      actions <- find_actions_for_type(b, type_id)
      for (i in seq_along(actions)) {
        local({
          idx <- i
          action <- actions[[idx]]
          action_form_server(
            paste0("action_", idx), action, connection,
            action_ctx, action_handlers, selected_row, current_type
          )
        })
      }
    })

    output$graph_content <- shiny::renderUI({
      row <- shiny::req(selected_row())
      obj_type <- current_type()
      b <- bundle()

      if (!requireNamespace("vertexR", quietly = TRUE)) {
        return(shiny::p(
          "Install vertexR for graph visualization: ",
          shiny::code('remotes::install_github("CathalByrneGit/vertexR")'),
          class = "text-muted"
        ))
      }

      if (!requireNamespace("visNetwork", quietly = TRUE)) {
        return(shiny::p(
          "Install visNetwork for graph visualization: ",
          shiny::code('install.packages("visNetwork")'),
          class = "text-muted"
        ))
      }

      if (length(b$links) == 0) {
        return(shiny::p("No links defined in bundle.", class = "text-muted"))
      }

      pk_cols <- get_pk_columns(obj_type)
      if (length(pk_cols) == 0) {
        return(shiny::p("No primary key defined for graph view."))
      }
      pk_col <- pk_cols[1]
      pk_value <- as.character(row[[pk_col]])

      tryCatch({
        g <- vertexR::vertex_graph(b, connection)
        node_id <- paste0(selected_type_id(), ":", pk_value)
        sub_g <- vertexR::vx_neighbors(g, node_id, depth = 2L)
        vertexR::vx_plot(sub_g)
      }, error = function(e) {
        shiny::p(
          paste("Graph visualization unavailable:", e$message),
          class = "text-muted"
        )
      })
    })
  })
}
