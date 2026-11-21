# Whole-app wiring through shiny::testServer(): the modules are driven by their
# namespaced inputs against a real database built from the spec example.

app_server <- function(con, ...) {
  build_explorer_server(aviation_bundle(), con, action_ctx = NULL,
                        concept_cols_fn = NULL, ...)
}

test_that("choosing a type lists its objects", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    session$setInputs(`type_selector-object_type` = "Airport")
    expect_match(output[["results_table-result_info"]], "^4 objects found")

    session$setInputs(`type_selector-object_type` = "Airline")
    expect_match(output[["results_table-result_info"]], "^3 objects found")
  })
})

test_that("free-text search narrows the results", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    session$setInputs(`type_selector-object_type` = "Airport",
                      `type_selector-search_text` = "london")
    session$elapse(1000)
    expect_match(output[["results_table-result_info"]], "^1 objects found")
  })
})

test_that("selecting a row exposes its properties in the detail panel", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    session$setInputs(`type_selector-object_type` = "Airport")
    session$setInputs(`results_table-results_dt_rows_selected` = 1L)
    props <- output[["detail_panel-properties_table"]]
    # rows come back in primary-key order, so row 1 is CDG; every property
    # of it is listed
    expect_match(props, "<td> airport_id </td> <td> CDG </td>", fixed = TRUE)
    expect_match(props, "Charles de Gaulle", fixed = TRUE)
    expect_match(props, "<td> elevation_ft </td> <td> 392 </td>", fixed = TRUE)
  })
})

# MockShinySession#sendCustomMessage is a no-op that drops its arguments, so
# swap in a recorder to see what the server actually sends to the browser.
record_custom_messages <- function(session) {
  sent <- new.env()
  sent$messages <- list()
  unlockBinding("sendCustomMessage", session)
  session$sendCustomMessage <- function(type, message) {
    sent$messages[[length(sent$messages) + 1L]] <- list(type = type, message = message)
    invisible()
  }
  sent
}

test_that("the dark mode toggle alternates theme messages", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    sent <- record_custom_messages(session)
    session$setInputs(`type_selector-object_type` = "Airport")

    session$setInputs(dark_mode_toggle = 1)
    session$setInputs(dark_mode_toggle = 2)
    session$setInputs(dark_mode_toggle = 3)

    msgs <- Filter(function(m) identical(m$type, "toggle_dark_mode"),
                   sent$messages)
    expect_equal(length(msgs), 3L)
    expect_equal(vapply(msgs, function(m) m$message$dark, logical(1)),
                 c(TRUE, FALSE, TRUE))
  })
})

test_that("Traverse -> lands on the linked objects, not the whole target type", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    # R001 is the first FlightRoute (primary-key order); RouteOrigin -> JFK
    session$setInputs(`type_selector-object_type` = "FlightRoute")
    session$setInputs(`results_table-results_dt_rows_selected` = 1L)
    session$setInputs(`link_panel-traverse_out_1` = 1)

    # the browser echoes the dropdown change that the traversal requested
    session$setInputs(`type_selector-object_type` = "Airport")
    expect_match(output[["results_table-result_info"]], "^1 objects found")
  })
})

test_that("Search Around goes back to the sources of the selected object", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    # first Airport by primary key is CDG; RouteOrigin <- FlightRoute from CDG: none,
    # RouteDestination <- FlightRoute into CDG: R004
    session$setInputs(`type_selector-object_type` = "Airport")
    session$setInputs(`results_table-results_dt_rows_selected` = 1L)
    session$setInputs(`link_panel-traverse_in_2` = 1)
    session$setInputs(`type_selector-object_type` = "FlightRoute")
    expect_match(output[["results_table-result_info"]], "^1 objects found")
  })
})

test_that("changing the type by hand drops the traversal filter", {
  con <- make_aviation_con()
  shiny::testServer(app_server(con), {
    session$setInputs(`type_selector-object_type` = "FlightRoute")
    session$setInputs(`results_table-results_dt_rows_selected` = 1L)
    session$setInputs(`link_panel-traverse_out_1` = 1)
    session$setInputs(`type_selector-object_type` = "Airport")
    expect_match(output[["results_table-result_info"]], "^1 objects found")

    session$setInputs(`type_selector-object_type` = "Airline")
    expect_match(output[["results_table-result_info"]], "^3 objects found")
  })
})
