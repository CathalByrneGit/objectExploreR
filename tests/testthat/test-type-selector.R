selector_args <- function() {
  list(
    bundle     = shiny::reactive(aviation_bundle()),
    nav_target = shiny::reactiveVal(NULL)
  )
}

test_that("type selector reports the chosen object type id", {
  shiny::testServer(type_selector_server, args = selector_args(), {
    session$setInputs(object_type = "Airport")
    expect_equal(session$returned$selected_type_id(), "Airport")

    session$setInputs(object_type = "FlightRoute")
    expect_equal(session$returned$selected_type_id(), "FlightRoute")
  })
})

test_that("type selector debounces and exposes the search text", {
  shiny::testServer(type_selector_server, args = selector_args(), {
    session$setInputs(object_type = "Airport", search_text = "lon")
    session$elapse(1000)
    expect_equal(session$returned$search_text(), "lon")
  })
})

test_that("type selector counts refresh clicks", {
  shiny::testServer(type_selector_server, args = selector_args(), {
    session$setInputs(object_type = "Airport", refresh = 0)
    before <- session$returned$refresh_trigger()
    session$setInputs(refresh = 1)
    session$setInputs(refresh = 2)
    expect_equal(session$returned$refresh_trigger(), before + 2L)
  })
})

test_that("type selector consumes a navigation request", {
  args <- selector_args()
  shiny::testServer(type_selector_server, args = args, {
    session$setInputs(object_type = "FlightRoute")
    args$nav_target("Airline")
    session$flushReact()
    # the request is handed to the dropdown and then cleared
    expect_null(args$nav_target())
  })
})
