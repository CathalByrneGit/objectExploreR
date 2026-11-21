test_that("display_name returns display name when available", {
  obj <- list(
    id = "Airport",
    display = list(name = "Airport Terminal")
  )
  expect_equal(display_name(obj), "Airport Terminal")
})

test_that("display_name falls back to id when no display name", {
  obj <- list(id = "Airport", display = list(name = NULL))
  expect_equal(display_name(obj), "Airport")

  obj2 <- list(id = "Airport", display = NULL)
  expect_equal(display_name(obj2), "Airport")
})

test_that("display_name falls back to id when display name is empty", {
  obj <- list(id = "Airport", display = list(name = ""))
  expect_equal(display_name(obj), "Airport")
})

test_that("find_links_for_type finds outgoing and incoming links", {
  b <- aviation_bundle()

  result <- find_links_for_type(b, "FlightRoute")
  expect_true(length(result$outgoing) == 3)
  expect_true(length(result$incoming) == 0)

  result_airport <- find_links_for_type(b, "Airport")
  expect_true(length(result_airport$outgoing) == 0)
  expect_true(length(result_airport$incoming) == 2)
})

test_that("get_object_type finds correct object", {
  b <- aviation_bundle()

  airport <- get_object_type(b, "Airport")
  expect_false(is.null(airport))
  expect_equal(airport$id, "Airport")

  nonexistent <- get_object_type(b, "NonExistent")
  expect_null(nonexistent)
})

test_that("object_type_choices returns named vector", {
  b <- aviation_bundle()

  choices <- object_type_choices(b)
  expect_type(choices, "character")
  expect_equal(length(choices), 3)
  expect_true("Airport" %in% choices)
  expect_true("Airline" %in% choices)
  expect_true("FlightRoute" %in% choices)
})

test_that("find_actions_for_type finds actions targeting a type", {
  b <- aviation_bundle()

  actions <- find_actions_for_type(b, "Airport")
  expect_equal(length(actions), 1)
  expect_equal(actions[[1]]$id, "UpdateAirportStatus")

  actions_airline <- find_actions_for_type(b, "Airline")
  expect_equal(length(actions_airline), 0)
})

test_that("get_pk_columns returns primary key property IDs", {
  b <- aviation_bundle()

  airport <- get_object_type(b, "Airport")
  pk <- get_pk_columns(airport)
  expect_equal(pk, "airport_id")
})

test_that("format_value formats booleans", {
  expect_equal(format_value(TRUE, "boolean"), "Yes")
  expect_equal(format_value(FALSE, "boolean"), "No")
})

test_that("format_value handles NA and NULL", {
  expect_true(is.na(format_value(NULL, "string")))
  expect_true(is.na(format_value(NA, "string")))
})

test_that("format_value formats strings", {
  expect_equal(format_value("hello", "string"), "hello")
  expect_equal(format_value(42, "integer"), "42")
})

test_that("object_type_choices maps display names to ids, in bundle order", {
  choices <- object_type_choices(aviation_bundle())
  expect_equal(unname(choices), c("Airport", "Airline", "FlightRoute"))
  # the example gives FlightRoute a display name that differs from its id
  expect_equal(names(choices)[3], "Flight Route")
})

test_that("get_object_type returns NULL (not an error) for unknown or empty ids", {
  b <- aviation_bundle()
  expect_null(get_object_type(b, "Nope"))
  expect_null(get_object_type(b, NULL))
  expect_null(get_object_type(b, ""))
})

test_that("get_pk_columns is empty for a type without a primary key", {
  ot <- list(id = "NoPk", properties = list())
  expect_equal(get_pk_columns(ot), character(0))
})

test_that("helpers accept the legacy bundle layout through as_bundle()", {
  raw <- jsonlite::fromJSON(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR"),
    simplifyVector = TRUE, simplifyDataFrame = FALSE
  )
  names(raw)[names(raw) == "objects"] <- "objectTypes"
  b <- suppressWarnings(ontologySpecR::as_bundle(raw))
  expect_equal(length(object_type_choices(b)), 3L)
})
