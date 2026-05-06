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

test_that("property_column returns source column when specified", {
  prop <- list(id = "airport_id", source = list(column = "ap_id"))
  expect_equal(property_column(prop), "ap_id")
})

test_that("property_column falls back to id", {
  prop <- list(id = "airport_id", source = NULL)
  expect_equal(property_column(prop), "airport_id")
})

test_that("find_links_for_type finds outgoing and incoming links", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

  result <- find_links_for_type(b, "FlightRoute")
  expect_true(length(result$outgoing) == 3)
  expect_true(length(result$incoming) == 0)

  result_airport <- find_links_for_type(b, "Airport")
  expect_true(length(result_airport$outgoing) == 0)
  expect_true(length(result_airport$incoming) == 2)
})

test_that("get_object_type finds correct object", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

  airport <- get_object_type(b, "Airport")
  expect_false(is.null(airport))
  expect_equal(airport$id, "Airport")

  nonexistent <- get_object_type(b, "NonExistent")
  expect_null(nonexistent)
})

test_that("object_type_choices returns named vector", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

  choices <- object_type_choices(b)
  expect_type(choices, "character")
  expect_equal(length(choices), 3)
  expect_true("Airport" %in% choices)
  expect_true("Airline" %in% choices)
  expect_true("FlightRoute" %in% choices)
})

test_that("find_actions_for_type finds actions targeting a type", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

  actions <- find_actions_for_type(b, "Airport")
  expect_equal(length(actions), 1)
  expect_equal(actions[[1]]$id, "UpdateAirportStatus")

  actions_airline <- find_actions_for_type(b, "Airline")
  expect_equal(length(actions_airline), 0)
})

test_that("get_pk_columns returns primary key property IDs", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

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
