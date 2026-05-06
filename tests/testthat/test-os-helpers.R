test_that("apply_filter_to_os applies text filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK International', 'New York', 'United States'),
      ('LAX', 'LAX International', 'Los Angeles', 'United States'),
      ('LHR', 'Heathrow', 'London', 'United Kingdom')
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)
  os <- objectSetsR::object_set(ctx, "Airport")

  filter_spec <- list(property_id = "country", type = "text", value = "United")
  filtered_os <- apply_filter_to_os(os, filter_spec)

  result <- objectSetsR::os_collect(filtered_os)
  expect_equal(nrow(result), 2)
})

test_that("apply_filter_to_os applies select filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK', 'New York', 'United States'),
      ('LAX', 'LAX', 'Los Angeles', 'United States'),
      ('LHR', 'Heathrow', 'London', 'United Kingdom')
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)
  os <- objectSetsR::object_set(ctx, "Airport")

  filter_spec <- list(property_id = "country", type = "select", value = "United Kingdom")
  filtered_os <- apply_filter_to_os(os, filter_spec)

  result <- objectSetsR::os_collect(filtered_os)
  expect_equal(nrow(result), 1)
  expect_equal(result$country, "United Kingdom")
})

test_that("apply_filter_to_os applies numeric_range filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT,
      latitude REAL,
      longitude REAL,
      elevation_ft INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK', 'NY', 'US', 40.6, -73.8, 13),
      ('LAX', 'LAX', 'LA', 'US', 33.9, -118.4, 128),
      ('LHR', 'LHR', 'London', 'UK', 51.5, -0.5, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)
  os <- objectSetsR::object_set(ctx, "Airport")

  filter_spec <- list(
    property_id = "elevation_ft",
    type = "numeric_range",
    value = list(min = 50, max = 200)
  )
  filtered_os <- apply_filter_to_os(os, filter_spec)

  result <- objectSetsR::os_collect(filtered_os)
  expect_equal(nrow(result), 2)
})

test_that("apply_filter_to_os applies boolean_tri filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airlines (
      airline_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      country TEXT,
      active INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airlines VALUES
      ('AA', 'American Airlines', 'US', 1),
      ('BA', 'British Airways', 'UK', 1),
      ('XX', 'Defunct Air', 'XX', 0)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)
  os <- objectSetsR::object_set(ctx, "Airline")

  filter_spec <- list(property_id = "active", type = "boolean_tri", value = "TRUE")
  filtered_os <- apply_filter_to_os(os, filter_spec)

  result <- objectSetsR::os_collect(filtered_os)
  expect_equal(nrow(result), 2)

  filter_spec2 <- list(property_id = "active", type = "boolean_tri", value = "any")
  filtered_os2 <- apply_filter_to_os(os, filter_spec2)
  result2 <- objectSetsR::os_collect(filtered_os2)
  expect_equal(nrow(result2), 3)
})

test_that("apply_search_to_os searches across string properties", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT,
      latitude REAL,
      longitude REAL,
      elevation_ft INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK International', 'New York', 'United States', 40.6, -73.8, 13),
      ('LAX', 'LAX International', 'Los Angeles', 'United States', 33.9, -118.4, 128),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.5, -0.5, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)
  os <- objectSetsR::object_set(ctx, "Airport")

  searched_os <- apply_search_to_os(os, b, "Airport", "London")
  result <- objectSetsR::os_collect(searched_os)
  expect_equal(nrow(result), 1)
  expect_equal(result$airport_id, "LHR")
})

test_that("traverse_link_os performs forward traversal", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT,
      latitude REAL,
      longitude REAL,
      elevation_ft INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK', 'NY', 'US', 40.6, -73.8, 13),
      ('LHR', 'LHR', 'London', 'UK', 51.5, -0.5, 83)
  ")

  DBI::dbExecute(con, "
    CREATE TABLE routes (
      route_id TEXT PRIMARY KEY,
      origin_id TEXT NOT NULL,
      destination_id TEXT NOT NULL,
      airline_id TEXT NOT NULL,
      stops INTEGER DEFAULT 0,
      equipment TEXT
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO routes VALUES
      ('R001', 'JFK', 'LHR', 'AA', 0, '777'),
      ('R002', 'JFK', 'LHR', 'BA', 0, 'A380')
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)

  route_origin <- NULL
  for (link in b$links) {
    if (link$id == "RouteOrigin") route_origin <- link
  }

  result <- traverse_link_os(ctx, route_origin, c("R001", "R002"), "forward")
  expect_s3_class(result, "data.frame")
  expect_true("airport_id" %in% names(result))
})

test_that("fetch_distinct_os returns unique values", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT,
      latitude REAL,
      longitude REAL,
      elevation_ft INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK', 'NY', 'United States', 40.6, -73.8, 13),
      ('LAX', 'LAX', 'LA', 'United States', 33.9, -118.4, 128),
      ('LHR', 'LHR', 'London', 'United Kingdom', 51.5, -0.5, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)

  vals <- fetch_distinct_os(ctx, "Airport", "country")
  expect_type(vals, "character")
  expect_equal(length(vals), 2)
  expect_true("United States" %in% vals)
  expect_true("United Kingdom" %in% vals)
})

test_that("fetch_range_os returns min/max values", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("objectSetsR")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE airports (
      airport_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      city TEXT,
      country TEXT,
      latitude REAL,
      longitude REAL,
      elevation_ft INTEGER
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO airports VALUES
      ('JFK', 'JFK', 'NY', 'US', 40.6, -73.8, 13),
      ('LAX', 'LAX', 'LA', 'US', 33.9, -118.4, 128),
      ('LHR', 'LHR', 'London', 'UK', 51.5, -0.5, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  ctx <- objectSetsR::ontology_context(b, con)

  range_val <- fetch_range_os(ctx, "Airport", "elevation_ft")
  expect_type(range_val, "list")
  expect_equal(range_val$min, 13)
  expect_equal(range_val$max, 128)
})
