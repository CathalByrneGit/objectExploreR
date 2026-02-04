test_that("fetch_objects returns correct data", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LAX', 'Los Angeles International', 'Los Angeles', 'United States', 33.9425, -118.4081, 128),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  result <- fetch_objects(con, airport_type)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("airport_id" %in% names(result))
  expect_true("name" %in% names(result))
})

test_that("fetch_objects applies text filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LAX', 'Los Angeles International', 'Los Angeles', 'United States', 33.9425, -118.4081, 128),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  filters <- list(
    list(property_id = "country", type = "text", value = "United States")
  )
  result <- fetch_objects(con, airport_type, filters = filters)
  expect_equal(nrow(result), 2)
})

test_that("fetch_objects applies numeric range filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LAX', 'Los Angeles International', 'Los Angeles', 'United States', 33.9425, -118.4081, 128),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  filters <- list(
    list(property_id = "elevation_ft", type = "numeric_range",
         value = list(min = 50, max = 200))
  )
  result <- fetch_objects(con, airport_type, filters = filters)
  expect_equal(nrow(result), 2)  # LAX (128) and LHR (83)
})

test_that("fetch_objects applies free-text search", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LAX', 'Los Angeles International', 'Los Angeles', 'United States', 33.9425, -118.4081, 128),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  result <- fetch_objects(con, airport_type, search_text = "London")
  expect_equal(nrow(result), 1)
  expect_equal(result$airport_id, "LHR")
})

test_that("fetch_linked returns linked objects (forward)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83)
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
      ('R002', 'JFK', 'LHR', 'BA', 0, 'A380'),
      ('R003', 'LHR', 'JFK', 'BA', 0, 'A380')
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

  # Find the RouteOrigin link (FlightRoute -> Airport)
  route_origin <- NULL
  for (link in b$links) {
    if (link$id == "RouteOrigin") route_origin <- link
  }

  # Fetch airports linked as origin from routes starting at JFK
  result <- fetch_linked(con, b, route_origin,
                          source_object_ids = c("R001", "R002"),
                          direction = "forward")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  # Both routes originate at JFK, so we should get JFK
  expect_true("JFK" %in% result$airport_id)
})

test_that("fetch_linked returns linked objects (reverse)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83)
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
      ('R002', 'JFK', 'LHR', 'BA', 0, 'A380'),
      ('R003', 'LHR', 'JFK', 'BA', 0, 'A380')
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )

  route_origin <- NULL
  for (link in b$links) {
    if (link$id == "RouteOrigin") route_origin <- link
  }

  # Reverse: find routes where JFK is the origin airport
  result <- fetch_linked(con, b, route_origin,
                          source_object_ids = c("JFK"),
                          direction = "reverse")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # R001, R002 originate from JFK
})

test_that("fetch_distinct_values returns unique values", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'JFK', 'New York', 'United States', 40.6, -73.8, 13),
      ('LAX', 'LAX', 'Los Angeles', 'United States', 33.9, -118.4, 128),
      ('LHR', 'LHR', 'London', 'United Kingdom', 51.5, -0.5, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  vals <- fetch_distinct_values(con, airport_type, "country")
  expect_type(vals, "character")
  expect_equal(length(vals), 2)
  expect_true("United States" %in% vals)
  expect_true("United Kingdom" %in% vals)
})

test_that("fetch_range returns min/max", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('JFK', 'JFK', 'New York', 'US', 40.6, -73.8, 13),
      ('LAX', 'LAX', 'LA', 'US', 33.9, -118.4, 128),
      ('LHR', 'LHR', 'London', 'UK', 51.5, -0.5, 83)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  range_val <- fetch_range(con, airport_type, "elevation_ft")
  expect_type(range_val, "list")
  expect_equal(range_val$min, 13)
  expect_equal(range_val$max, 128)
})

test_that("fetch_objects handles boolean filter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
      ('AA', 'American Airlines', 'United States', 1),
      ('BA', 'British Airways', 'United Kingdom', 1),
      ('XX', 'Defunct Air', 'Unknown', 0)
  ")

  b <- ontologySpecR::read_bundle(
    system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  )
  airline_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airline") airline_type <- obj
  }

  filters <- list(
    list(property_id = "active", type = "checkbox", value = TRUE)
  )
  result <- fetch_objects(con, airline_type, filters = filters)
  expect_equal(nrow(result), 2)
})

test_that("fetch_objects respects limit parameter", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("ontologySpecR")

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
  airport_type <- NULL
  for (obj in b$objects) {
    if (obj$id == "Airport") airport_type <- obj
  }

  result <- fetch_objects(con, airport_type, limit = 2)
  expect_equal(nrow(result), 2)
})
