# Shared fixtures. The bundle always comes from the spec itself
# (ontologySpecR::spec_example_bundle()); tests must not hand-roll bundle lists.
#
# The database matches that bundle: Airport is backed by `public.airports`
# (the example declares schema "public"), Airline by `airlines`, FlightRoute
# by `routes`. DuckDB is used when installed; otherwise RSQLite, with an
# attached in-memory database standing in for the `public` schema.

aviation_bundle <- function() {
  ontologySpecR::spec_example_bundle()
}

make_aviation_con <- function(env = parent.frame()) {
  if (requireNamespace("duckdb", quietly = TRUE)) {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS public")
  } else {
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS public")
  }
  withr::defer(
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con),
    envir = env
  )

  DBI::dbExecute(con, "
    CREATE TABLE public.airports (
      airport_id TEXT PRIMARY KEY, name TEXT NOT NULL, city TEXT,
      country TEXT, latitude DOUBLE PRECISION, longitude DOUBLE PRECISION,
      elevation_ft INTEGER)")
  DBI::dbExecute(con, "
    INSERT INTO public.airports VALUES
      ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
      ('LAX', 'Los Angeles International', 'Los Angeles', 'United States', 33.9425, -118.4081, 128),
      ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.47, -0.4543, 83),
      ('CDG', 'Charles de Gaulle', 'Paris', 'France', 49.0097, 2.5479, 392)")

  DBI::dbExecute(con, "
    CREATE TABLE airlines (
      airline_id TEXT PRIMARY KEY, name TEXT NOT NULL, country TEXT,
      active BOOLEAN)")
  DBI::dbExecute(con, "
    INSERT INTO airlines VALUES
      ('AA', 'American Airlines', 'United States', TRUE),
      ('BA', 'British Airways', 'United Kingdom', TRUE),
      ('XX', 'Defunct Air', 'Nowhere', FALSE)")

  DBI::dbExecute(con, "
    CREATE TABLE routes (
      route_id TEXT PRIMARY KEY, origin_id TEXT NOT NULL,
      destination_id TEXT NOT NULL, airline_id TEXT NOT NULL,
      stops INTEGER, equipment TEXT)")
  DBI::dbExecute(con, "
    INSERT INTO routes VALUES
      ('R001', 'JFK', 'LHR', 'AA', 0, '777'),
      ('R002', 'JFK', 'LHR', 'BA', 0, 'A380'),
      ('R003', 'LHR', 'JFK', 'BA', 0, 'A380'),
      ('R004', 'LAX', 'CDG', 'AA', 1, '787')")

  con
}

# Look up one object type / link type from the fixture bundle by id.
fixture_object_type <- function(id) {
  ontologySpecR::bundle_object_type(aviation_bundle(), id)
}
fixture_link_type <- function(id) {
  ontologySpecR::bundle_link_type(aviation_bundle(), id)
}
