# These exercise the objectSetsR bridge against the spec example bundle and a
# matching database (see helper-fixtures.R).

fixture_ctx <- function(env = parent.frame()) {
  con <- make_aviation_con(env)
  objectSetsR::ontology_context(aviation_bundle(), con)
}

collect_filtered <- function(os, spec) {
  objectSetsR::os_collect(apply_filter_to_os(os, spec))
}

test_that("text filter matches substrings, case-insensitively", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airport")

  res <- collect_filtered(os, list(property_id = "country", type = "text",
                                   value = "United"))
  expect_equal(sort(res$airport_id), c("JFK", "LAX", "LHR"))

  res <- collect_filtered(os, list(property_id = "country", type = "text",
                                   value = "united kingdom"))
  expect_equal(res$airport_id, "LHR")
})

test_that("blank text filter is a no-op", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airport")
  res <- collect_filtered(os, list(property_id = "country", type = "text",
                                   value = "  "))
  expect_equal(nrow(res), 4L)
})

test_that("select and select_multi filters match exact values", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airport")

  res <- collect_filtered(os, list(property_id = "country", type = "select",
                                   value = "France"))
  expect_equal(res$airport_id, "CDG")

  res <- collect_filtered(os, list(property_id = "country",
                                   type = "select_multi",
                                   value = c("France", "United Kingdom")))
  expect_equal(sort(res$airport_id), c("CDG", "LHR"))

  res <- collect_filtered(os, list(property_id = "country",
                                   type = "select_multi",
                                   value = character(0)))
  expect_equal(nrow(res), 4L)
})

test_that("numeric_range filter is inclusive and tolerates one-sided bounds", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airport")

  res <- collect_filtered(os, list(property_id = "elevation_ft",
                                   type = "numeric_range",
                                   value = list(min = 50, max = 200)))
  expect_equal(sort(res$airport_id), c("LAX", "LHR"))

  res <- collect_filtered(os, list(property_id = "elevation_ft",
                                   type = "numeric_range",
                                   value = list(min = 300, max = NA)))
  expect_equal(res$airport_id, "CDG")
})

test_that("boolean_tri filter handles True / False / Any", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airline")

  active <- collect_filtered(os, list(property_id = "active",
                                      type = "boolean_tri", value = "TRUE"))
  expect_equal(sort(active$airline_id), c("AA", "BA"))

  inactive <- collect_filtered(os, list(property_id = "active",
                                        type = "boolean_tri", value = "FALSE"))
  expect_equal(inactive$airline_id, "XX")

  any_val <- collect_filtered(os, list(property_id = "active",
                                       type = "boolean_tri", value = "any"))
  expect_equal(nrow(any_val), 3L)
})

test_that("filters compose", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airport")
  os  <- apply_filter_to_os(os, list(property_id = "country", type = "text",
                                     value = "United"))
  os  <- apply_filter_to_os(os, list(property_id = "elevation_ft",
                                     type = "numeric_range",
                                     value = list(min = 100, max = 1000)))
  expect_equal(objectSetsR::os_collect(os)$airport_id, "LAX")
})

test_that("apply_search_to_os searches every string property, any case", {
  ctx <- fixture_ctx()
  b   <- aviation_bundle()
  os  <- objectSetsR::object_set(ctx, "Airport")

  hit <- function(text) {
    objectSetsR::os_collect(apply_search_to_os(os, b, "Airport", text))$airport_id
  }
  expect_equal(hit("London"), "LHR")            # name + city
  expect_equal(hit("london"), "LHR")            # case-insensitive
  expect_equal(hit("France"), "CDG")            # country only
  expect_equal(sort(hit("International")), c("JFK", "LAX"))
  expect_equal(length(hit("")), 4L)             # blank search = everything
  expect_equal(length(hit("zzz-no-match")), 0L)
})

test_that("traverse_link_os follows a link forward, once per target", {
  ctx  <- fixture_ctx()
  link <- fixture_link_type("RouteOrigin")

  # R001 and R002 both originate at JFK: one airport, not two
  res <- traverse_link_os(ctx, link, c("R001", "R002"), "forward")
  expect_equal(res$airport_id, "JFK")

  res <- traverse_link_os(ctx, link, c("R001", "R004"), "forward")
  expect_equal(sort(res$airport_id), c("JFK", "LAX"))
})

test_that("traverse_link_os follows a link in reverse (search around)", {
  ctx  <- fixture_ctx()
  link <- fixture_link_type("RouteOrigin")

  res <- traverse_link_os(ctx, link, "JFK", "reverse")
  expect_equal(sort(res$route_id), c("R001", "R002"))

  expect_equal(nrow(traverse_link_os(ctx, link, "CDG", "reverse")), 0L)
  expect_equal(nrow(traverse_link_os(ctx, link, character(0), "reverse")), 0L)
})

test_that("a second link type to the same target keeps its own join keys", {
  ctx  <- fixture_ctx()
  link <- fixture_link_type("RouteDestination")
  res  <- traverse_link_os(ctx, link, "R001", "forward")
  expect_equal(res$airport_id, "LHR")
})

test_that("fetch_distinct_os returns sorted unique values", {
  ctx  <- fixture_ctx()
  vals <- fetch_distinct_os(ctx, "Airport", "country")
  expect_equal(vals, c("France", "United Kingdom", "United States"))

  expect_equal(length(fetch_distinct_os(ctx, "Airport", "country", limit = 2)), 2L)
})

test_that("fetch_range_os returns min and max", {
  ctx <- fixture_ctx()
  rng <- fetch_range_os(ctx, "Airport", "elevation_ft")
  expect_equal(rng$min, 13)
  expect_equal(rng$max, 392)
})

test_that("collect_limited caps rows in the query", {
  ctx <- fixture_ctx()
  os  <- objectSetsR::object_set(ctx, "Airport")
  expect_equal(nrow(collect_limited(os, 2L)), 2L)
  expect_equal(nrow(collect_limited(os, 100L)), 4L)
})
