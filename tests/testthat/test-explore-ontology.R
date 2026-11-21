# explore_ontology() is the entry point: it normalises the bundle and builds
# the ActionContext. shiny::runApp() is stubbed so no server starts.

launch <- function(...) {
  testthat::local_mocked_bindings(runApp = function(app, ...) invisible(app),
                                  .package = "shiny")
  explore_ontology(...)
}

test_that("a non-DBI connection is rejected", {
  expect_error(launch(aviation_bundle(), "not a connection"),
               "open DBI connection")
})

test_that("a closed connection is rejected up front", {
  con <- make_aviation_con()
  DBI::dbDisconnect(con)
  expect_error(launch(aviation_bundle(), con), "open DBI connection")
})

test_that("a bundle with no object types is rejected", {
  con <- make_aviation_con()
  empty <- ontologySpecR::bundle(bundle_id = "empty", bundle_version = "0.1.0")
  expect_error(launch(empty, con), "no object types")
})

test_that("a path to a bundle JSON file is accepted", {
  con  <- make_aviation_con()
  path <- system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  expect_warning(app <- launch(path, con), "no action_ctx")
  expect_s3_class(app, "shiny.appobj")
})

test_that("a bundle with actions but no way to run them warns", {
  con <- make_aviation_con()
  expect_warning(launch(aviation_bundle(), con), "Actions will not execute")
})

test_that("an ActionContext suppresses the warning", {
  con <- make_aviation_con()
  ctx <- actionTypesR::action_context(aviation_bundle(), con)
  expect_no_warning(launch(aviation_bundle(), con, action_ctx = ctx))
})

test_that("legacy action_handlers still work, with a deprecation warning", {
  con <- make_aviation_con()
  expect_warning(
    launch(aviation_bundle(), con, action_handlers = list(
      UpdateAirportStatus = function(conn, action, params, targets) NULL
    )),
    "deprecated"
  )
})
