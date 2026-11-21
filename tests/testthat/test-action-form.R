# Actions must go through actionTypesR::submit_action() via an ActionContext.

fixture_action_ctx <- function(con, calls) {
  ctx <- actionTypesR::action_context(aviation_bundle(), con)
  actionTypesR::register_handler(
    ctx, "UpdateAirportStatus",
    function(conn, action, params, targets) {
      calls$n       <- if (is.null(calls$n)) 1L else calls$n + 1L
      calls$params  <- params
      calls$targets <- targets
      invisible(NULL)
    }
  )
}

action_form_args <- function(con, action_ctx) {
  b <- aviation_bundle()
  list(
    action       = ontologySpecR::bundle_action_type(b, "UpdateAirportStatus"),
    connection   = con,
    action_ctx   = action_ctx,
    selected_row = shiny::reactive(list(airport_id = "JFK", name = "JFK")),
    current_type = shiny::reactive(ontologySpecR::bundle_object_type(b, "Airport"))
  )
}

test_that("submitting the form runs the handler through submit_action()", {
  con   <- make_aviation_con()
  calls <- new.env()
  ctx   <- fixture_action_ctx(con, calls)

  shiny::testServer(action_form_server, args = action_form_args(con, ctx), {
    session$setInputs(param_new_status = "closed")
    session$setInputs(submit_action = 1)

    expect_equal(calls$n, 1L)
    expect_equal(calls$params$new_status, "closed")
    expect_equal(calls$targets[[1]], "JFK")
    expect_match(output$action_result, "Action completed")
  })

  # submit_action() logs every submission; a bypass would leave no trace
  log <- actionTypesR::action_log(ctx)
  expect_equal(nrow(log), 1L)
})

test_that("clicking submit twice submits twice, not four times", {
  con   <- make_aviation_con()
  calls <- new.env()
  ctx   <- fixture_action_ctx(con, calls)

  shiny::testServer(action_form_server, args = action_form_args(con, ctx), {
    session$setInputs(param_new_status = "open")
    session$setInputs(submit_action = 1)
    session$setInputs(submit_action = 2)
    expect_equal(calls$n, 2L)
  })
})

test_that("a missing required parameter is reported, not run", {
  con   <- make_aviation_con()
  calls <- new.env()
  ctx   <- fixture_action_ctx(con, calls)

  shiny::testServer(action_form_server, args = action_form_args(con, ctx), {
    session$setInputs(param_new_status = "")
    session$setInputs(submit_action = 1)
    expect_null(calls$n)
    expect_match(output$action_result, "^Error:")
  })
})

test_that("without an action context the form says so", {
  con <- make_aviation_con()
  shiny::testServer(action_form_server, args = action_form_args(con, NULL), {
    session$setInputs(param_new_status = "closed")
    session$setInputs(submit_action = 1)
    expect_match(output$action_result, "No action context")
  })
})
