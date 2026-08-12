# search_moodys_series() used to error on its own documented defaults:
# `rows > 100 || is.null(rows)` with rows = NULL evaluated NULL > 100 first,
# giving logical(0). These tests pin the validation order.

test_that("an empty query is rejected before any network call", {
  expect_error(search_moodys_series(""), "query cannot be empty")
  expect_error(search_moodys_series(NULL), "query cannot be empty")
  expect_error(search_moodys_series(), "query cannot be empty")
})

test_that("a NULL rows argument does not error during validation", {
  # Fails at the token/network stage, not on the length-zero comparison
  # that used to abort the call.
  err <- tryCatch(
    search_moodys_series("colorado employment", rows = NULL),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl("argument is of length zero", err, fixed = TRUE))
})

test_that("large row counts warn rather than error", {
  local_mocked_bindings(
    .moodys_token_req = function(...) stop("stop-before-network")
  )

  expect_warning(
    try(search_moodys_series("q", rows = 500), silent = TRUE),
    "Large row counts"
  )
})
