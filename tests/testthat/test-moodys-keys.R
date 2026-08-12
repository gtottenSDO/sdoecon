test_that("moodys_key() falls back to environment variables", {
  # Force the keyring lookup to fail so the env-var branch is exercised.
  local_mocked_bindings(
    key_get = function(...) stop("no keyring"),
    .package = "keyring"
  )
  withr::local_envvar(MOODYS_ACC_KEY = "acc-from-env", MOODYS_ENC_KEY = "")

  expect_equal(moodys_key("acc"), "acc-from-env")
  expect_error(moodys_key("enc"), "No Moody's enc key found")
})

test_that("moodys_key() prefers the credential store over env vars", {
  local_mocked_bindings(
    key_get = function(service, username, ...) paste0("kr-", username),
    .package = "keyring"
  )
  withr::local_envvar(MOODYS_ACC_KEY = "acc-from-env")

  expect_equal(moodys_key("acc"), "kr-acc_key")
})

test_that("moodys_key() rejects unknown key names", {
  expect_error(moodys_key("nope"))
})

test_that("set_moodys_api_key() writes to .Renviron without duplicating", {
  # Must pass renviron_path explicitly. R resolves `~` once at startup, so
  # overriding HOME does NOT redirect path.expand("~") -- a test relying on
  # that would silently rewrite the developer's real ~/.Renviron.
  renviron <- withr::local_tempfile()
  writeLines(c("OTHER_VAR='keep'", "MOODYS_ACC_KEY='old'"), renviron)

  expect_message(
    set_moodys_api_key(
      "new-acc",
      "new-enc",
      backend = "renviron",
      renviron_path = renviron
    ),
    "Restart R"
  )

  lines <- readLines(renviron)
  expect_true("OTHER_VAR='keep'" %in% lines)
  expect_equal(sum(grepl("^MOODYS_ACC_KEY=", lines)), 1)
  expect_true("MOODYS_ACC_KEY='new-acc'" %in% lines)
  expect_true("MOODYS_ENC_KEY='new-enc'" %in% lines)
})

test_that("set_moodys_api_key() session backend sets env vars only", {
  withr::local_envvar(MOODYS_ACC_KEY = NA, MOODYS_ENC_KEY = NA)

  expect_message(
    set_moodys_api_key("a", "e", backend = "session"),
    "this session only"
  )
  expect_equal(Sys.getenv("MOODYS_ACC_KEY"), "a")
})

test_that("the deprecated `permanent` argument still maps to a backend", {
  withr::local_envvar(MOODYS_ACC_KEY = NA, MOODYS_ENC_KEY = NA)

  expect_warning(
    suppressMessages(set_moodys_api_key("a", "e", permanent = FALSE)),
    "deprecated"
  )
  expect_equal(Sys.getenv("MOODYS_ACC_KEY"), "a")
})
