# Crosswalk tests need the SDO Postgres server. Skip cleanly when it is
# unreachable so the suite still runs offline and in CI.
skip_if_no_sdo_db <- function() {
  testthat::skip_if_offline()
  con <- try(sdotools::sdo_db_connect(public = TRUE), silent = TRUE)
  if (inherits(con, "try-error")) {
    testthat::skip("SDO Postgres database is not reachable")
  }
  try(DBI::dbDisconnect(con), silent = TRUE)
  invisible(TRUE)
}
