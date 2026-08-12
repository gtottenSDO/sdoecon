#' Internal session environment
#'
#' Holds the Moody's OAuth token and its expiry, plus the crosswalk cache.
#' Not exported; state lives for the duration of the R session only.
#'
#' @keywords internal
#' @noRd
.moodys_env <- new.env(parent = emptyenv())

#' Crosswalk cache
#'
#' Crosswalks are small, immutable reference tables, but every
#' [sdotools::sdo_db_pull_table()] call opens and closes its own database
#' connection. Caching them for the session avoids paying a TCP handshake per
#' lookup -- which matters because [get_bls_qcew()] resolves [geography_xwalk()]
#' as a default argument.
#'
#' @keywords internal
#' @noRd
.xwalk_cache <- new.env(parent = emptyenv())

#' Clear the cached crosswalks
#'
#' Crosswalks fetched from the SDO Postgres database are cached for the life of
#' the R session. Call this to force the next crosswalk lookup to re-query the
#' database, for example after a crosswalk has been updated server-side.
#'
#' @return Invisibly, the character vector of cache keys that were dropped.
#' @export
#'
#' @examples
#' sdoecon_xwalk_refresh()
sdoecon_xwalk_refresh <- function() {
  keys <- ls(envir = .xwalk_cache, all.names = TRUE)
  rm(list = keys, envir = .xwalk_cache)
  invisible(keys)
}

#' Fetch a value through the crosswalk cache
#'
#' @param key Cache key.
#' @param value Expression producing the value; only evaluated on a cache miss.
#' @param use_cache Whether to consult the cache at all.
#'
#' @keywords internal
#' @noRd
.cached <- function(key, value, use_cache = TRUE) {
  if (!use_cache) {
    return(value)
  }
  if (!exists(key, envir = .xwalk_cache, inherits = FALSE)) {
    assign(key, value, envir = .xwalk_cache)
  }
  get(key, envir = .xwalk_cache, inherits = FALSE)
}
