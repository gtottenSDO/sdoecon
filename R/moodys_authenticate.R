moodys_base_url <- "https://api.economy.com/data/v1/"

#' Build a signed Moody's request
#'
#' The Moody's Data Buffet API authenticates most endpoints with an
#' HMAC-SHA256 signature over the access key and a UTC timestamp. This helper
#' centralises that scheme, which was previously repeated in four places.
#'
#' @param path Path below `https://api.economy.com/data/v1/`.
#' @param accKey,encKey Moody's API keys.
#' @param ... Query parameters appended to the URL.
#'
#' @return An `httr2_request`.
#' @keywords internal
#' @noRd
.moodys_signed_req <- function(
  path,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc"),
  ...
) {
  time_stamp <- format(
    as.POSIXct(Sys.time()),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  signature <- digest::hmac(encKey, paste0(accKey, time_stamp), "sha256")

  req <- httr2::request(paste0(moodys_base_url, path)) |>
    httr2::req_headers(
      AccessKeyId = accKey,
      Signature = signature,
      TimeStamp = time_stamp
    ) |>
    # The API is rate limited; this replaces the blanket Sys.sleep(1) that
    # used to precede every call.
    httr2::req_throttle(capacity = 60, fill_time_s = 60) |>
    httr2::req_retry(max_tries = 3)

  # list2() rather than list() so callers can splice with !!!
  params <- rlang::list2(...)
  if (length(params) > 0) {
    req <- httr2::req_url_query(req, !!!params)
  }

  req
}

#' Authenticate against the Moody's API for a bearer token
#'
#' Requests an OAuth2 token and stores it, with its expiry, in an internal
#' session environment. Called automatically by [check_moodys_token()]; you
#' rarely need to call it yourself.
#'
#' @param accKey Moody's API access key. Defaults to the stored key, see
#'   [moodys_key()].
#' @param encKey Moody's API encryption key. Defaults to the stored key.
#'
#' @return Invisibly, the access token.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moodys_token()
#' }
get_moodys_token <- function(
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  response <- httr2::request(paste0(moodys_base_url, "oauth2/token")) |>
    httr2::req_body_form(
      client_id = accKey,
      client_secret = encKey,
      grant_type = "client_credentials"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(response)

  .moodys_env$token <- data$access_token
  .moodys_env$token_expires <- Sys.time() + data$expires_in
  # Record whose credentials minted this token, so check_moodys_token() can
  # tell a cache hit from a different account asking for one.
  .moodys_env$key_id <- .moodys_key_id(accKey, encKey)

  invisible(.moodys_env$token)
}

#' Return a valid Moody's bearer token
#'
#' Returns the cached token if it has not expired, otherwise requests a fresh
#' one via [get_moodys_token()].
#'
#' @inheritParams get_moodys_token
#'
#' @return The bearer token, as a length-1 character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' check_moodys_token()
#' }
check_moodys_token <- function(
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  valid <- exists("token_expires", envir = .moodys_env, inherits = FALSE) &&
    identical(.moodys_env$key_id, .moodys_key_id(accKey, encKey)) &&
    .moodys_env$token_expires > Sys.time()

  if (!valid) {
    get_moodys_token(accKey, encKey)
  }

  .moodys_env$token
}

#' Identify a pair of Moody's keys without retaining them
#'
#' @param accKey,encKey Moody's API keys.
#'
#' @return A hash of the key pair, used to scope the cached bearer token.
#' @keywords internal
#' @noRd
.moodys_key_id <- function(accKey, encKey) {
  digest::digest(list(accKey, encKey))
}

#' Build a bearer-token Moody's request
#'
#' @param path Path below `https://api.economy.com/data/v1/`.
#' @param ... Query parameters appended to the URL.
#' @param accKey,encKey Moody's API keys used to mint the bearer token. Named
#'   after `...`, so callers must pass them by name.
#'
#' @return An `httr2_request`.
#' @keywords internal
#' @noRd
.moodys_token_req <- function(
  path,
  ...,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  token <- check_moodys_token(accKey, encKey)

  req <- httr2::request(paste0(moodys_base_url, path)) |>
    httr2::req_headers(
      Accept = "application/json",
      Authorization = paste0("Bearer ", token)
    ) |>
    httr2::req_retry(max_tries = 3)

  params <- rlang::list2(...)
  if (length(params) > 0) {
    req <- httr2::req_url_query(req, !!!params)
  }

  req
}
