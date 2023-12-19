
#' Authenticate Moody's API for token
#'
#' Stores token and token expiration time in temporary environment variable
#'
#' @param accKey
#' @param encKey
#' @import httr2
#' @importFrom utils URLencode
#'
#' @return Stores token and token expiration time in temporary environment variable
#' @export
#'
#' @examples

get_moodys_token <- function(accKey = Sys.getenv("MOODYS_ACC_KEY"),
                                encKey = Sys.getenv("MOODYS_ENC_KEY")) {
  # Load library
  library(httr2)

  # Define base URL and endpoint
  base_url <- "https://api.economy.com/data/v1/oauth2/token"


  # Define headers and body
  headers <- list("Content-Type" = "application/x-www-form-urlencoded")

  body <- list(
    client_id = accKey,
    client_secret = encKey,
    grant_type = "client_credentials"
  )

  # Create and perform the request
  req <- request(base_url) |>
    req_headers(!!!headers) |>
    req_body_form(!!!body)

  response <- req_perform(req)

  # Parse the response
  data <- resp_body_json(response)

  # Add token to
  # Print the data
  temporary_env$token <- data$access_token
  temporary_env$token_expires <- Sys.time() + data$expires_in
}


#' Check Moody's Token
#'
#' @param accKey
#' @param encKey
#'
#' @return
#' @export
#'
#' @examples

check_moodys_token <- function(accKey = Sys.getenv("MOODYS_ACC_KEY"),
                               encKey = Sys.getenv("MOODYS_ENC_KEY")) {
  if(all(exists("token_expires", envir = temporary_env),
       temporary_env$token_expires > Sys.time())) {
  token <- temporary_env$token
} else {
  get_moodys_token(accKey, encKey)
  token <- temporary_env$token
}
}

