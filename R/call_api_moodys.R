#' Call the Moody's API
#'
#' @description
#' Makes a signed call against the Moody's economy.com API. This is the
#' low-level entry point; most users want [get_moodys_series()],
#' [search_moodys_series()], or [get_moodys_basket()].
#'
#' Based on the Moody's API documentation:
#' <https://www.economy.com/support/apis/getting-started>
#'
#' @param apiCommand Path below `https://api.economy.com/data/v1/`.
#' @param accKey Moody's API access key. Defaults to the stored key, see
#'   [moodys_key()].
#' @param encKey Moody's API encryption key. Defaults to the stored key.
#' @param type HTTP method, `"GET"` or `"POST"`.
#'
#' @return An `httr2_response`. Note that versions before 0.1.0 returned an
#'   `httr` response; use [httr2::resp_body_json()] rather than
#'   `httr::content()` to read it.
#' @export
#'
#' @examples
#' \dontrun{
#' call_api_moodys("baskets/") |> httr2::resp_body_json()
#' }
call_api_moodys <- function(
  apiCommand,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc"),
  type = c("GET", "POST")
) {
  type <- match.arg(type)

  .moodys_signed_req(apiCommand, accKey = accKey, encKey = encKey) |>
    httr2::req_method(type) |>
    httr2::req_perform()
}

#' Parse a Moody's API response as JSON
#'
#' @param resp An `httr2_response`.
#'
#' @return The parsed body, simplified by [jsonlite::fromJSON()].
#' @keywords internal
#' @noRd
.moodys_json <- function(resp) {
  jsonlite::fromJSON(httr2::resp_body_string(resp))
}
