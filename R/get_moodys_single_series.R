#' Get Moody's Data Buffet series
#'
#' @description
#' Retrieves Moody's Data Buffet series by mnemonic. Requests are automatically
#' split into chunks of 25, the API's per-call limit. Use [convert_moodys()] to
#' turn the result into a data frame.
#'
#' Access and encryption keys can be stored with [set_moodys_api_key()] or
#' passed directly.
#'
#' API documentation: <https://www.economy.com/products/tools/api#db>
#'
#' Based on Moody's sample code:
#' <https://github.com/moodysanalytics/databuffet-api-codesamples/blob/master/R/Single-Series.R>
#'
#' @param mnemonics A single mnemonic or a character vector of mnemonics. More
#'   than 25 are requested in batches.
#' @param freq Frequency conversion code. See [get_moodys_codes()].
#' @param trans Transformation code. See the API user guide.
#' @param vintage Vintage to request. See [get_moodys_vintages()].
#' @param accKey Moody's API access key. Defaults to the stored key, see
#'   [moodys_key()].
#' @param encKey Moody's API encryption key. Defaults to the stored key.
#'
#' @return An `httr2_response` for 25 or fewer mnemonics, or a list of
#'   responses when batching. Pass either to [convert_moodys()].
#' @export
#'
#' @examples
#' \dontrun{
#' jobs_forecast_202309 <- get_moodys_series("fet.iusa", vintage = "202309") |>
#'   convert_moodys()
#' }
get_moodys_series <- function(
  mnemonics,
  freq = "0",
  trans = "0",
  vintage = NULL,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  create_req <- function(mnemonics) {
    .moodys_token_req(
      "multi-series/",
      m = paste0(mnemonics, collapse = ";"),
      freq = freq,
      trans = trans,
      vintage = vintage
    )
  }

  # The API accepts at most 25 mnemonics per call.
  if (length(mnemonics) > 25) {
    chunks <- split(mnemonics, ceiling(seq_along(mnemonics) / 25))

    chunks |>
      purrr::map(\(x) create_req(x)) |>
      httr2::req_perform_sequential(progress = TRUE)
  } else {
    httr2::req_perform(create_req(mnemonics))
  }
}

#' Convert a Moody's Data Buffet query to a data frame
#'
#' @param resp An `httr2_response`, or a list of responses, from
#'   [get_moodys_series()].
#'
#' @return A tibble, one row per observation.
#' @export
#'
#' @examples
#' \dontrun{
#' jobs_forecast_202309 <- get_moodys_series("fet.iusa", vintage = "202309") |>
#'   convert_moodys()
#' }
convert_moodys <- function(resp) {
  process_response <- function(response) {
    response |>
      httr2::resp_body_json() |>
      purrr::pluck("data") |>
      tibble::tibble() |>
      rlang::set_names("response") |>
      tidyr::unnest_wider("response") |>
      tidyr::unnest_longer("data") |>
      tidyr::unnest_wider("data")
  }

  if (inherits(resp, "httr2_response")) {
    process_response(resp)
  } else {
    resp |>
      httr2::resps_successes() |>
      httr2::resps_data(\(x) process_response(x))
  }
}
