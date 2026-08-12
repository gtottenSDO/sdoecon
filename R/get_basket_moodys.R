#' Get Moody's basket data
#'
#' @description
#' Runs a named Moody's basket, waits for it to finish processing, and returns
#' the resulting CSV. Use [process_moodys()] to reshape the result to long
#' format with geography labels.
#'
#' Based on the Moody's API documentation:
#' <https://www.economy.com/support/apis/getting-started>
#'
#' @param BASKET_NAME Name of the basket to run.
#' @param accKey Moody's API access key. Defaults to the stored key, see
#'   [moodys_key()].
#' @param encKey Moody's API encryption key. Defaults to the stored key.
#' @param max_wait Maximum seconds to wait for the basket to finish processing
#'   before erroring. Baskets are run server-side and can take minutes.
#' @param poll_interval Seconds between status checks.
#'
#' @return A tibble of the basket contents, one row per series.
#' @export
#'
#' @examples
#' \dontrun{
#' moodys_colorado_forecast <- get_moodys_basket("colorado_sector_emp")
#' }
get_moodys_basket <- function(
  BASKET_NAME,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc"),
  max_wait = 600,
  poll_interval = 10
) {
  # Find the basket by name and keep its ID.
  baskets <- call_api_moodys("baskets/", accKey, encKey) |>
    .moodys_json()
  basketID <- baskets$basketId[baskets$name == BASKET_NAME]

  if (length(basketID) == 0) {
    stop(
      "No Moody's basket named '",
      BASKET_NAME,
      "'. Available baskets: ",
      paste(baskets$name, collapse = ", "),
      call. = FALSE
    )
  }

  # Running a basket requires a POST.
  order <- call_api_moodys(
    paste0("orders?type=baskets&action=run&id=", basketID),
    accKey,
    encKey,
    type = "POST"
  ) |>
    .moodys_json()
  orderID <- order$orderId

  # Poll until the order finishes. Bounded, so a stuck order cannot hang the
  # session indefinitely.
  deadline <- Sys.time() + max_wait
  repeat {
    status <- call_api_moodys(paste0("orders/", orderID), accKey, encKey) |>
      .moodys_json()

    if (!isTRUE(status$processing)) {
      break
    }

    if (Sys.time() > deadline) {
      stop(
        "Moody's basket '",
        BASKET_NAME,
        "' was still processing after ",
        max_wait,
        " seconds (order ",
        orderID,
        "). Increase `max_wait` or check the order in Data Buffet.",
        call. = FALSE
      )
    }

    Sys.sleep(poll_interval)
  }

  # Download the completed output and parse it straight from the response
  # body. Nothing is written to disk; earlier versions wrote a `basket.data`
  # file into the working directory.
  body <- call_api_moodys(
    paste0("orders?type=baskets&id=", basketID),
    accKey,
    encKey
  ) |>
    httr2::resp_body_string()

  # The API returns CRLF-terminated lines. Writing those through a text-mode
  # connection on Windows produced "\r\r\n", which readr does not merely
  # mis-parse -- it crashes the R session. Normalise before parsing.
  body <- gsub("\r\n?", "\n", body)

  readr::read_csv(I(body), show_col_types = FALSE)
}
