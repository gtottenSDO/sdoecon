#' Get Moody's Data Buffet Series
#'
#' This function retrieves Moody's Data Buffet Series using Mnemonic Code
#' (or set of up to 25 Mnemonic Codes). Access and Encryption Keys can be stored
#' using set_moodys_api_key(), or passed through to the function directly.
#' Frequency, Transformation Code, and Vintage arguments are optional. Use
#' convert_moodys() to create an xts or data frame object.
#'
#' API documentation can be found at https://www.economy.com/products/tools/api#db
#'
#' Script based off of Moody's sample code available at:
#' https://github.com/moodysanalytics/databuffet-api-codesamples/blob/master/R/Single-Series.R
#'
#' @param mnemonics Either a single mnemonic or list of mnemonics (less than 25)
#' to pass to the Moody's API
#' @param accKey Access Key for Moody's API (defaults to environment variable)
#' @param encKey Encryption Key for Moody's API (defaults to environment variable)
#' @param freq Frequency conversion (for list of codes use
#' get_moodys_codes(frequencies), or see the API user guide)
#' @param trans Transformation code (for list of codes see API user guide)
#' @param vintage Get list of available vintages using
#' get_moodys_vintages(mnemonic)
#' @import httr2
#' @importFrom magrittr %>%
#'
#' @return returns list from json. Use function convert_moodys to create
#' a data frame
#' @export
#'
#' @examples
#' jobs_forecast_vintage_202309 <- get_moodys_series("fet.iusa",
#' vintage = "202309") %>%
#'  convert_moodys(type = "xts")




get_moodys_series <- function(mnemonics,
                              freq = "0",
                              trans = "0",
                              vintage = NULL,
                              accKey = Sys.getenv("MOODYS_ACC_KEY"),
                              encKey = Sys.getenv("MOODYS_ENC_KEY")) {



  if (length(mnemonics) > 25) {
    # check for token in the environment


    # break the mnemonics into groups of 25
    m_chunks <- split(mnemonics, ceiling(seq_along(mnemonics) / 25))
  }

  create_req <- function(mnemonics,
                         freq = NULL,
                         trans = NULL,
                         vintage,
                         ...) {
    # check for token in the environment
    check_moodys_token()

    params <- list(
      m = paste0(mnemonics, collapse = ";"),
      freq = freq,
      trans = trans,
      vintage = vintage,
      ...
    )

    headers <- list(Accept = "application/json",
                    Authorization = paste0("Bearer ", temporary_env$token))

    req <- request("https://api.economy.com/data/v1/multi-series/") |>
      req_headers(!!!headers) |>
      req_url_query(!!!params)

    return(req)
  }

  if(length(mnemonics) > 25) {

    req_lst <- m_chunks |>
      map(~create_req(mnemonics = .x,
                      freq,
                      trans,
                      vintage))

    response <- req_lst |>
      req_perform_sequential(progress = TRUE)


  } else {
    req <- create_req(head(mnemonics,25),
                      freq,
                      trans,
                      vintage)

    response <- req_perform(req)

  }


  return(response)
}


#' Convert Moody's Data Buffet series query to an object
#'
#' @param series Object from get_moodys_series()
#' @param series_type Whether the series is a single or multi series query
#'
#' @return Returns a dataframe
#' @export
#'
#' @examples
#' jobs_forecast_vintage_202309 <- get_moodys_series("fet.iusa",
#' vintage = "202309") %>%
#'  convert_moodys(type = "xts")

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

  if(all(length(attributes(resp)) >0,
            attributes(resp)$class == "httr2_response")) {
    out <- process_response(resp)
  } else {
    out <- resp |>
      httr2::resps_successes() |>
      httr2::resps_data(\(resp) process_response(resp))
  }

  return(out)

}




