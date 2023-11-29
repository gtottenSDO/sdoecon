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
                              accKey = Sys.getenv("MOODYS_ACC_KEY"),
                              encKey = Sys.getenv("MOODYS_ENC_KEY"),
                              freq = "0",
                              trans = "0",
                              vintage = NULL) {

  if (length(mnemonics) > 25){
    stop("Too many mnemonics. Please limit to 25.")
  }

  apiCommand <- paste0(
    #ifelse(length(mnemonics) > 1,
    "multi-", #""),
    "series?m=",
    utils::URLencode(paste0(mnemonics, collapse = ";"), reserved = TRUE),
    "&freq=", freq,
    "&trans=", trans,
    ifelse(is.null(vintage), "", paste0("&vintage=", vintage))
  )

  url <- paste0(
    "https://api.economy.com/data/v1/",
    apiCommand
  )

  timeStamp <- format(as.POSIXct(Sys.time()), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  hashMsg <- paste(accKey, timeStamp, sep = "")
  signature <- digest::hmac(encKey, hashMsg, "sha256")

  Sys.sleep(1)
  req <- httr::GET(url, httr::add_headers(
    "AccessKeyId" = accKey,
    "Signature" = signature,
    "TimeStamp" = timeStamp
  ))
  #### Are You Behind A Proxy? If so, try:
  #   Add the use_proxy argument to httr::GET
  #   req <- httr::GET(url, httr::add_headers("AccessKeyId" = accKey,
  #                                           "Signature" = signature,
  #                                           "TimeStamp" = timeStamp),
  #                  use_proxy("http:://myproxy",80))

  series <- jsonlite::fromJSON(httr::content(req, as = "text"))
  return(series)
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

convert_moodys <- function(series) {


    out <- series %>%
      purrr::pluck("data") %>%
      tidyr::unnest(cols = c(data))

    return(out)

}

