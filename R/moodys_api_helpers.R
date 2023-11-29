#' Lookup Available Vintages for a Moodys Mnemonic
#'
#' @param mnemonic Mnemonic to lookup
#' @param accKey Access Key for Moody's API (defaults to environment variable)
#' @param encKey Encryption Key for Moody's API (defaults to environment variable)
#'
#' @return Data frame of available vintages
#' @export


get_moodys_vintages <- function(mnemonic,
                                accKey = Sys.getenv("MOODYS_ACC_KEY"),
                                encKey = Sys.getenv("MOODYS_ENC_KEY")){
  url <- paste0("https://api.economy.com/data/v1/vintages?m=",
                utils::URLencode(mnemonic))

  timeStamp <- format(as.POSIXct(Sys.time()), "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  hashMsg   <- paste(accKey, timeStamp, sep="")
  signature <- digest::hmac(encKey, hashMsg, "sha256")

  Sys.sleep(1)
  req <- httr::GET(url, httr::add_headers("AccessKeyId" = accKey,
                                          "Signature" = signature,
                                          "TimeStamp" = timeStamp))

  vintages <- jsonlite::fromJSON(httr::content(req, as="text"))

  return(vintages)
}

#' Table of API Codes
#'
#' @param codetype Either "filetypes" or "frequencies" (filetypes are only for baskets)
#' @param accKey Access Key for Moody's API (defaults to environment variable)
#' @param encKey Encryption Key for Moody's API (defaults to environment variable)
#'
#' @return Data frame of available codes
#' @export


get_moodys_codes <- function(codetype = c("filetypes",
                                          "frequencies"),
                             accKey = Sys.getenv("MOODYS_ACC_KEY"),
                             encKey = Sys.getenv("MOODYS_ENC_KEY")){

  url <- paste0("https://api.economy.com/data/v1/",
                codetype,
                ifelse(codetype == "filetypes","?type=baskets",""))

  timeStamp <- format(as.POSIXct(Sys.time()), "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  hashMsg   <- paste(accKey, timeStamp, sep="")
  signature <- digest::hmac(encKey, hashMsg, "sha256")

  Sys.sleep(1)
  req <- httr::GET(url, httr::add_headers("AccessKeyId" = accKey,
                                          "Signature" = signature,
                                          "TimeStamp" = timeStamp))

  codes <- jsonlite::fromJSON(httr::content(req, as="text"))

  return(codes)
}
