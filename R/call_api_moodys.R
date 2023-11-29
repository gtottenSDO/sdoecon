#' Call Moody's API
#'
#' @description
#' Function to make a call based on the Moody's economy.com API.
#'
#' Based on the Moody's API documentation:
#' https://www.economy.com/support/apis/getting-started
#'
#' @param apiCommand Moody's API command
#' @param accKey Moody's API acccess key (defaults to environment variable if installed using set_moodys_api_key)
#' @param encKey Moody's API encryption key (defaults to environment variable if installed using set_moodys_api_key)
#' @param type GET or POST command to pass to API
#'
#' @return httr request object
#' @export

call_api_moodys <- function(apiCommand, accKey = Sys.getenv('MOODYS_ACC_KEY'), encKey = Sys.getenv("MOODYS_ENC_KEY"), type = "GET"){
  url <- paste("https://api.economy.com/data/v1/", apiCommand, sep = "")
  print(url)
  timeStamp <- format(as.POSIXct(Sys.time()), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  hashMsg   <- paste(accKey, timeStamp, sep = "")
  signature <- digest::hmac(encKey, hashMsg, "sha256")

  Sys.sleep(1)
  if (type == "POST") {
    req <- httr::POST(url, httr::add_headers("AccessKeyId" = accKey,
                                             "Signature" = signature,
                                             "TimeStamp" = timeStamp))
  } else {
    req <- httr::GET(url, httr::add_headers("AccessKeyId" = accKey,
                                            "Signature" = signature,
                                            "TimeStamp" = timeStamp))
  }
  return(req)
}
