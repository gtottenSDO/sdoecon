#' Get Moody's basket data
#' @description
#' Function to get Moody's data for a specific basket.
#' Data is converted to long format and statewide and Denver Boulder MSA are aggregated.
#'
#' Function based on Moody's API documentation:
#' https://www.economy.com/support/apis/getting-started
#'
#' @param BASKET_NAME Name of basket to download
#' @param accKey Moody's API acccess key (defaults to environment variable if installed using set_moodys_api_key)
#' @param encKey Moody's API encryption key (defaults to environment variable if installed using set_moodys_api_key)
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @import stringr
#'
#' @return Data frame with long format data
#' @export
#'
#' @examples
#' moodys_colorado_forecast <- basket_call(BASKET_NAME = "colorado_sector_emp")

get_moodys_basket <- function(BASKET_NAME, accKey = Sys.getenv("MOODYS_ACC_KEY"), encKey = Sys.getenv("MOODYS_ENC_KEY")){

  #####
  # Setup:
  # 1. Store your access key, encryption key, and basket name.
  # Get your keys at:
  # https://www.economy.com/myeconomy/api-key-info


  #####
  # Identify a basket to execute:
  # 2. Get list of baskets.
  # 3. Extract the basket with a given name, and save its ID for later.
  baskets.json <- call_api_moodys("baskets/", accKey, encKey, type = "GET")
  baskets      <- fromJSON(httr::content(baskets.json, as = "text"))
  basketID     <- baskets$basketId[baskets$name == BASKET_NAME]

  # 4. Execute a particular basket using its ID.
  # This requires that the optional argument “type” be set to "POST".
  call    <- paste("orders?type=baskets&action=run&id=", basketID, sep = "")
  order   <- call_api_moodys(call, accKey, encKey, type = "POST")
  orderID <- fromJSON(httr::content(order, as = "text"))$orderId

  #####
  # Download the output:
  # 5. Periodically check if the order has completed.
  call <- paste("orders/", orderID, sep = "")
  processing.check <- TRUE
  while(processing.check) {
    status <- call_api_moodys(call, accKey, encKey, type = "GET")
    processing.check <- fromJSON(httr::content(status, as = "text"))$processing
    Sys.sleep(10)
  }
  rm(status)

  # 6. Download completed output.
  call    <- paste("orders?type=baskets&id=", basketID, sep = "")
  request <- call_api_moodys(call, accKey, encKey, type = "GET")

  # 7. This works for CSV baskets:
  cat(httr::content(request, as = "text", type = "text/html", encoding = "UTF-8"),
      file = "basket.data", sep = "\n")

  df <- read_csv("basket.data")
return(df)
}


