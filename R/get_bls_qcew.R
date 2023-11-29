#' Function to get BLS data from the BLS
#' Quarterly Census of Employment and Wages (QCEW)
#'
#' Description of the data:
#' https://www.bls.gov/cew/data-overview.htm
#' Description of downloadable zip files:
#' https://www.bls.gov/cew/downloadable-data-files.htm
#'
#' Description of api:
#' https://www.bls.gov/cew/additional-resources/open-data/home.htm
#'
#' @param year Year to download
#' @param freq Frequency of data to download, must be either "q" (quarterly) or "a" (annual) when downloading zip files, or be the quarter number (1-4) or "a" (annual) when using the api call
#' @param call_type Either "zip" or "api"
#' @param state State FIPS code for calling API or filtering downloaded
#' zip files
#' @param county County FIPS code for calling API or filtering downloaded
#' zip files
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @importFrom vroom vroom cols
#'
#' @return A data frame with the requested data
#' @export
#'
#' @examples
#' qcew_2020_all <- get_bls_qcew(year = 2020, freq = "q", call_type = "zip")

get_bls_qcew <- function(year = 2022,
                         freq = "a",
                         call_type = "zip",
                         state = "08",
                         county = NULL) {
  assertthat::assert_that(call_type %in% c("zip", "api"),
                          msg = "Call type must be either
                          zip or api")

  if (call_type == "zip") {
    assertthat::assert_that(freq %in% c("q", "a"),
                            msg = "Frequency must be either
                            q (quarterly) or a (annual)")
    if (freq == "q") {
      freq <- "qtrly"
    } else {
      freq <- "annual"
    }
    url <- paste0(
      "https://data.bls.gov/cew/data/files/",
      year,
      "/csv/",
      year,
      "_",
      freq,
      "_singlefile.zip"
    )

    if(is.null(county)){
      county <- sdoecon::geography_xwalk$county_fips
    } else {
      county
    }

    tf <- tempfile()

    download.file(url, tf, mode = "wb")

    data <-  vroom::vroom(tf, col_types = cols(.default = "c")) %>%
      dplyr::filter(stringr::str_sub(area_fips, 1, 2) %in% state &
                      stringr::str_sub(area_fips, 3, 5) %in% county)

    return(data)

  } else {

    assertthat::assert_that(!is.null(county),
                            msg = "County must be specified
                            when using the api call")

    assertthat::assert_that(year >= (lubridate::year(Sys.Date()) - 5),
                            msg = "Year must be within the past 5 years
                            when using the api call")

    assertthat::assert_that(freq %in% c(1:4, "a"),
                            msg = "Frequency must be either
                            a specific quarter, or a (annual)")

    assert_that(length(state) == 1 & length(county) == 1,
                msg = "State and county must be single values")

    url = paste0("https://data.bls.gov/cew/data/api/",
                 year,
                 "/",
                 freq,
                 "/area/",
                 state,
                 county,
                 ".csv")

    data <- vroom(url, col_types = "c")

    return(data)
  }
}
