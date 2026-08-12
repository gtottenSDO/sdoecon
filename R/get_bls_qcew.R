#' Get BLS Quarterly Census of Employment and Wages (QCEW) data
#'
#' @description
#' Downloads QCEW data either as a bulk single-file zip (all areas for a year,
#' filtered locally) or through the open-data API (one area at a time, limited
#' to roughly the past five years).
#'
#' Data overview: <https://www.bls.gov/cew/data-overview.htm>;
#' downloadable files: <https://www.bls.gov/cew/downloadable-data-files.htm>;
#' API: <https://www.bls.gov/cew/additional-resources/open-data/home.htm>
#'
#' @param year Year to download.
#' @param freq Frequency. For `call_type = "zip"`, either `"q"` (quarterly) or
#'   `"a"` (annual). For `call_type = "api"`, a quarter number (1-4) or `"a"`.
#' @param call_type Either `"zip"` or `"api"`.
#' @param state State FIPS code.
#' @param county County FIPS code(s). Defaults to every Colorado county in
#'   [geography_xwalk()] for `call_type = "zip"`; required, and a single value,
#'   for `call_type = "api"`.
#'
#' @return A tibble of the requested data, all columns read as character.
#' @export
#'
#' @examples
#' \dontrun{
#' qcew_2020_all <- get_bls_qcew(year = 2020, freq = "q", call_type = "zip")
#' }
get_bls_qcew <- function(
  year = 2022,
  freq = "a",
  call_type = c("zip", "api"),
  state = "08",
  county = NULL
) {
  call_type <- match.arg(call_type)

  if (call_type == "zip") {
    assertthat::assert_that(
      freq %in% c("q", "a"),
      msg = "Frequency must be either q (quarterly) or a (annual)"
    )

    freq <- if (freq == "q") "qtrly" else "annual"

    url <- paste0(
      "https://data.bls.gov/cew/data/files/",
      year,
      "/csv/",
      year,
      "_",
      freq,
      "_singlefile.zip"
    )

    if (is.null(county)) {
      county <- geography_xwalk()$county_fips
    }

    tf <- tempfile(fileext = ".zip")
    on.exit(unlink(tf), add = TRUE)
    utils::download.file(url, tf, mode = "wb")

    vroom::vroom(tf, col_types = vroom::cols(.default = "c")) |>
      dplyr::filter(
        stringr::str_sub(.data$area_fips, 1, 2) %in% state,
        stringr::str_sub(.data$area_fips, 3, 5) %in% county
      )
  } else {
    assertthat::assert_that(
      !is.null(county),
      msg = "County must be specified when using the api call"
    )

    assertthat::assert_that(
      year >= (as.integer(format(Sys.Date(), "%Y")) - 5),
      msg = "Year must be within the past 5 years when using the api call"
    )

    assertthat::assert_that(
      as.character(freq) %in% c("1", "2", "3", "4", "a"),
      msg = "Frequency must be either a specific quarter (1-4), or a (annual)"
    )

    assertthat::assert_that(
      length(state) == 1 && length(county) == 1,
      msg = "State and county must be single values"
    )

    url <- paste0(
      "https://data.bls.gov/cew/data/api/",
      year,
      "/",
      freq,
      "/area/",
      state,
      county,
      ".csv"
    )

    vroom::vroom(url, col_types = vroom::cols(.default = "c"))
  }
}
