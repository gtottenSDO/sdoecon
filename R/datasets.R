#' Leifa Jobs
#'
#' Historical Leifa jobs data produced by the Colorado State Demography
#' Office, in long format.
#'
#' @format A data frame with 396,396 rows and 7 variables:
#' \describe{
#'  \item{bea_fips}{County FIPS code, 5 digits, on the BEA basis}
#'  \item{county_fips}{County FIPS code, 3 digits}
#'  \item{area}{Area name}
#'  \item{gcode}{SDO industry code}
#'  \item{leifa_group}{Type of worker: `"WS"` (wage and salary),
#'   `"Prop"` (sole proprietor), or `"Jobs"` (all workers, WS + Prop)}
#'  \item{year}{Year the estimate applies to}
#'  \item{total}{Estimated value}
#' }
#'
#' @source Colorado State Demography Office
"leifa_long"

#' Total Jobs Estimates and Forecast
#'
#' County-level total jobs, combining historical estimates with the Colorado
#' State Demography Office forecast. The statewide total is carried as
#' `county_fips` `"000"`.
#'
#' @format A data frame with 3,300 rows and 4 variables:
#' \describe{
#'  \item{county_fips}{County FIPS code, 3 digits; `"000"` is statewide}
#'  \item{population_year}{Year the estimate applies to, 2001 to 2050}
#'  \item{total_jobs}{Total jobs}
#'  \item{data_type}{Either `"ESTIMATE"` (historical) or `"FORECAST"`}
#' }
#'
#' @source Colorado State Demography Office
"jobs_forecast"
