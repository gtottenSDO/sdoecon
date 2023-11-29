#' Leifa Jobs
#'
#' Historical Leifa data produced by the Colorado State Demography Office,
#' in Long Format
#'
#' @format A data frame with 396396 rows and 7 variables:
#' \describe{
#'  \item{fips_long}{County FIPS codes converted to 5 digits}
#'  \item{county_fips}{3 digit FIPS code}
#'  \item{area}{Area name}
#'  \item{gcode}{SDO industry code}
#'  \item{leifa_group}{Grouping to describe type of worker. Either
#'  "WS" = Wage and Salary,
#'  "Prop = Sole Propreitor,
#'  "Jobs" = All workers (i.e., WS + Prop)}
#'  \item{year}{Year applicable to estimate}
#'  \item{total}{value applicable to estimate}
#' }
#'
#' @source Colorado State Demography Office Crosswalk
"leifa_long"
