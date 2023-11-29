#' Denver Boulder Geography Crosswalk
#'
#' A crosswalk file containing the counties in the Denver Boulder MSA
#'
#' @format A data frame with 7 rows and 5 variables:
#' \describe{
#'  \item{county_fips}{County FIPS code}
#'  \item{county_name}{County name}
#'  \item{area}{Label "Denver-Boulder Region" for labeling in other datasets}
#'  \item{area_fips}{FIPS code for the Denver-Boulder Region (500)}
#'  \item{fips_long}{County FIPS codes converted to 5 digits}
#' }
#'
#' @source Colorado State Demography Office Crosswalk
"denver_boulder_geography_xwalk"



#' Geography Crosswalk
#'
#' A crosswalk file containing the counties in Colorado for mapping them to regions
#'
#' @format A data frame with 65 rows and 8 variables:
#' \describe{
#'  \item{fips_long}{County FIPS codes converted to 5 digits}
#'  \item{county_fips}{County FIPS code}
#'  \item{county_name}{County name}
#'  \item{pmreg_code}{Codes for identifying planning and management regions}
#'  \item{pmreg_name}{Names for identifying planning and management regions}
#'  \item{super_pm_name}{Names for identifying super planning and management regions}
#'  \item{touism_region_code}{Codes for identifying tourism regions}
#'  \item{base_region_code}{Codes for identifying base regions}
#' }
#'
#' @source Colorado State Demography Office Crosswalk
"geography_xwalk"

#' Gcode to Naics 2 Digit Crosswalk
#'
#' A crosswalk file containing the SDO industry codes and the NAICS 2 digit codes
#'
#' @format A data frame with 23 rows and 5 variables:
#' \describe{
#' \item{ownership_code}{Ownership code}
#' \item{gcode}{SDO industry code}
#' \item{gcode_label}{SDO industry label}
#' \item{naics_code}{NAICS 2 digit code}
#' \item{naics_label}{NAICS 2 digit label}
#' }
#'
#' @source Colorado State Demography Office Crosswalk
"gcode_n2_crosswalk"

