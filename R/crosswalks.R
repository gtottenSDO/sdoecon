# Crosswalk accessors.
#
# These were bundled .rda datasets until v0.1.0. They now live in the SDO
# Postgres database and are read through sdotools. Note that
# sdotools::sdo_xwalk_get() cannot be used here: it hard-codes the "xwalk"
# schema, and the economic crosswalks live in "econ".

#' Pull a crosswalk table from the SDO database
#'
#' @param schema,table Schema and table to read.
#' @param con Optional open connection. When supplied the session cache is
#'   bypassed, since the caller is managing the connection deliberately.
#'
#' @return A tibble.
#' @keywords internal
#' @noRd
.sdo_xwalk_pull <- function(schema, table, con = NULL) {
  .cached(
    key = paste(schema, table, sep = "."),
    # vintage = NULL because these reference tables carry no model_id column;
    # the default "latest" would both error and emit a vintage message.
    value = sdotools::sdo_db_pull_table(
      schema = schema,
      table = table,
      vintage = NULL,
      public = TRUE,
      con = con
    ),
    use_cache = is.null(con)
  )
}

#' Geography Crosswalk
#'
#' Colorado counties mapped to planning and management regions, super planning
#' and management regions, tourism regions, and base regions. Includes a
#' statewide row (`fips_long` `"08000"`).
#'
#' Read from `econ.crosswalk_econ` in the SDO Postgres database. Prior to
#' version 0.1.0 this was a bundled dataset; it is now a function, so existing
#' code needs `geography_xwalk()` rather than `geography_xwalk`.
#'
#' @param con Optional open database connection, as returned by
#'   [sdotools::sdo_db_connect()]. Supply one to reuse a connection across
#'   several calls; otherwise a short-lived read-only connection is opened.
#'
#' @return A tibble with 66 rows and 8 variables:
#' \describe{
#'  \item{fips_long}{County FIPS code, 5 digits}
#'  \item{county_fips}{County FIPS code, 3 digits}
#'  \item{county_name}{County name}
#'  \item{pmreg_code}{Planning and management region code}
#'  \item{pmreg_name}{Planning and management region name}
#'  \item{superpm_name}{Super planning and management region name}
#'  \item{tourism_region_code}{Tourism region code}
#'  \item{base_region_code}{Base region code}
#' }
#'
#' @source Colorado State Demography Office, `econ.crosswalk_econ`
#' @seealso [region_xwalk()], [denver_boulder_geography_xwalk()],
#'   [sdoecon_xwalk_refresh()]
#' @export
#'
#' @examples
#' \dontrun{
#' geography_xwalk()
#' }
geography_xwalk <- function(con = NULL) {
  .sdo_xwalk_pull("econ", "crosswalk_econ", con = con)
}

#' Region Crosswalk
#'
#' Colorado counties mapped to regions. Counties appear once per region they
#' belong to, so the table is longer than the county count.
#'
#' Read from `econ.crosswalk_region` in the SDO Postgres database. The database
#' table also carries `region` and `countyfips` columns; these are dropped here
#' to preserve the shape of the dataset this function replaced.
#'
#' @inheritParams geography_xwalk
#'
#' @return A tibble with 159 rows and 4 variables:
#' \describe{
#'  \item{region_code}{Region code}
#'  \item{region_name}{Region name}
#'  \item{county_fips}{County FIPS code, 3 digits}
#'  \item{county_name}{County name}
#' }
#'
#' @source Colorado State Demography Office, `econ.crosswalk_region`
#' @export
#'
#' @examples
#' \dontrun{
#' region_xwalk()
#' }
region_xwalk <- function(con = NULL) {
  .sdo_xwalk_pull("econ", "crosswalk_region", con = con) |>
    dplyr::select(
      "region_code",
      "region_name",
      "county_fips",
      "county_name"
    )
}

#' Gcode to 2-Digit NAICS Crosswalk
#'
#' SDO industry codes mapped to 2-digit NAICS sectors.
#'
#' Read from `econ.crosswalk_n2_gcode` in the SDO Postgres database. The
#' database names two columns differently (`coc` and `naics2`); they are
#' renamed here to preserve the shape of the dataset this function replaced.
#'
#' @inheritParams geography_xwalk
#'
#' @return A tibble with 23 rows and 5 variables:
#' \describe{
#'  \item{ownership_code}{Ownership code (`coc` in the database)}
#'  \item{gcode}{SDO industry code}
#'  \item{gcode_label}{SDO industry label}
#'  \item{naics_code}{2-digit NAICS code (`naics2` in the database)}
#'  \item{naics_label}{2-digit NAICS label}
#' }
#'
#' @source Colorado State Demography Office, `econ.crosswalk_n2_gcode`
#' @export
#'
#' @examples
#' \dontrun{
#' gcode_n2_crosswalk()
#' }
gcode_n2_crosswalk <- function(con = NULL) {
  .sdo_xwalk_pull("econ", "crosswalk_n2_gcode", con = con) |>
    dplyr::rename(
      ownership_code = "coc",
      naics_code = "naics2"
    ) |>
    dplyr::select(
      "ownership_code",
      "gcode",
      "gcode_label",
      "naics_code",
      "naics_label"
    )
}

#' Denver-Boulder Geography Crosswalk
#'
#' The seven counties making up the Denver-Boulder region.
#'
#' Assembled from `xwalk.area_county` (area 521, "Denver-Boulder Metro Area")
#' joined to [geography_xwalk()] for county names. The office's own `500` area
#' code is applied on the way out: downstream consumers such as
#' [process_moodys()] encode the `500` / `08500` convention, so the database's
#' internal `521` identifier is deliberately not surfaced here.
#'
#' @inheritParams geography_xwalk
#'
#' @return A tibble with 7 rows and 5 variables:
#' \describe{
#'  \item{area}{Always `"Denver-Boulder Region"`, for labeling joined datasets}
#'  \item{county_fips}{County FIPS code, 3 digits}
#'  \item{county_name}{County name}
#'  \item{area_fips}{Always `"500"`, the SDO code for the Denver-Boulder region}
#'  \item{fips_long}{County FIPS code, 5 digits}
#' }
#'
#' @source Colorado State Demography Office, `xwalk.area_county`
#' @export
#'
#' @examples
#' \dontrun{
#' denver_boulder_geography_xwalk()
#' }
denver_boulder_geography_xwalk <- function(con = NULL) {
  counties <- .sdo_xwalk_pull("xwalk", "area_county", con = con) |>
    dplyr::filter(.data$area_id == "521") |>
    dplyr::select("county_fips")

  names <- geography_xwalk(con = con) |>
    dplyr::select("county_fips", "county_name")

  counties |>
    dplyr::left_join(names, by = "county_fips") |>
    dplyr::mutate(
      area = "Denver-Boulder Region",
      area_fips = "500",
      fips_long = paste0("08", .data$county_fips)
    ) |>
    dplyr::select(
      "area",
      "county_fips",
      "county_name",
      "area_fips",
      "fips_long"
    ) |>
    dplyr::arrange(.data$county_fips)
}
