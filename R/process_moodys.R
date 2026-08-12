#' Process Moody's basket data
#'
#' Reshapes a Moody's basket into a long data frame, converts values to
#' counts (the API reports thousands), and appends statewide and Denver-Boulder
#' MSA aggregates alongside the county rows, each labeled by `geo_type`.
#'
#' @param df A data frame from [get_moodys_basket()].
#'
#' @return A tibble with one row per series, geography and date, containing a
#'   `geo_type` column with values `"national"`, `"statewide"`,
#'   `"denver_boulder_msa"` and `"county"`.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moodys_basket("colorado_sector_emp") |> process_moodys()
#' }
process_moodys <- function(df) {
  # Deliberately not named `geography`: the reshaped data has a column by that
  # name, and dplyr's data masking would resolve it to the column instead.
  geo_xwalk <- geography_xwalk()
  db_xwalk <- denver_boulder_geography_xwalk()

  # Basket CSVs end each row with a trailing comma, so the reader appends an
  # unnamed, entirely empty column ("...91"). Left in place it pivots into a
  # block of NA-date, NA-value rows that quietly poison any downstream sum.
  placeholder <- grepl("^\\.\\.\\.[0-9]+$", names(df)) &
    vapply(df, \(x) all(is.na(x)), logical(1))
  df <- df[, !placeholder, drop = FALSE]

  moodys_long <- df |>
    tidyr::pivot_longer(
      cols = !("Mnemonic":"Native Frequency"),
      names_to = "date",
      values_to = "value"
    ) |>
    dplyr::mutate(
      date = as.Date(.data$date, format = "%m/%d/%Y"),
      short_mnemonic = stringr::str_sub(.data$Mnemonic, end = -7),
      fips_long = .data$FIP,
      county_fips = stringr::str_sub(.data$FIP, -3),
      value = .data$value * 1000,
      .after = "Mnemonic"
    ) |>
    dplyr::select(-"FIP")

  names(moodys_long) <- stringr::str_to_lower(
    stringr::str_replace_all(names(moodys_long), " ", "_")
  )

  # Geography labeling.
  #
  # NOTE: the national filter looks for a 2-character FIP, which no basket
  # observed so far actually contains -- every FIP in a Colorado basket is the
  # 5-character "08xxx" form, so this branch currently yields zero rows. It is
  # left as-is rather than guessed at, because changing it would silently alter
  # published output. Confirm the national FIP against a basket that is known
  # to include one before touching this.
  national <- moodys_long |>
    dplyr::filter(.data$fips_long == "00") |>
    dplyr::mutate(geo_type = "national")

  counties <- moodys_long |>
    dplyr::filter(.data$fips_long %in% .env$geo_xwalk$fips_long) |>
    dplyr::mutate(geo_type = "county")

  statewide <- counties |>
    dplyr::mutate(
      fips_long = "08000",
      county_fips = "000",
      geography = "Colorado",
      mnemonic = paste0(stringr::str_sub(.data$mnemonic, end = -4), "000")
    ) |>
    dplyr::group_by(dplyr::across(-"value")) |>
    dplyr::summarize(value = sum(.data$value), .groups = "drop") |>
    dplyr::mutate(geo_type = "statewide")

  denver_boulder_msa <- moodys_long |>
    dplyr::filter(.data$county_fips %in% .env$db_xwalk$county_fips) |>
    dplyr::mutate(
      fips_long = "08500",
      county_fips = "500",
      geography = "Denver-Boulder MSA",
      mnemonic = paste0(stringr::str_sub(.data$mnemonic, end = -4), "500")
    ) |>
    dplyr::group_by(dplyr::across(-"value")) |>
    dplyr::summarize(value = sum(.data$value), .groups = "drop") |>
    dplyr::mutate(geo_type = "denver_boulder_msa")

  dplyr::bind_rows(national, statewide, denver_boulder_msa, counties)
}
