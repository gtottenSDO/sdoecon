#' Process Moodys Data
#'
#' Process Moody's data into long data frame. Calculate State and Denver Boulder MSA totals. Label county, state, msa and national and combine into single data frame.
#'
#' @param df Data frame of Moody's data
#'
#' @return Data frame of Moody's data
#' @export

process_moodys <- function(df) {
moodys_long <- df %>%
  tidyr::pivot_longer(
    cols = !("Mnemonic":"Native Frequency"),
    names_to = "date",
    values_to = "value"
    ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    short_mnemonic = str_sub(Mnemonic, end = -7),
    fips_long = FIP,
    county_fips =  str_sub(FIP, -3),
    value = value * 1000,
    .after = Mnemonic) %>%
  select(-FIP)

names(moodys_long) <- str_to_lower(str_replace(names(moodys_long), " ", "_"))

# 9. Add geography info
national <- moodys_long %>%
  filter(fips_long == "00") %>%
  mutate(
    geo_type = "national"
    )

counties <- moodys_long %>%
  filter(fips_long %in% geography_xwalk$fips_long) %>%
  mutate(
    geo_type = "county"
  )

statewide <- counties %>%
  mutate(
    fips_long = "08000",
    county_fips = "000",
    geography = "Colorado",
    mnemonic = str_sub(mnemonic, end = -4) %>% paste0("000")
  ) %>%
  group_by(across(-value)) %>%
  summarize(value = sum(value)) %>%
  mutate(
    geo_type = "statewide"
  )

denver_boulder_msa <- moodys_long %>%
  filter(county_fips %in% denver_boulder_geography_xwalk$county_fips) %>%
  mutate(
    fips_long = "08500",
    county_fips = "500",
    geography = "Denver-Boulder MSA",
    mnemonic = str_sub(mnemonic, end = -4) %>% paste0("500")
  ) %>%
  group_by(across(-value)) %>%
  summarize(value = sum(value)) %>%
  mutate(
    geo_type = "denver_boulder_msa"
  )

return(bind_rows(national, statewide, denver_boulder_msa, counties))
}
