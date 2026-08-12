# Fixture is a trimmed real basket: 8 Colorado counties, 7 of which make up
# the Denver-Boulder region (Alamosa, 003, deliberately does not).

read_fixture <- function() {
  readr::read_csv(
    test_path("fixtures", "moodys_basket_sample.csv"),
    show_col_types = FALSE
  )
}

test_that("process_moodys() labels every geography type it produces", {
  skip_if_no_sdo_db()

  out <- process_moodys(read_fixture())

  expect_s3_class(out, "data.frame")
  expect_true(all(
    c("geo_type", "fips_long", "county_fips", "value") %in% names(out)
  ))
  expect_setequal(
    unique(out$geo_type),
    c("county", "statewide", "denver_boulder_msa")
  )
})

test_that("process_moodys() aggregates Denver-Boulder over 7 counties only", {
  skip_if_no_sdo_db()

  raw <- read_fixture()
  out <- process_moodys(raw)

  db <- dplyr::filter(out, .data$geo_type == "denver_boulder_msa")
  expect_true(all(db$fips_long == "08500"))
  expect_true(all(db$county_fips == "500"))

  counties <- dplyr::filter(out, .data$geo_type == "county")
  expect_equal(dplyr::n_distinct(counties$fips_long), 8)

  # Denver-Boulder excludes Alamosa; statewide includes it.
  one_date <- min(db$date, na.rm = TRUE)
  db_total <- sum(db$value[db$date == one_date])
  county_total <- sum(counties$value[counties$date == one_date])
  expect_lt(db_total, county_total)
})

test_that("process_moodys() converts thousands to counts", {
  skip_if_no_sdo_db()

  raw <- read_fixture()
  out <- process_moodys(raw)

  first_raw <- raw[[7]][1] # first date column, Adams County
  first_out <- out |>
    dplyr::filter(.data$geo_type == "county", .data$fips_long == "08001") |>
    dplyr::arrange(.data$date) |>
    dplyr::pull("value") |>
    utils::head(1)

  expect_equal(first_out, first_raw * 1000)
})

test_that("the trailing-comma placeholder column is dropped", {
  skip_if_no_sdo_db()

  raw <- read_fixture()
  # The reader names the trailing empty column "...91".
  expect_true(any(grepl("^\\.\\.\\.[0-9]+$", names(raw))))

  out <- process_moodys(raw)

  expect_false(any(is.na(out$date)))
  expect_false(any(is.na(out$value)))
})

test_that("column names are lowercased with spaces replaced", {
  skip_if_no_sdo_db()

  out <- process_moodys(read_fixture())

  expect_true("native_frequency" %in% names(out))
  expect_false(any(grepl(" ", names(out))))
  expect_equal(names(out), tolower(names(out)))
})
