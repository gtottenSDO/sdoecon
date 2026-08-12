# Parity tests for the crosswalk accessors.
#
# Until v0.1.0 these were bundled .rda datasets. The expected shapes below were
# captured from those datasets before they were deleted, so these tests assert
# that the database-backed replacements are drop-in compatible.

test_that("geography_xwalk() matches the dataset it replaced", {
  skip_if_no_sdo_db()

  x <- geography_xwalk()

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 66)
  expect_named(
    x,
    c(
      "fips_long",
      "county_fips",
      "county_name",
      "pmreg_code",
      "pmreg_name",
      "superpm_name",
      "tourism_region_code",
      "base_region_code"
    )
  )
  # The statewide row is relied on by downstream joins.
  expect_true("08000" %in% x$fips_long)
})

test_that("region_xwalk() drops the database-only columns", {
  skip_if_no_sdo_db()

  x <- region_xwalk()

  expect_equal(nrow(x), 159)
  expect_named(
    x,
    c("region_code", "region_name", "county_fips", "county_name")
  )
  # econ.crosswalk_region also carries these; they were not in the dataset.
  expect_false(any(c("region", "countyfips") %in% names(x)))
})

test_that("gcode_n2_crosswalk() renames the database columns", {
  skip_if_no_sdo_db()

  x <- gcode_n2_crosswalk()

  expect_equal(nrow(x), 23)
  expect_named(
    x,
    c("ownership_code", "gcode", "gcode_label", "naics_code", "naics_label")
  )
  expect_false(any(c("coc", "naics2") %in% names(x)))
})

test_that("denver_boulder_geography_xwalk() rebuilds the legacy 500 shape", {
  skip_if_no_sdo_db()

  x <- denver_boulder_geography_xwalk()

  expect_equal(nrow(x), 7)
  expect_named(
    x,
    c("area", "county_fips", "county_name", "area_fips", "fips_long")
  )
  expect_equal(
    sort(x$county_fips),
    c("001", "005", "013", "014", "031", "035", "059")
  )
  # The SDO 500 convention, not the database's internal 521 area id.
  expect_true(all(x$area_fips == "500"))
  expect_true(all(x$area == "Denver-Boulder Region"))
  expect_false(any(grepl("521", x$area_fips)))
  expect_equal(x$fips_long, paste0("08", x$county_fips))
  expect_false(anyNA(x$county_name))
})

test_that("crosswalks are cached and the cache can be cleared", {
  skip_if_no_sdo_db()

  sdoecon_xwalk_refresh()

  first <- geography_xwalk()
  second <- geography_xwalk()
  expect_equal(first, second)

  # A populated cache means the second call did not re-query.
  expect_true("econ.crosswalk_econ" %in% ls(sdoecon:::.xwalk_cache))

  cleared <- sdoecon_xwalk_refresh()
  expect_true("econ.crosswalk_econ" %in% cleared)
  expect_length(ls(sdoecon:::.xwalk_cache), 0)
})

test_that("an explicit connection bypasses the cache", {
  skip_if_no_sdo_db()

  sdoecon_xwalk_refresh()

  con <- sdotools::sdo_db_connect(public = TRUE)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  x <- geography_xwalk(con = con)

  expect_equal(nrow(x), 66)
  expect_length(ls(sdoecon:::.xwalk_cache), 0)
})
