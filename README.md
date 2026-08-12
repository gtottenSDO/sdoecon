# sdoecon

Tools used by the Colorado State Demography Office to assemble economic and
jobs data:

* wrappers for the Moody's Analytics Data Buffet API,
* a downloader for the BLS Quarterly Census of Employment and Wages (QCEW),
* accessors for the office's economic crosswalks,
* the historical Leifa jobs series and the total jobs estimate/forecast.

## Installation

`sdoecon` depends on [sdotools](https://github.com/ColoradoDemography/sdotools),
which is also not on CRAN. `remotes` will pick it up from the `Remotes` field:

``` r
# install.packages("remotes")
remotes::install_github("gtottenSDO/sdoecon")
```

## Crosswalks

Crosswalks are read live from the SDO Postgres database rather than bundled
with the package, so they cannot drift from the server copy. Each is a
function:

``` r
library(sdoecon)

geography_xwalk()                 # counties -> PM, super PM, tourism, base regions
region_xwalk()                    # counties -> regions
gcode_n2_crosswalk()              # SDO industry codes -> 2-digit NAICS
denver_boulder_geography_xwalk()  # the 7 Denver-Boulder counties
```

These need network access to the SDO database but **not** credentials --- they
connect with the shared read-only account. Results are cached for the session:

``` r
sdoecon_xwalk_refresh()  # force the next lookup to re-query

# Reuse one connection across several lookups
con <- sdotools::sdo_db_connect(public = TRUE)
geography_xwalk(con = con)
region_xwalk(con = con)
DBI::dbDisconnect(con)
```

If you need a crosswalk that this package does not wrap, reach for
`sdotools::sdo_xwalk_get()` (schema `xwalk`) or
`sdotools::sdo_db_pull_table()` (any schema) directly.

> Upgrading from an earlier version? These were bundled datasets, so existing
> code needs `()` added --- `geography_xwalk()` rather than `geography_xwalk`.
> See `NEWS.md`.

## Moody's Data Buffet

Store your keys once. Get them at
<https://www.economy.com/myeconomy/api-key-info>.

``` r
set_moodys_api_key("your-access-key", "your-encryption-key")
```

Keys go to the system credential store by default. `backend = "renviron"`
writes to `~/.Renviron` instead, and keys already there keep working.

``` r
# Find series
search_moodys_series("colorado total employment") |>
  convert_moodys_search()

# Pull series by mnemonic; more than 25 are batched automatically
get_moodys_series("fet.iusa", vintage = "202309") |>
  convert_moodys()

# Run a saved basket and reshape it with geography labels
get_moodys_basket("colorado_sector_emp") |>
  process_moodys()
```

## BLS QCEW

``` r
# Bulk annual file, filtered to Colorado counties
get_bls_qcew(year = 2022, freq = "a", call_type = "zip")

# Single area via the open-data API (roughly the last five years)
get_bls_qcew(year = 2023, freq = 1, call_type = "api", county = "001")
```
