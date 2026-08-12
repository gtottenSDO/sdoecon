# sdoecon 0.1.0

First release after a full review and cleanup. This version contains breaking
changes and one change to published output; read the first three sections
before upgrading.

## Breaking: crosswalks are now functions, not datasets

The four crosswalks used to ship as bundled `.rda` datasets. They now live in
the SDO Postgres database and are read through `sdotools`, so stale copies can
no longer drift from the server. **Add `()` to existing calls:**

``` r
# Before
sdoecon::geography_xwalk$county_fips

# After
sdoecon::geography_xwalk()$county_fips
```

This affects `geography_xwalk()`, `region_xwalk()`, `gcode_n2_crosswalk()` and
`denver_boulder_geography_xwalk()`. The returned shapes are unchanged --
same columns, same names, same row counts -- and there is a parity test for
each. `denver_boulder_geography_xwalk()` still reports the office's `500` area
code, not the database's internal `521` identifier.

These functions now require network access to the SDO Postgres host. They do
not require credentials: they connect with the shared read-only account. Each
crosswalk is cached for the session; call `sdoecon_xwalk_refresh()` to force a
re-query. Pass `con` to reuse an existing connection.

`leifa_long` and `jobs_forecast` have no database equivalent and remain
bundled datasets.

## Breaking: `call_api_moodys()` returns an httr2 response

The Moody's functions were split across two HTTP stacks. All of them are now on
`httr2`, so `call_api_moodys()` returns an `httr2_response` rather than an
`httr` response. Read it with `httr2::resp_body_json()` instead of
`httr::content()`. The HMAC-SHA256 signing scheme is unchanged.

`get_moodys_vintages()` and `get_moodys_codes()` still return parsed data
frames, so their contracts are unaffected.

## Changed output: statewide aggregates in `process_moodys()`

`process_moodys()` builds its `"statewide"` rows by summing everything it has
labeled `"county"`, and it labeled counties by matching against all of
`geography_xwalk()$fips_long` -- which carries a statewide row, `08000`. A
basket that included Colorado's own series therefore had that series counted as
a county *and* added to the sum of the counties, so the reported statewide
figure came out at roughly twice the truth.

`08000` is now excluded from the county match. **Statewide values will differ
from prior vintages for any basket that carried the state series**; the new
figures are the sum of the counties alone. County and Denver-Boulder rows are
unaffected.

## API keys

`set_moodys_api_key()` now stores keys in the system credential store by
default, matching the `sdo_db` pattern in `sdotools`. Keys previously written
to `.Renviron` continue to work -- `moodys_key()` checks the credential store
first, then `MOODYS_ACC_KEY` / `MOODYS_ENC_KEY`. Use
`backend = "renviron"` to keep the old behavior. The `permanent` argument is
deprecated: `permanent = TRUE` maps to `backend = "renviron"`, which is what it
used to do, and `permanent = FALSE` to `backend = "session"`.

## Bug fixes

* `get_moodys_series()`, `search_moodys_series()` and
  `search_moodys_series_all()` ignored their own `accKey` / `encKey` arguments.
  The internal request builder minted the bearer token from the stored keys
  regardless, so passing keys explicitly did nothing -- and with no key stored,
  a call supplying valid keys still failed with "No Moody's acc key found". The
  cached token is now scoped to the credentials that minted it, so a call
  naming a second account no longer reuses the first account's token.
* `get_bls_qcew()` defaulted `county` to every `county_fips` in
  `geography_xwalk()`, including the statewide `"000"` entry, so the zip branch
  returned the Colorado aggregate (`area_fips` `"08000"`) alongside the county
  records and summing the result double-counted the state. The default is now
  counties only.
* `set_moodys_api_key(permanent = TRUE)` fell through to the credential store
  rather than `.Renviron`. Because `moodys_key()` prefers the credential store,
  that silently shadowed the `.Renviron` the caller expected to edit.
* `Imports` declared bare `httr2` and `readr`. `req_throttle(capacity =,
  fill_time_s =)` requires httr2 >= 1.1.0 and `read_csv(I(body))` requires
  readr >= 2.0.0; on older versions every signed Moody's call failed with an
  "unused arguments" error. Both now carry version floors.
* `search_moodys_series()` could not be called with its own documented
  defaults. `rows > 100 || is.null(rows)` evaluated `NULL > 100` first, which
  produced `logical(0)` and aborted with "argument is of length zero".
* `get_moodys_basket()` wrote a `basket.data` file into the user's working
  directory and read it back with an undeclared `readr` dependency. It now
  parses the response body in memory and writes nothing to disk, and `readr` is
  declared in `Imports`. (The stray `basket.data` this produced has been removed
  from the repository.)
* `get_moodys_basket()` polled for order completion in an unbounded loop; a
  stuck order hung the session. Added `max_wait` (default 600s).
* `get_moodys_basket()` corrupted line endings. The API returns CRLF-terminated
  lines, and writing them back out through a text-mode connection on Windows
  produced `\r\r\n`. `readr` does not merely mis-parse that -- it crashes the R
  session outright. The basket is now parsed directly from the response body
  with line endings normalised first. (The `basket.data` file removed from the
  repository had exactly this corruption.)
* `process_moodys()` now drops the empty placeholder column that basket CSVs
  produce from their trailing comma. It previously pivoted into a block of
  NA-date, NA-value rows labeled as real geographies, which silently poisoned
  downstream sums.
* `set_moodys_api_key()` gained a `renviron_path` argument, so the
  `backend = "renviron"` path can be pointed somewhere other than your home
  directory.
* `get_moodys_series()` failed for more than 25 mnemonics because `purrr` was
  used but not declared. `purrr`, `tibble`, `rlang` and `readr` are now in
  `Imports`.
* `get_bls_qcew(call_type = "api")` failed because `lubridate` was used but not
  declared; it now uses base R for the year check.
* `get_moodys_codes()` did not apply `match.arg()`, so the default argument
  produced a malformed URL.
* `get_moodys_token()` called `library(httr2)` inside the function.
* `call_api_moodys()` printed the request URL on every call.
* `convert_moodys()` tested response class via `attributes()`; it now uses
  `inherits()`.
* `process_moodys()` replaced only the first space in each column name, and
  emitted grouping messages from `summarize()`.
* API errors are now surfaced: a 401 previously failed as an unrelated JSON
  parse error. Requests retry up to three times and are throttled rather than
  preceded by a blanket `Sys.sleep(1)`.

## Known issue

`process_moodys()` selects national rows with `fips_long == "00"`. No basket
inspected so far contains a 2-character FIP -- every FIP in a Colorado basket
is the 5-character `08xxx` form -- so this branch currently returns zero rows.
It was left unchanged rather than guessed at, because altering it would
silently change published output. Confirm the national FIP against a basket
known to include one before changing it.

## Other

* Added an MIT license, a README, tests, and a real package description;
  `R CMD check` previously failed on the usethis placeholders.
* Documented the `jobs_forecast` dataset, which had no documentation, and
  corrected the `leifa_long` documentation, which described a `fips_long`
  column that does not exist (the column is `bea_fips`).
* Dropped the `%>%` re-export. The package now uses the native `|>` pipe
  throughout, and no longer imports `magrittr` or `httr`.
* `temporary_env` is no longer exported.
