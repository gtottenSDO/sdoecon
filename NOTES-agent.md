# Agent notes

Repo-specific gotchas that cost real time to work out. Newest first.

## readr crashes R on Moody's basket files (`\r\r\n`)

**Symptom:** `Rscript` dies with **exit code 5**, no error, no traceback. Any
`cat()`/`message()` after the failing call never appears, so it looks like the
process silently stops at the wrong line. `readLines()` on the same file works
fine; only `readr::read_csv()` crashes. Copying the file elsewhere and reading
the copy also works, which makes it look like a path problem. It isn't.

**Cause:** the file contains `\r\r\n` line terminators. The Moody's API returns
CRLF-terminated lines, and the old `get_moodys_basket()` wrote them back out
with `cat(..., sep = "\n")`. On Windows that connection is in text mode, so the
`\n` became another `\r\n` -- giving `\r\r\n`. readr 2.2.0 / vroom 1.7.1 does
not error on that, it segfaults.

The committed `basket.data` had this corruption, which is why the test fixture
derived from it crashed the suite.

**Fix:** normalise before parsing -- `gsub("\r\n?", "\n", body)` -- and parse
straight from the string with `readr::read_csv(I(body))` rather than round-
tripping through a file. Test fixtures derived from real API output must be
normalised too; check with
`sum(readBin(f, "raw", file.size(f)) == as.raw(13))`.

**Debugging tip:** when a crash swallows stdout, write progress to a file
connection with `flush()` after each line. That is how the failing call was
located; `cat` to the console was useless because the buffer died with the
process.

## `withr::local_envvar(HOME = ...)` does not redirect `path.expand("~")`

R resolves `~` from `R_USER` **once at startup**. Overriding `HOME` mid-session
has no effect on `path.expand("~")` on Windows. A test that set a temp `HOME`
and then called a function writing to `file.path(path.expand("~"), ".Renviron")`
rewrote the developer's **real** `~/.Renviron`, destroying the stored Moody's
API keys.

**Rule:** any function that writes to a path derived from `~` must take that
path as an argument so tests can redirect it. `set_moodys_api_key()` now has
`renviron_path` for exactly this reason. Never test such a function by
manipulating `HOME`.

On this machine `path.expand("~")` is `C:\Users\sdo`, but there are also
`.Renviron` files in `C:\Users\sdo\Documents`. Know which one R actually loads
before touching either.

## dplyr masking: local variables named after columns

`process_moodys()` renames its columns to lowercase, producing a `geography`
column. A local variable also called `geography` (the crosswalk) was shadowed
inside `dplyr::filter()`, giving
`$ operator is invalid for atomic vectors`. Name crosswalk locals something
that cannot collide (`geo_xwalk`, `db_xwalk`) and reference them with `.env$`.

## Crosswalks live in Postgres, not in this package

The four crosswalks were bundled `.rda` datasets until v0.1.0 and are now
functions reading `econ.*` via `sdotools`. Note that
`sdotools::sdo_xwalk_get()` **cannot** be used for them -- it hard-codes the
`xwalk` schema. Use `sdotools::sdo_db_pull_table(schema, table)` and pass
`vintage = NULL`; these reference tables have no `model_id` column, so the
default `vintage = "latest"` both errors and emits a message.

`denver_boulder_geography_xwalk()` maps to database area id **521**, but must
return the office's **500** convention, which `process_moodys()` depends on.
