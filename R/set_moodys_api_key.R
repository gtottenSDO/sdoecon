moodys_keyring_service <- "moodys"

#' Store Moody's API keys
#'
#' Get your keys at <https://www.economy.com/myeconomy/api-key-info>.
#'
#' The default backend is the system credential store, matching the
#' `sdo_db` pattern used by `sdotools`. Keys written to `.Renviron` by earlier
#' versions of this package continue to work -- see [moodys_key()] for the
#' resolution order.
#'
#' @param accKey Moody's API access key.
#' @param encKey Moody's API encryption key.
#' @param backend Where to store the keys. One of `"keyring"` (the system
#'   credential store, recommended), `"renviron"` (appended to `~/.Renviron`,
#'   requiring an R restart), or `"session"` (environment variables for the
#'   current session only).
#' @param permanent Deprecated. `permanent = FALSE` is equivalent to
#'   `backend = "session"`.
#' @param renviron_path Path to the `.Renviron` file to write when
#'   `backend = "renviron"`. Defaults to the one in your home directory.
#'   Exposed mainly so this can be tested without touching a real `.Renviron`:
#'   note that R resolves `~` once at startup, so overriding the `HOME`
#'   environment variable does *not* redirect the default.
#'
#' @return Invisibly, the backend used.
#' @export
#'
#' @examples
#' \dontrun{
#' set_moodys_api_key("my-access-key", "my-encryption-key")
#' }
set_moodys_api_key <- function(
  accKey,
  encKey,
  backend = c("keyring", "renviron", "session"),
  permanent = NULL,
  renviron_path = file.path(path.expand("~"), ".Renviron")
) {
  backend <- match.arg(backend)

  if (!is.null(permanent)) {
    warning(
      "`permanent` is deprecated; use `backend` instead.",
      call. = FALSE
    )
    if (isFALSE(permanent)) {
      backend <- "session"
    }
  }

  switch(
    backend,
    keyring = {
      keyring::key_set_with_value(
        moodys_keyring_service,
        username = "acc_key",
        password = accKey
      )
      keyring::key_set_with_value(
        moodys_keyring_service,
        username = "enc_key",
        password = encKey
      )
      message("Moody's API keys stored in the system credential store.")
    },
    renviron = {
      lines <- if (file.exists(renviron_path)) {
        readLines(renviron_path, warn = FALSE)
      } else {
        character()
      }

      # Drop any existing entries before appending the new ones.
      lines <- lines[!grepl("^MOODYS_(ACC|ENC)_KEY=", lines)]
      lines <- c(
        lines,
        paste0("MOODYS_ACC_KEY='", accKey, "'"),
        paste0("MOODYS_ENC_KEY='", encKey, "'")
      )

      writeLines(lines, renviron_path)
      message(
        "Moody's API keys written to ",
        renviron_path,
        ". Restart R for the changes to take effect."
      )
    },
    session = {
      Sys.setenv("MOODYS_ACC_KEY" = accKey, "MOODYS_ENC_KEY" = encKey)
      message("Moody's API keys set for this session only.")
    }
  )

  invisible(backend)
}

#' Retrieve a stored Moody's API key
#'
#' Resolves a key from, in order: the system credential store (service
#' `"moodys"`), then the `MOODYS_ACC_KEY` / `MOODYS_ENC_KEY` environment
#' variables. Errors if neither is set.
#'
#' This is the default for the `accKey` and `encKey` arguments throughout the
#' package, so it is rarely called directly.
#'
#' @param which Which key to retrieve, `"acc"` or `"enc"`.
#'
#' @return The key, as a length-1 character vector.
#' @export
#'
#' @examples
#' \dontrun{
#' moodys_key("acc")
#' }
moodys_key <- function(which = c("acc", "enc")) {
  which <- match.arg(which)
  username <- paste0(which, "_key")
  envvar <- paste0("MOODYS_", toupper(which), "_KEY")

  from_keyring <- tryCatch(
    keyring::key_get(moodys_keyring_service, username = username),
    error = function(e) NULL
  )
  if (!is.null(from_keyring) && nzchar(from_keyring)) {
    return(from_keyring)
  }

  from_env <- Sys.getenv(envvar, unset = "")
  if (nzchar(from_env)) {
    return(from_env)
  }

  stop(
    "No Moody's ",
    which,
    " key found. Store one with `set_moodys_api_key()`, or set the ",
    envvar,
    " environment variable.",
    call. = FALSE
  )
}
