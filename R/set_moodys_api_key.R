#' Install Moodys API Keys
#'
#' @param accKey Moodys API Access Key
#' @param encKey Moodys API Encryption Key
#' @param permanent Permanently write keys to user environment
#'
#' @export


set_moodys_api_key <- function(accKey, encKey, permanent = TRUE) {
  if (permanent) {
    renviron_path <- file.path(path.expand("~"), ".Renviron")

    # If the file doesn't exist, create it
    if (!file.exists(renviron_path)) {
      file.create(renviron_path)
    }

    # Read the current .Renviron content
    lines <- readLines(renviron_path, warn = FALSE)

    # Define the patterns for the keys
    patterns <- c("^MOODYS_ACC_KEY=", "^MOODYS_ENC_KEY=")

    # Remove any existing entries for the keys
    for (pattern in patterns) {
      lines <- lines[!grepl(pattern, lines)]
    }

    # Add the new API keys
    lines <- c(lines, paste0("MOODYS_ACC_KEY='", accKey, "'"), paste0("MOODYS_ENC_KEY='", encKey, "'"))

    # Write back to .Renviron
    writeLines(lines, renviron_path)

    # Inform the user to restart R for the changes to take effect
    message("API keys set. Please restart R for the changes to take effect.")
  } else {
  print("Installing API keys to current session")
  Sys.setenv("MOODYS_ACC_KEY" = accKey)
  Sys.setenv("MOODYS_ENC_KEY" = encKey)
  }
}

