#' Look up available vintages for a Moody's mnemonic
#'
#' @param mnemonic Mnemonic to look up.
#' @param accKey Moody's API access key. Defaults to the stored key, see
#'   [moodys_key()].
#' @param encKey Moody's API encryption key. Defaults to the stored key.
#'
#' @return A data frame of available vintages.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moodys_vintages("fet.iusa")
#' }
get_moodys_vintages <- function(
  mnemonic,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  .moodys_signed_req(
    "vintages",
    accKey = accKey,
    encKey = encKey,
    m = mnemonic
  ) |>
    httr2::req_perform() |>
    .moodys_json()
}

#' Table of Moody's API codes
#'
#' @param codetype Either `"filetypes"` (basket file types) or
#'   `"frequencies"` (frequency conversion codes).
#' @inheritParams get_moodys_vintages
#'
#' @return A data frame of available codes.
#' @export
#'
#' @examples
#' \dontrun{
#' get_moodys_codes("frequencies")
#' }
get_moodys_codes <- function(
  codetype = c("filetypes", "frequencies"),
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  codetype <- match.arg(codetype)

  params <- if (codetype == "filetypes") list(type = "baskets") else list()

  .moodys_signed_req(codetype, accKey = accKey, encKey = encKey, !!!params) |>
    httr2::req_perform() |>
    .moodys_json()
}
