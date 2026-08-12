#' Search Moody's Data Buffet series
#'
#' @description
#' Searches Moody's Data Buffet for series matching a query. Use
#' [convert_moodys_search()] to turn the result into a data frame, or
#' [search_moodys_series_all()] to page through every match.
#'
#' API documentation: <https://www.economy.com/products/tools/api#db>
#'
#' @param query Search query string.
#' @param geo_rfa Geographic region filter, appended to the query as
#'   `geo_rfa:<value>`. Defaults to `"IUSA"` (United States).
#' @param ... Further filter clauses, combined into the query with `AND`.
#' @param start Starting position for pagination, 0-indexed.
#' @param rows Number of rows to return.
#' @param sort Sort criteria. Leave `NULL` to sort by relevance score.
#' @param accKey Moody's API access key. Defaults to the stored key, see
#'   [moodys_key()].
#' @param encKey Moody's API encryption key. Defaults to the stored key.
#'
#' @return An `httr2_response`. Pass it to [convert_moodys_search()].
#' @export
#'
#' @examples
#' \dontrun{
#' unemployment_search <- search_moodys_series("unemployment rate united states")
#' unemployment_df <- convert_moodys_search(unemployment_search)
#'
#' # With pagination
#' gdp_search <- search_moodys_series("GDP", start = 0, rows = 50)
#' }
search_moodys_series <- function(
  query,
  geo_rfa = "IUSA",
  ...,
  start = 0,
  rows = 25,
  sort = NULL,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  if (missing(query) || is.null(query) || !nzchar(query)) {
    stop("Search query cannot be empty", call. = FALSE)
  }

  if (!is.null(rows) && rows > 100) {
    warning(
      "Large row counts may result in slower response times. ",
      "Consider using pagination.",
      call. = FALSE
    )
  }

  params <- list(
    q = paste(query, paste0("geo_rfa:", geo_rfa), ..., sep = " AND "),
    start = start,
    rows = rows
  )

  if (!is.null(sort) && nzchar(sort)) {
    params$sort <- sort
  }

  .moodys_token_req("search", !!!params) |>
    httr2::req_perform()
}

#' Convert Moody's search results to a data frame
#'
#' @param search_response An `httr2_response` from [search_moodys_series()].
#'
#' @return A tibble of search results. Total match count, start position and
#'   returned count are attached as the attributes `total_results`,
#'   `start_position` and `returned_results`; see [summarize_search_results()].
#' @export
#'
#' @examples
#' \dontrun{
#' unemployment_search <- search_moodys_series("unemployment rate")
#' unemployment_df <- convert_moodys_search(unemployment_search)
#' }
convert_moodys_search <- function(search_response) {
  if (!inherits(search_response, "httr2_response")) {
    stop(
      "Input must be an httr2_response object from search_moodys_series()",
      call. = FALSE
    )
  }

  response_data <- httr2::resp_body_json(search_response)

  if (is.null(response_data$results) || response_data$count == 0) {
    warning("No search results found", call. = FALSE)
    return(tibble::tibble())
  }

  search_df <- response_data$results |>
    tibble::tibble() |>
    rlang::set_names("doc") |>
    tidyr::unnest_wider("doc")

  attr(search_df, "total_results") <- response_data$count %||% 0
  attr(search_df, "start_position") <- response_data$start %||% 0
  attr(search_df, "returned_results") <- nrow(search_df)

  search_df
}

#' Summarize Moody's search results
#'
#' @param search_df A data frame from [convert_moodys_search()].
#'
#' @return Invisibly, `search_df`. Called for the printed summary.
#' @export
#'
#' @examples
#' \dontrun{
#' search_moodys_series("unemployment rate") |>
#'   convert_moodys_search() |>
#'   summarize_search_results()
#' }
summarize_search_results <- function(search_df) {
  total <- attr(search_df, "total_results") %||% "Unknown"
  start <- attr(search_df, "start_position") %||% 0
  returned <- attr(search_df, "returned_results") %||% nrow(search_df)

  message("Search Results Summary:")
  message("  Total results found: ", total)
  message("  Starting position: ", start)
  message("  Results returned: ", returned)

  if (ncol(search_df) > 0) {
    message("  Available columns: ", paste(names(search_df), collapse = ", "))
  }

  invisible(search_df)
}

#' Search Moody's series across all result pages
#'
#' Repeatedly calls [search_moodys_series()] to retrieve up to `max_results`
#' matches. Use with care for broad queries.
#'
#' @param query Search query string.
#' @param max_results Maximum number of results to retrieve.
#' @param rows_per_page Number of rows per API call.
#' @param sort Sort criteria. Leave `NULL` to sort by relevance score.
#' @inheritParams search_moodys_series
#'
#' @return A tibble of all retrieved search results, carrying the same
#'   attributes as [convert_moodys_search()].
#' @export
#'
#' @examples
#' \dontrun{
#' search_moodys_series_all("colorado employment", max_results = 200)
#' }
search_moodys_series_all <- function(
  query,
  max_results = 500,
  rows_per_page = 100,
  sort = NULL,
  accKey = moodys_key("acc"),
  encKey = moodys_key("enc")
) {
  initial_df <- search_moodys_series(
    query = query,
    start = 0,
    rows = rows_per_page,
    sort = sort,
    accKey = accKey,
    encKey = encKey
  ) |>
    convert_moodys_search()

  total_available <- attr(initial_df, "total_results") %||% 0

  results_to_get <- min(max_results, total_available)
  if (results_to_get <= rows_per_page) {
    return(initial_df)
  }

  remaining_results <- results_to_get - rows_per_page
  additional_calls <- ceiling(remaining_results / rows_per_page)

  message("Retrieving ", results_to_get, " of ", total_available, " results...")

  all_results <- vector("list", additional_calls + 1)
  all_results[[1]] <- initial_df

  for (i in seq_len(additional_calls)) {
    start_pos <- i * rows_per_page
    rows_this_call <- min(
      rows_per_page,
      remaining_results - (i - 1) * rows_per_page
    )

    if (rows_this_call <= 0) {
      break
    }

    all_results[[i + 1]] <- search_moodys_series(
      query = query,
      start = start_pos,
      rows = rows_this_call,
      sort = sort,
      accKey = accKey,
      encKey = encKey
    ) |>
      convert_moodys_search()
  }

  combined_df <- dplyr::bind_rows(all_results)

  attr(combined_df, "total_results") <- total_available
  attr(combined_df, "start_position") <- 0
  attr(combined_df, "returned_results") <- nrow(combined_df)

  combined_df
}
