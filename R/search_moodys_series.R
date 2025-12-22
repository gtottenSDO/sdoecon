#' Search Moody's Data Buffet Series
#'
#' This function searches Moody's Data Buffet Series using a search query.
#' Access and Encryption Keys can be stored using set_moodys_api_key(),
#' or passed through to the function directly. Use convert_moodys_search()
#' to create a data frame object from the search results.
#'
#' API documentation can be found at https://www.economy.com/products/tools/api#db
#'
#' @param query Search query string to find relevant series
#' @param geo_rfa Geographic region filter (default: "IUSA" for United States)
#' @param rel_id Relationship ID to filter results (default: "F1A0751C-5283-4EAC-9F5A-2F3114C9AF85")
#' @param start Starting position for pagination (0-indexed)
#' @param rows Number of rows to return (default: 25, max recommended: 100)
#' @param sort Sort criteria (leave blank to sort by relevance score)
#' @param accKey Access Key for Moody's API (defaults to environment variable)
#' @param encKey Encryption Key for Moody's API (defaults to environment variable)
#' @import httr2
#' @importFrom magrittr %>%
#'
#' @return returns list from json. Use function convert_moodys_search() to create
#' a data frame
#' @export
#'
#' @examples
#' # Search for unemployment data
#' unemployment_search <- search_moodys_series("unemployment rate united states")
#' unemployment_df <- convert_moodys_search(unemployment_search)
#'
#' # Search with pagination
#' gdp_search <- search_moodys_series("GDP", start = 0, rows = 50)

search_moodys_series <- function(
  query = NULL,
  geo_rfa = "IUSA",
  ...,
  start = NULL,
  rows = NULL,
  sort = NULL,
  accKey = Sys.getenv("MOODYS_ACC_KEY"),
  encKey = Sys.getenv("MOODYS_ENC_KEY")
) {
  # Check for token in the environment
  check_moodys_token()

  # Validate inputs
  if (missing(query) || is.null(query) || query == "") {
    stop("Search query cannot be empty")
  }

  if (rows > 100 || is.null(rows)) {
    warning(
      "Large row counts may result in slower response times. Consider using pagination."
    )
  }

  # Build query parameters
  params <- list(
    q = paste(
      query,
      paste0("geo_rfa:", geo_rfa),
      ...,
      sep = " AND "
    ),
    start = start,
    rows = rows
  )

  # Add sort parameter if provided
  if (!is.null(sort) && sort != "") {
    params$sort <- sort
  }

  # Set up headers
  headers <- list(
    Accept = "application/json",
    Authorization = paste0("Bearer ", temporary_env$token)
  )

  # Create and execute request
  req <- request("https://api.economy.com/data/v1/search") |>
    req_headers(!!!headers) |>
    req_url_query(!!!params)

  response <- req_perform(req)

  return(response)
}
moodys_search <- search_moodys_series("employment", rows = 500) |>
  convert_moodys_search()

#' Convert Moody's Search Results to Data Frame
#'
#' @param search_response Object from search_moodys_series()
#'
#' @return Returns a dataframe with search results
#' @export
#'
#' @examples
#' unemployment_search <- search_moodys_series("unemployment rate")
#' unemployment_df <- convert_moodys_search(unemployment_search)

convert_moodys_search <- function(search_response) {
  if (!inherits(search_response, "httr2_response")) {
    stop("Input must be an httr2_response object from search_moodys_series()")
  }

  # Parse the JSON response
  response_data <- search_response |>
    httr2::resp_body_json()

  # Check if there are results
  if (
    is.null(response_data$results) ||
      response_data$count == 0
  ) {
    warning("No search results found")
    return(tibble::tibble())
  }

  # Extract search metadata
  total_results <- response_data$count %||% 0
  start_position <- response_data$start %||% 0

  # Convert search results to data frame
  search_df <- response_data$results |>
    tibble::tibble() |>
    rlang::set_names("doc") |>
    tidyr::unnest_wider("doc")

  # Add metadata as attributes
  attr(search_df, "total_results") <- total_results
  attr(search_df, "start_position") <- start_position
  attr(search_df, "returned_results") <- nrow(search_df)

  return(search_df)
}


#' Get Search Result Summary
#'
#' @param search_df Data frame from convert_moodys_search()
#'
#' @return Prints summary of search results
#' @export

summarize_search_results <- function(search_df) {
  total <- attr(search_df, "total_results") %||% "Unknown"
  start <- attr(search_df, "start_position") %||% 0
  returned <- attr(search_df, "returned_results") %||% nrow(search_df)

  cat("Search Results Summary:\n")
  cat("  Total results found:", total, "\n")
  cat("  Starting position:", start, "\n")
  cat("  Results returned:", returned, "\n")

  if (ncol(search_df) > 0) {
    cat("  Available columns:", paste(names(search_df), collapse = ", "), "\n")
  }

  invisible(search_df)
}


#' Search Moody's Series with Pagination Helper
#'
#' This function automatically handles pagination to retrieve all results
#' for a search query. Use with caution for queries with many results.
#'
#' @param query Search query string
#' @param max_results Maximum number of results to retrieve (default: 500)
#' @param rows_per_page Number of rows per API call (default: 100)
#' @param sort Sort criteria
#' @param accKey Access Key for Moody's API
#' @param encKey Encryption Key for Moody's API
#'
#' @return Data frame with all search results
#' @export

search_moodys_series_all <- function(
  query,
  max_results = 500,
  rows_per_page = 100,
  sort = NULL,
  accKey = Sys.getenv("MOODYS_ACC_KEY"),
  encKey = Sys.getenv("MOODYS_ENC_KEY")
) {
  # Initial search to get total count
  initial_search <- search_moodys_series(
    query = query,
    start = 0,
    rows = rows_per_page,
    sort = sort,
    accKey = accKey,
    encKey = encKey
  )

  initial_df <- convert_moodys_search(initial_search)
  total_available <- attr(initial_df, "total_results")

  if (total_available == 0) {
    return(initial_df)
  }

  # Calculate how many more results we need
  results_to_get <- min(max_results, total_available)

  if (results_to_get <= rows_per_page) {
    return(initial_df)
  }

  # Calculate pagination
  remaining_results <- results_to_get - rows_per_page
  additional_calls <- ceiling(remaining_results / rows_per_page)

  cat("Retrieving", results_to_get, "of", total_available, "total results...\n")

  # Collect all results
  all_results <- list(initial_df)

  for (i in 1:additional_calls) {
    start_pos <- i * rows_per_page
    rows_this_call <- min(
      rows_per_page,
      remaining_results - (i - 1) * rows_per_page
    )

    if (rows_this_call <= 0) {
      break
    }

    cat(
      "Fetching results",
      start_pos + 1,
      "to",
      start_pos + rows_this_call,
      "...\n"
    )

    search_response <- search_moodys_series(
      query = query,
      start = start_pos,
      rows = rows_this_call,
      sort = sort,
      accKey = accKey,
      encKey = encKey
    )

    search_df <- convert_moodys_search(search_response)
    all_results[[i + 1]] <- search_df

    # Small delay to be respectful to the API
    Sys.sleep(0.1)
  }

  # Combine all results
  combined_df <- dplyr::bind_rows(all_results)

  # Preserve metadata from the initial search
  attr(combined_df, "total_results") <- total_available
  attr(combined_df, "start_position") <- 0
  attr(combined_df, "returned_results") <- nrow(combined_df)

  return(combined_df)
}
