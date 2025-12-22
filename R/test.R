library(sdoecon)

query <- "employment&geo_rfa=IUSA&geotitle=United"

response <- search_moodys_series(query)

res_df <- convert_moodys_search(response)
search_response <- response
geo_title_exact <- "United States"
rows <- 500
params <- list(
  q = query,
  geo_rfa = "IUSA"
)

geo_title_exact = "United States"
  headers <- list(
  Accept = "application/json",
  `Content-Type` = "application/json",
  Authorization = paste0("Bearer ", temporary_env$token)
)
  # Create and execute request
  req <- request("https://api.economy.com/data/v1/search") |>
    req_headers(!!!headers) |>
    req_url_query(!!!params)


  response <- req_perform(req)

  response_data <- response |>
    httr2::resp_body_json()
