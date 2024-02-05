#' Retrieves occurrence data from NBN Atlas within a specified buffer around given longitude and latitude
#'
#' @param lon numeric Longitude in decimal degrees
#' @param lat numeric Latitude in decimal degrees
#' @param radius numeric Buffer radius in kilometers, default is 1 km
#' @param n numeric Maximum number of occurrences to retrieve, default is 10,000
#'
#' @return data frame containing occurrence information within the buffer
#'
#' @importFrom dplyr select
#' @importFrom jsonlite fromJSON
#' @importFrom tictoc tic toc
#'
#' @examples
#' get_nbn_buffer(-1.5, 52.0, 2)
#'
#' @export


get_nbn_buffer <- function(lon, lat, radius = 1, n = 10000, tf_id){

  require(dplyr) # Load the dplyr package for data manipulation.
  require(jsonlite) # Load the jsonlite package for working with JSON data.
  require(tictoc) # Load the tictoc package to measure elapsed time.
  require(data.table)

  tic()  # Start a timer to measure how long the code takes to execute.


  base_url <- "https://records-ws.nbnatlas.org/occurrences/search?q=*:*&" # Set the base URL for the NBN Atlas web service.
  search <- paste0(base_url, "lat=", lat, "&lon=", lon, "&radius=", radius, "&pageSize=", n) # Concatenate the base URL with the values provided by the user to build the full URL for the API request
  df <- fromJSON(search, simplifyDataFrame = TRUE)

  toc() # Stop the timer and print the elapsed time to the console.

  df$occurrences |>
    dplyr::mutate(site = tf_id) |> 
  as.data.table()
  


}
