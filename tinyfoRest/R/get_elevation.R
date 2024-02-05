#' Get Elevation
#'
#' This function retrieves the elevation at a given longitude and latitude using the Google Maps Elevation API.
#'
#' @param lon The longitude of the location.
#' @param lat The latitude of the location.
#' @param api_key The API key required to make the API call.
#' @return The elevation at the given location.
#' @importFrom needs needs
#' @importFrom glue glue
#' @examples
#' get_elevation(-122.084, 37.422, 'your_api_key')
#' @export


get_elevation <- function(lat, lon,  api_key){

  require(needs)
  require(httr)
  require(jsonlite)
  require(glue)
  require(reticulate)

  base_url <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", lat, ",", lon, "&key=", api_key)

  elevation <- fromJSON(base_url, simplifyVector = TRUE)
  elevation <- elevation$results$elevation
  return(elevation)

}


