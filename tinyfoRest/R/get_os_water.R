#' os_ngd_api_call
#'
#' Calls the Ordnance Survey OpenSpace National Geographic Database API and returns a list containing the results and the API call URL.
#'
#' @param bbox The bounding box as a spatial object or a numeric vector with xmin, ymin, xmax, ymax coordinates.
#' @param key The API key for the Ordnance Survey Data Hub.
#' @param offset The offset value for pagination.
#' @return A list with two elements:
#' \itemize{
#'   \item \code{os_lc}: The result of the API call as a spatial object of class \code{sf}.
#'   \item \code{url}: The API call URL used.
#' }
#' @import tidyverse
#' @import reticulate
#' @import mapview
#' @import sf
#' @import tictoc
#' @import leaflet
#' @examples
#' bbox <- c(-2.16, 53.9, -2.13, 53.92)
#' key <- Sys.getenv('OS_DATA_HUB')
#' os_ngd_api_call(bbox, key, 0)
#'
#' @export

os_ngd_api_call_water <- function(bbox, api_key, offset){


  require(needs)

  needs(tidyverse, reticulate, mapview, sf, tictoc, colourvalues, leaflet, colorspace, geojsonsf)

  key = api_key
  base_url <- "https://api.os.uk/features/ngd/ofa/v1/collections/wtr-fts-water-1/items"
  ## import modules

    bbox <- st_bbox(bbox) |>
      paste0(collapse = ",")
    api_call <- glue::glue(base_url, "?bbox=", bbox, "&key=", key, "&limit=100&offset=", offset)
    os_lc <- geojsonsf::geojson_sf(api_call)
    out <- list(os_lc = os_lc, url = api_call)


}


