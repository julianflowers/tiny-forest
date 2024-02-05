#' Get Natural Features
#'
#' This function extracts natural features within a given \code{buffer} around a specified \code{tf_id} location.
#'
#' @param df A data.frame containing the location data.
#' @param tf_id An integer specifying the focal point of the analysis.
#' @param buffer A numeric value specifying the desired buffer size in meters.
#'
#' @return A list with two items:
#' \item{d}{A spatial objects data.frame (sf) containing the natural features within the buffer.}
#' \item{b}{A spatial objects data.frame (sf) containing the buffer area around the focal point.}
#'
#' @importFrom sf st_as_sf st_transform st_intersection st_bbox
#' @importFrom osmdata osmdata_sf add_osm_feature opq
#' @importFrom mapview mapview
#'
#' @examples
#' \dontrun{
#' get_water_features(df, tf_id = 25, buffer = 500)
#' }
#' @export

get_osm_natural_features <- function(df, tf_id = 85, buffer = 1000){

  ## get natural water features

  #require(tinyForestR)
  require(sf)
  require(tidyverse)
  library(mapview)
  require(osmdata)

  id <- tf_id
  buffer <- buffer

  ll <- df |>
    select(tf_id, lon, lat) |>
    distinct() |>
    filter(tf_id == id)


  tf_buffer <- ll |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(27700) |>
    st_buffer(buffer)

  tf_bbox <- tf_buffer |>
    st_transform(crs = 4326) |>
    st_bbox()

  wq <- opq(bbox = tf_bbox) %>%
    add_osm_feature(key = "natural")

  wd <- osmdata_sf(wq)

  tf_w <- st_intersection(tf_buffer, wd$osm_polygons |> st_transform(27700))


  # m <-  tf_w |>
  #   mapview() +
  #   mapview(tf_w1) +
  #   mapview(tf_buffer)

  out <- list(d = tf_w, b = tf_buffer)

}




