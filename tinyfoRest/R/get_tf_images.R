#' Save ggmap Images
#'
#' This function saves a ggmap satellite image given a longitude and latitude.
#'
#' @param lon longitude of the center of the image
#' @param lat latitude of the center of the image
#' @param key Google API key for ggmap imagery
#' @param zoom degree of zoom of image; default = 15
#' @param tf_id earthwatch id of Tiny Forest
#'
#' @import ggmap
#' @import tidyverse
#'
#' @examples
#' get_ggmap_images(-71.0636, 42.3581, "API_key", zoom=19)
#'
#' @export



get_tf_images <- function(lon, lat, key, zoom = 15, tf_id){

  Sys.setenv(GGMAP_GOOGLE_API_KEY = key)
  library(needs)
  needs(ggmap, tidyverse)
  ggmap::register_google(key)
  ggmap::get_googlemap(center = c(lon = lon, lat = lat),
                       zoom = zoom, maptype = "satellite", messaging = TRUE) |>
  ggmap() -> p


  ggsave(paste0(here::here(), "/images/tf_", tf_id,"_", lon, "-", lat, "-", Sys.Date(), "zoom=", zoom,  ".png"), p)

}


