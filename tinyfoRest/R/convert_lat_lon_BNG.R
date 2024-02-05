#' Convert Latitude and Longitude to Ordnance Survey Grid Reference
#'
#' This function uses the 'OSGridConverter' Python package to convert latitude and
#' longitude coordinates to Ordnance Survey grid references.
#'
#' @param lon numeric vector of longitude coordinates
#' @param lat numeric vector of latitude coordinates
#' @return A list with a string containing the Ordnance Survey grid reference for
#' the lat-lon coordinate pair.
#' @import reticulate
#' @import stringr
#' @examples
#'
#' os_lat_lon_to_grid(-2.353, 51.509)
#'
#' @seealso
#'
#' \code{\link{os_grid_to_lat_lon}}
#'
#' @export


os_lat_lon_to_grid <- function(lat, lon){

  require(reticulate); require(tidyverse)

  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/tinyforest/bin/python")
  use_virtualenv("tinyforest")

  py_install("OSGridConverter", pip = TRUE, envname = "tinyforest")

  osgrid <- import("OSGridConverter")

  grid <- osgrid$latlong2grid(lat, lon)
  r <- py_to_r(grid) |>
    as.character()


  r <- paste0(stringr::str_sub(r[1], 1, 2), stringr::str_sub(r[1], 4,7))
  out <- list(grid = r)

}

