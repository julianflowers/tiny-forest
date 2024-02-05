#' Converts Ordnance Survey grid reference to latitude and longitude
#'
#' This function converts an Ordnance Survey grid reference to latitude and longitude.
#' It requires the reticulate and tidyverse packages to be installed,
#' as well as the OSGridConverter python package.
#' @param grid A character vector of an Ordnance Survey grid reference (e.g. 'TL4871')
#' @return A list with the longitude and latitude in decimal degrees.
#' @import reticulate
#' @import tidyverse
#'
#' @export



os_grid_to_lat_lon <- function(grid){

  # load the reticulate and tidyverse packages
  require(reticulate); require(tidyverse)

  # # set the virtual environment path
  # Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/tinyforest/bin/python")

  # activate the virtual environment
  use_virtualenv("tinyforest")

  # import the OSGridConverter package
  py_install("OSGridConverter", pip = TRUE, envname = "tinyforest")

  # import the OSGridConverter package
  osgrid <- import("OSGridConverter")

  # convert the grid reference to latitude and longitude using the OS Grid Converter package
  ll <- osgrid$grid2latlong(grid)

  # convert the output from Python to R format
  ll <- py_to_r(ll) |>
    as.character()

  # split the output by ':'
  ll <- ll |>
    str_split(":")

  # extract and parse the latitude and longitude values
  lon <- parse_guess(ll[[1]][1])

  lat <- parse_guess(ll[[1]][2])

  # create a list containing the extracted latitude and longitude values
  out <- list(lon = lon, lat = lat)

}
