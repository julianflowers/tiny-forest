#' Get S2 Landcover
#'
#' Function to retrieve Sentinel-2 landcover data for a specified location and time period.
#'
#' @param tf_id Numeric ID associated with the training forest \code{default = tf_id}.
#' @param lon Numeric, longitude coordinate of the location \code{\link{default} = -0.250830}.
#' @param lat Numeric, latitude coordinate of the location \code{\link{default} = 52.5739214}.
#' @param dist Numeric, buffer distance in meters around the location \code{\link{default} = 1000}.
#' @param start_date Character string, start date of the time period in 'YYYY-MM-DD' format \code{\link{default} = '2023-01-01'}.
#' @param end_date Character string, end date of the time period in 'YYYY-MM-DD' format \code{\link{default} = '2023-09-01'}.
#'
#' @import reticulate
#' @import rgee
#' @import tidyrgee
#' @importFrom zoo zoo
#' @import stars
#' @import terra
#' @import tidyverse
#'
#' @importFrom reticulate import
#'
#' @examples
#' ## Specify the tf_id
#' tf_id <- 85
#'
#' ## Specify the location coordinates
#' lon <- -0.250830
#' lat <- 52.5739214
#'
#' ## Specify the buffer distance
#' dist <- 1000
#'
#' ## Specify the time period
#' start_date <- '2023-01-01'
#' end_date <- '2023-09-01'
#'
#' ## Call the get_s2_landcover function
#' get_s2_landcover(tf_id = tf_id, lon = lon, lat = lat, dist = dist)
#' @export

get_s2_landcover <- function(tf_id = 85, lon = -0.250830, lat = 52.5739214, dist = 1000, start_date = "2023-01-01", end_date = "2023-09-01"){

  require(reticulate); require(rgee); require(tidyrgee)
  require(stars)
  require(terra)
  #require(tinyForestR)
  require(tidyverse)

  ee <- import("ee")
  geemap <- import("geemap")
  geedim <- import("geedim")

  #ee_Authenticate()
  ee_Initialize(drive = TRUE)

  ## load image collection


  ic <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

  ## parameters

  lon <- lon
  lat <- lat
  tf_id <- tf_id

  point <- ee$Geometry$Point(c(lon, lat))$buffer(100)
  roi <- ee$Geometry$Point(c(lon, lat))$buffer(dist)
  start <- start_date
  end <- end_date

  dw <- ic$filterBounds(roi)
  dw <- dw$filterDate(start, end)
  dw <- dw$filter(ee$Filter$lte("CLOUDY_PIXEL_PERCENTAGE", 10))
  dw_im <- dw$mean()

  tidy_dw <- tidyrgee::as_tidyee(dw)

  tmpd <- tempdir()

  geemap$ee_export_image(ee_object = dw_im, filename = paste0(tmpd, "/dw.tif"), scale = 10, crs = "EPSG:4326", region = roi)

  r <- fs::dir_ls(tmpd, regexp = ".tif")

  r_s <- raster::stack(r)

  plot <- plotRGB(r_s, r=4, g=3, b=2, stretch = "lin", axes = TRUE, main = "DW")


  out <- list(image_dates = tidy_dw$vrt, tf_id = tf_id, buffer = dist, image = dw_im, raster = r_s, plot = plot)


}




