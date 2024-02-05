#' Get land cover data from the Dynamic World Land Cover dataset for a given location
#'
#' Downloads the image collection from the Dynamic World Land Cover (DW) dataset on Google Earth Engine (GEE) platform, filters it using a given location and time frame, and returns the land cover statistics for the given area.
#'
#' @param tf_id text: specify which Tiny Forest site ID to use, defaults to tf_id
#' @param lon numeric: longitude of the center of the area of interest, defaults to 0.0969
#' @param lat numeric: latitude of the center of the area of interest, defaults to 51.578
#' @param dist numeric: distance around the center point to create a circular buffer in meters, defaults to 500
#' @param start_date character: starting date of the image collection, defaults to '2022-01-01'
#' @param end_date character: ending date of the image collection, defaults to '2022-12-31'
#' @return list: a list containing the following items:
#' \item{map}{a map object showing the image collection and area of interest}
#' \item{image_dates}{vector of image dates in the collection}
#' \item{buffer}{the buffer distance used to select the area of interest}
#'
#' @import reticulate
#' @import rgee
#' @import tidyrgee
#' @import stars
#' @import tidyrgee
#' @importFrom tidyr drop_na
#' @importFrom terra as.data.frame
#' @importFrom graphics plot

#' @examples
#' \dontrun{
#' # Get data for the default location
#' get_dw_landcover()
#'
#' # Get data for a specific location and buffer distance
#' get_dw_landcover(tf_id= "85", lon = -1.472293, lat = 51.78398, dist = 1000)
#' }
#'
#' @export


get_dw_landcover <- function(tf_id = tf_id, lon = -0.250830, lat = 52.5739214, dist = 1000, start_date = "2023-01-01", end_date = "2023-08-01"){

  require(reticulate); require(rgee); require(tidyrgee); if(!require(zoo))install.packages("zoo")
  library(zoo)
  require(stars)
  require(terra)
  ##require(tinyForestR)

  ee <- import("ee")
  geemap <- import("geemap")
  geedim <- import("geedim")

  #ee_Authenticate()
  #ee_Initialize(drive = TRUE)

  ## load image collection


  ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")

  ## parameters

  lon <- lon
  lat <- lat
  tf_id <- tf_id

  point <- ee$Geometry$Point(c(lon, lat))$buffer(100)
  roi <- ee$Geometry$Point(c(lon, lat))$buffer(dist)
  start <- start_date
  end <- end_date

  pal <- c(
    '#419bdf', '#397d49', '#88b053', '#7a87c6', '#e49653', '#dfc35a','#c42811',
    '#a59b8f', '#b39fe1')


  dw <- ic$filterBounds(roi)
  dw <- dw$filterDate(start, end)
  dw <- dw$select("label")
  dw_im <- dw$mode()


  tidy_dw <- tidyrgee::as_tidyee(dw)

  tmpd <- tempdir()

  geemap$ee_export_image(ee_object = dw_im, filename = paste0(tmpd, "/dw.tif"), scale = 10, crs = "EPSG:4326", region = roi)

  r <- fs::dir_ls(tmpd, regexp = ".tif")

  r_s <- stars::read_stars(r[1], along = "band")


  out <- list(image_dates = tidy_dw$vrt, tf_id = tf_id, buffer = dist, image = dw_im, raster = r_s, palette = pal)


}




