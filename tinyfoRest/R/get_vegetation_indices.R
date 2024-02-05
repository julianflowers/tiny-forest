
#' Get Vegetation Indices
#'
#' This function retrieves vegetation indices from Sentinel-2 satellite imagery within a specified region of interest.
#'
#' @param tf_id The ID of the Tiny Forest in the tinyForestR package.
#' @param buffer The buffer distance in meters around the region of interest. Default is 1000 meters.
#' @param lat The latitude of the center of the region of interest.
#' @param lon The longitude of the center of the region of interest.
#' @param start The start date of the image collection. Default is '2023-01-01'.
#' @param end The end date of the image collection. Default is '2023-08-01'.
#'
#' @importFrom tidyrgee as_tidyee
#' @export



get_vegetation_indices <- function(tf_id, buffer = 1000, lat, lon, start = "2023-01-01", end = "2023-08-01"){

  library(rgee)
  library(tidyrgee)
  library(tidyverse)
  require(reticulate)
  require(stars)
  require(terra)
  require(tidyterra)

   eemont <- import("eemont")
   ee <- import("ee")
   geemap <- import("geemap")

   ee$Initialize()

   # lon <- ll$X[1]
   # lat <- ll$Y[1]


   roi = ee$Geometry$Point(c(lon, lat))$buffer(buffer) # Extended constructor


   S2 <- ee$ImageCollection('COPERNICUS/S2_SR_HARMONIZED')$
     filterBounds(roi)$
     filterDate(start, end)$ # Extended (pre-processing)
     filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 10))$ # Extended (pre-processing)
     scaleAndOffset()$ # Extended (pre-processing)
     spectralIndices(c('NDVI', 'GBNDVI', 'EVI2', 'GLI', 'MNDVI', 'NDBI'))$
     select(c('NDVI', 'GBNDVI', 'EVI2', 'GLI', 'MNDVI', 'NDBI'))

   S2_median <- S2$
     median() # Extended (processing)

   S2dates <- tidyrgee::as_tidyee(S2)

   tmpd <- tempdir()

   geemap$ee_export_image(ee_object = S2_median, filename = paste0(tmpd, "/s2.tif"), scale = 10, crs = "EPSG:4326", region = roi)

   r <- fs::dir_ls(tmpd, regexp = "s2.tif")

   terra_s <- rast(r[1])

   r_s <- stars::read_stars(r[1], along = "band")

   rs_sf <- r_s |>
     st_as_sf() |>
     rename(NDVI = 1, GBNDVI = 2, EVI2 = 3, GLI = 4, MNDVI = 5, NDBI = 6)

   plot_r_s <- plot(r_s, col = viridis::viridis(20))

   boxplot <- rs_sf |>
     pivot_longer(names_to = "VI", values_to = "vals", cols = 1:6) |>
     ggplot(aes(vals, VI)) +
     geom_boxplot() +
     labs(title = paste0("Variation in vegatation indices in TF", tf_id))

   out <- list(plot = plot_r_s, boxplot = boxplot, raster = r_s, sf = rs_sf, images = S2dates, tf_id = tf_id, terra = terra_s)

}

