#' Download species occurrence data from NBN Atlas
#'
#' This function downloads species occurrence data from the NBN Atlas for a specified location.
#'
#' @param lon The longitude of the location.
#' @param lat The latitude of the location.
#' @param radius The radius around the location to search for occurrences (default is 1L).
#'
#' @return A data frame containing the downloaded species occurrence data.
#'
#' @importFrom curl curl_download
#' @import glue
#' @import data.table
#'
#' @examples
#' download_nbn_species(0, 0)
#'
#' @export


download_nbn_data <- function(lon, lat, radius = 1L){

 require(curl)
  require(data.table)

  base_url <- "https://records-ws.nbnatlas.org/occurrences/index/download?reasonTypeId=10&q=*:*&"
  search_url <- glue::glue(base_url, "lat=", {lat}, "&lon=", {lon}, "&radius=", {radius})

  temp <- tempfile()

  tempdir <- tempdir()

  curl::curl_download(search_url, temp)

  data <- unzip(temp, exdir = tempdir)

  out <- data[1] |>
    fread()

  return(out)



}



