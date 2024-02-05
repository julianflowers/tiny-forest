#' Get PhyloPic Image
#'
#' This function retrieves an image from PhyloPic API and saves it to a specified directory.
#'
#' @param search A character string containing the name of the organism or scientific name of the taxon.
#' @param path A character string specifying the directory to save the image file.
#' @return The image is saved to the directory specified by the path parameter.
#' @import rphylopic
#' @export
#' @examples
#' get_phylopic_image("Homo sapiens", "C:/images/")
#' get_phylopic_image("Gorilla gorilla", "D:/pictures/")

get_phylopic_image <- function(search, path){

  if(!require(rphylopic))install.packages("rphylopic")
  library(rphylopic)

  img <- pick_phylopic(name = search, n = 1)
  save_phylopic(img = img, path = path)


}

