#' Extract elevations from text
#'
#' @param text
#'
#' @return list
#' @export
#'
#' @examples get_elevation(text)
get_elevation <- function(text){

  require(tibble)
  require(dplyr)
  require(stringr)
  require(tidyr)

  el_pattern <- "(elevation of|a.s.l|above sea level)?\\s?(\\d{1,})\\s([m]?)\\s(elevation|a.s.l|above sea level)"

  elevations <- str_match_all(text, el_pattern) |>
    enframe()

  return(elevations)

}

