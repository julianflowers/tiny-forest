#' Get air temperature
#'
#' @param text
#'
#' @return character
#' @export
#'
#' @examples get_air_temperature(text)
#'

get_air_temperature <- function(text){

  require(tibble)
  require(dplyr)
  require(stringr)
  require(tidyr)

  at_pattern <- "(\\d{1,}\\.?\\d{1,2}?)\\D*(\\u00b0|\\u25e6|degrees)(.?[C])\\D*"
  at_pattern <- str_match_all(text, at_pattern) |>
    enframe()

}
