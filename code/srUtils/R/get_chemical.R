#' Extract chemical
#'
#' @param text
#'
#' @return list
#' @export
#'
#' @examples get_chemical(text)
get_chemical <- function(text){

  require(tibble)
  require(dplyr)
  require(stringr)
  require(tidyr)

  chemical <- "N2O|NO2|NO|CO|CO2|[Cc]arbon|nitrog*|.*flux|ghg|greenhouse.*"
  chem_pattern <- str_match_all(text, chemical) |>
    enframe()

  return(chem_pattern)

}
