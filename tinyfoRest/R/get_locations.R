#' Get locations
#'
#' Extracts locations from text. Uses spaCy which is a state of the art natural language processing tools and returns a data frame
#'
#'
#'
#' @param text
#'
#' @return data frame
#' @export
#'
#' @examples get_locations(text)
#'
get_locations <- function(text){

  require(reticulate)
  require(spacyr)
  require(dplyr)
  #model <- spacyr::spacy_download_langmodel(model = "en_core_web_md")
  #spacyr::spacy_initialize(model = model, condaenv = "spacy_condaenv", python_executable = "/Users/julianflowers/Library/r-miniconda-arm64/envs/spacy_condaenv/bin/python")

  x <- spacyr::spacy_parse(text, entity = TRUE)
  x <- spacyr::entity_consolidate(x)

  locs <- x |>
    dplyr::filter(stringr::str_detect(pos, "ENTITY")) |>
    dplyr::filter(entity_type %in% c("GPE", "LOC"))

  return(locs)



}

