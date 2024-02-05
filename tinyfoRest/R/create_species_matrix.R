#' Create a species matrix from a dataframe
#'
#' This function takes an input dataframe, with species and tf_id columns, and creates a matrix
#' with species as the rows and tf_id as the columns. Entries in the matrix are counts of
#' the number of times each species occurs at each site (as represented by tf_id).
#'
#' @param df a dataframe with columns 'tf_id', 'species', and 'classs'
#' @param class a string indicating the ecological class of interest
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @return a list with two elements: sp, a species-by-tf_id matrix with species counts; sites,
#' a vector of unique tf_id values indicating the sites represented in the matrix.
#'
#' @examples
#' data(iris)
#' iris_species <- iris %>% select(Species) %>% mutate(classs = 'example') %>% rename(species = Species, tf_id = classs)
#' create_species_matrix(iris_species, 'example')
#'
#' @export

create_species_matrix <- function(df, class = NULL){

  require(janitor); require(dplyr); require(tidyr)

  counts <- df |>
    select(tf_id, species, classs) |>
    filter(classs == class) |>
    group_by(tf_id) |>
    count(species)


  df_matrix <- counts |>
    pivot_wider(names_from = "species", values_from = "n", values_fill = 0) |>
    clean_names()

  species_matrix  <- df_matrix[, -1]
  sites <- df_matrix[, 1]

  out <- list(sp = species_matrix, sites = sites)
  return(out)

}

