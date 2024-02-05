#' calculate seasonal bd metrics
#'
#' This function calculates various biodiversity metrics (richness and diversity) for a given tiny forest and class
#'
#' @param tfid The tiny forest id for which the metrics need to be calculated
#' @param class The class for which the metrics need to be calculated
#'
#' @return A list containing the following:
#'
#' metrics  : data.frame containing monthly values of the metrics
#' matrix   : data.frame containing monthly count of species for each site, in wide format
#' plot     : a ggplot object showing monthly variation in richness
#'
#' @import data.table
#' @import vegan
#' @import tidyverse
#' @import janitor
#'
#' @examples
#' calc_bd_metrics(85, 'Aves')
#'
#' @export


calc_bd_metrics <- function(df, class){

   library(data.table)
   library(vegan)
   library(tidyverse)


  tf_bd <- df |>
    setDT()

  # tf_bd[, tf_id := parse_number(tf)]
  # tf_bd[tf_id == tfid, ]
  tf_bd <- tf_bd[between(year, 2015, 2023) & classs == class,]

  species <- tf_bd[, .N, by = .(species, month)] |>
    pivot_wider(names_from = "species", values_from = "N", values_fill = 0) |>
    janitor::clean_names() |>
    arrange(month)


## total monthly observations

  tf_bd_tot <- tf_bd[between(year, 2010, 2023),]
  obs_m <- tf_bd_tot[, .N, by = .(month)][order(month)]


## richness
  spec_n <- specnumber(species[-1])
  month <- species[1]
  spec_n_m <- data.frame(month, richness = spec_n) |>
  arrange(month) |>
  bind_cols(total_observations = obs_m) |>
  mutate(ratio = spec_n / N)

## richness plot

  plot <- spec_n_m |>
    ggplot(aes(factor(month...1), spec_n)) +
    geom_col()

## diversity

  div <- diversity(species[-1], "simpson")

  div <- spec_n_m |>
    bind_cols(diversity = div) |>
    rename(month = month...1) |>
    select(-month...3)

## bray

  bray <- vegdist(species[-1], "bray")


## raup

  raup <- vegdist(species[-1], "raup")



  out <- list(metrics = div, matrix = species, plot = plot, bray = bray, raup = raup)

}













