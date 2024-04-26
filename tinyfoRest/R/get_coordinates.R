## function to extract location coordinates

#' Get lat-longs
#'
#' @param text
#'
#' @return matrix
#' @export
#'
#' @examples
#' get_coordinates(text$text)
#'


get_coordinates <- function(text){

  require(tibble)
  require(dplyr)
  require(stringr)
  require(tidyr)

  colon <- "([NnSsEeWw])(\\d{1,2}):(\\d{1,2}):(\\d{1,2})"
  decimal <- paste0("(\\d{1,3})", "\\.", "(\\d{1,2})", ".?",  "(", stringi::stri_unescape_unicode('\\u00b0'), "|", stringi::stri_unescape_unicode('\\u25e6'), ")", ".?", "([NnSsEeWw])")
  polar_lat <- paste0("(\\d{1,3}.?)", "(", stringi::stri_unescape_unicode('\\u00b0'), "|", stringi::stri_unescape_unicode('\\u25e6'), ")", "\\s?", "(\\d{1,3})", "\\D?", "([WENnSs])")
  polar_lat_1 <- "(\\d{1,2})(\\d{1,2}).*([WENS])"
  e_n <- "(\\d{4,6})\\D?([NnSs]).?(\\d{4,6})\\D?([EeWw])"
  misc <- ".?(\\d{1,2})(u)(\\d{1,3}).?([NSEW])"
  simple <- paste0("(\\d{1,3}\\s?)", "(", stringi::stri_unescape_unicode('\\u00b0'), "|", stringi::stri_unescape_unicode('\\u25e6'), ")", "([WENnSs])")
  pattern <- "(\\d{1,3}°)\\s?(\\d{1,2}′)\\s?(\\d{1,2}″)\\s?([NS]),?\\s?(\\d{1,3}°)\\s?(\\d{1,2}′)\\s?(\\d{1,2}″)\\s?([EW])"


  colon_pattern <- str_match_all(text, colon) |>
    tibble::enframe()

  decimal_pattern <- str_match_all(text, decimal) |>
    tibble::enframe()

  polar_lat_pattern <- str_match_all(text, polar_lat) |>
    tibble::enframe()

  polar_lat_1_pattern <- str_match_all(text, polar_lat_1) |>
    tibble::enframe()

  # polar_long_pattern <- str_match_all(text, polar_long) |>
  #   tibble::enframe()

  e_n_pattern <- str_match_all(text, e_n) |>
    tibble::enframe()

  misc_pattern <- str_match_all(text, misc) |>
    tibble::enframe()

  simple_pattern <- str_match_all(text, simple) |>
    tibble::enframe()


  out <- data.frame(c = colon_pattern, d = decimal_pattern, lat = polar_lat_pattern, polar_lat_1_pattern,
                           en = e_n_pattern, misc = misc_pattern, simple = simple_pattern)
  out
}


