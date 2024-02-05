#' Retrieves BSBI( Botanical Society of Britain and Ireland) data for a given grid reference
#'
#' @param grid_ref A valid UK Ordnance Survey grid reference
#'
#' @return A list containing the records in the BSBI database for the given grid reference
#'
#' @examples
#' get_bsbi_data("NY7250")
#' get_bsbi_data("TQ3080")
#' @export

get_bsbi_data <- function(grid_ref){

  url <- paste0("https://database.bsbi.org/reports/sitetaxa.php?gridref=", grid_ref, "&minfreq=1&minyear=2015&maxyear=2020&sortrecent=1")

  records <- read_html(url) |>
    html_nodes("section") |>
    html_text2() |>
    str_split("\\n")

  out <- list(records = records)

}
