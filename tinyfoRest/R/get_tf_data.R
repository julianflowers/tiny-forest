
#' Get Tiny Forest Data
#'
#' This function scrapes information about tiny forests from a specified URL and creates a data frame with the scraped information.
#'
#' @return A list containing several objects:
#' \describe{
#'   \item{id_table} A data frame containing information about each tiny forest, including the URL, stub, and tf_id.
#'   \item{tidy_tf} A tidy data frame with cleaned and transformed information about each tiny forest.
#'   \item{tidy_tf_sf} A spatial data frame with longitude and latitude columns, suitable for mapping.
#'   \item{tidy_tf_uk_map} A map showing the locations of the tiny forests in the UK and Ireland.
#' }
#'
#' @import needs
#' @import rvest
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @import data.table
#' @import devtools
#' @import sf
#' @import vegan
#' @import mapview
#' @import ggthemes
#' @import ggmap
#' @import tictoc
#' @importFrom lubridate today
#' @importFrom lubridate dmy
#'
#' @param url The URL to scrape for tiny forest data.
#'
#' @examples
#' \dontrun{
#'   get_tf_data()
#' }
#'
#' @export
#'
#'
get_tf_data <- function(){

    # Load necessary packages
    library(needs)
    needs(rvest, tidyverse, lubridate, tidyfast, data.table, devtools, sf, vegan, mapview, ggthemes, ggmap, tictoc)

    # Install package from GitHub
    devtools::install_github('julianflowers/myScrapers', force = FALSE)

    # Load package from GitHub
    library(myScrapers)

    # Define URL to scrape
    url <- 'https://tinyforest.earthwatch.org.uk/tiny-forest-sites'

    tic()

    ## scrape web addresses for each tiny forest
    links <- get_page_links(url) %>%
        .[grepl('tiny-forest-sites/', .)] |>
        unique() |>
        enframe()

    # Create a new data frame to store scraped information
    tf_df <- links |>
        # Add the URL to the data frame
        mutate(url = paste0('https://tinyforest.earthwatch.org.uk', value),
               # Add a 'stub' column which removes some unnecessary text from the web address
               #stub = str_remove(value, 'tiny-forest-sites?view=artc'),
               # Add a tf_id column to identify each tiny forest
               tf_id = str_extract(str_remove(value, "catid=8"), "\\d{1,}")) |>
        # Remove any NA values from the data frame
        drop_na()

    # Define a function to create a table of information for each tiny forest
    create_tf_table <- function(df, i){

        read_html(tf_df$url[i]) |>
            html_elements('div') |>
            html_text2() |>
            enframe() |>
            unnest('value') |>
            dplyr::filter(str_detect(value, '^Planted') |
                       str_detect(value, 'GPS') |
                       str_detect(value, 'Area') |
                       str_detect(value, '^Species') |
                       str_detect(value, 'Featured')
            ) |>
            mutate(tf_id = tf_df$tf_id[i],
                   metric = case_when(str_detect(value, '^Planted\nBy') ~ 'planted_by',
                                      str_detect(value, '^Species') ~ 'trees',
                                      str_detect(value, '^GPS') ~ 'gps')
            ) |>
            drop_na() |>
            distinct() |>
            group_by(metric) |>
            dplyr::slice(1)
    }

    # Use the `map_dfr` function to apply the `create_tf_table` function
    # to each tiny forest in the data frame and combine the results into a single data frame

    tf_table <- map_dfr(1:nrow(tf_df), \(x) create_tf_table(tf_df, x), .progress = TRUE)

    tidy_tf_table <- tf_table |>
        dplyr::select(-name) |>
        pivot_wider(names_from = 'metric', values_from = 'value', values_fill = 'NA') |>
        mutate(gps = str_remove(gps, '\\\n.*'),
               gps = str_remove(gps, '\\\r.*'),
               gps = str_remove(gps, 'GPS: ')
        )|>
        separate(gps, c('lat', 'lon'), ',\\s?') |>
        mutate_at(c('lat', 'lon'), parse_number) |>
        separate(planted_by, c('pb', 'planted_by', 'plant_date', 'area', 'class_area', 'feature'), ':\\\n') |>
        dplyr::select(-pb) |>
        mutate(area= parse_number(area),
               class_area = parse_number(class_area),
               plant_date = dmy(str_remove(plant_date, 'Planted Area')),
               planted_by = str_remove(planted_by, 'Planting.*'),
               planted_by = str_remove(planted_by, 'Date*'),
               tf_age = lubridate::today() - plant_date,
               trees = str_remove(trees, 'Species Planted in the Forest:')

        ) |>
        dplyr::filter(trees !="NA", !is.na(plant_date), !is.na(lon))

    tidy_tf_table <- tidy_tf_table |>
    mutate(site = as.numeric(tf_id)) |>
    arrange(site) |>
    filter(site !=1)

    sf::sf_use_s2(FALSE)

    tidy_tf_table_sf <- tidy_tf_table |>
        st_as_sf(coords = c('lon', 'lat'), crs = 4326)

    uk <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Countries_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

    uk <- st_union(uk)

    ## filter for UK and Ireland
    uk_tf <- st_intersection(tidy_tf_table_sf, uk)


    map = mapview(uk_tf)

    toc()

    output <- list(id_table = tf_df, tidy_tf = tidy_tf_table, tidy_tf_sf = uk_tf, map = map)

    return(output)
}
