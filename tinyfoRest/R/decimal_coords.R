

#' Calculate decimal coordinates from degrees/minutes
#' @param degrees
#' @param minutes
#' @param point
#'
#' @return data frame
#' @export
#'
#' @examples decimal_from_deg_min(degrees, minutes, point)
#'
decimal_from_deg_min <- function(degrees, minutes, point){

  # if(point == "[sS]"|point == "[wW]")

  decimal <- degrees + minutes / 60



  return(decimal)

}

