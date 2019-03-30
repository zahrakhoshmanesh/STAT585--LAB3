#' Solution of team 7
#' 
#' @param file A path of the file
#' @param tolerance A number
#' @import tidyverse
#' @import purrr
#' @import sf
#' @import maptools
#' @import checkmate
#' @import dplyr
#' @export 
#' @return A dataframe of the geographic information of the polygons
#' examples
#' team_7(file="gadm36_AUS_1.shp",0.01)


team_7 <- function(file,tolerance){
  checkmate::assertFile(file, access = "r") # check whether the file path is correct, and whether the file is readeable
  checkmate::assertNumber(tolerance,lower = 0, upper = 1) # check whether the tolerance is a number between 0 and 1.
  ozbig <- read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)
  f <- function(dframe){
    dframe <- data.frame(order = c(1:nrow(dframe)), long = dframe$x, lat = dframe$y)
  }
  ## Here our depth is 3, but that could change depending on the file
  ozplus <- oz$geometry %>%
    modify_depth(3,data.frame) %>%
    modify_depth(3,f) %>%
    flatten() %>%
    flatten() %>%
    dplyr::bind_rows(.id = "group")
  checkmate::testDataFrame(ozplus)
  return(ozplus)
}
