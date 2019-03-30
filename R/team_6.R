#' Function from Team 6
#' 
#' @param file A path having shape file of selected country.
#' @param tolerance A number for tolerence.
#' @import tidyverse
#' @import purrr
#' @import sf
#' @import maptools
#' @import checkmate
#' @import dplyr
#' @export 
#' @return The dataframe about how to plot the map of a country such as Australia as default
#' examples
#' file<-"gadm36_AUS_shp/gadm36_AUS_1.shp"
#' tolerance <- 0.1
#' team_6(file="gadm36_AUS_1.shp",0.01)
#' team_6(file,0.1) %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon(fill="white",color="black",lwd=1)




team_6 <- function(file, tolerance){
    if(!hasArg(file)){
    file=system.file("gadm36_AUS_shp/gadm36_AUS_1.shp", package="Team5")
    warning('file path does not provided by user, set to default file path, gadm36_AUS_shp/gadm36_AUS_1.shp')
  }
  else if(!file.exists(as.character(file))){
    file=system.file("gadm36_AUS_shp/gadm36_AUS_1.shp", package="Team5")
    warning('file does not exist, set to default file, Australia map')
  }
  # check whether the tolerance is a number 
  if(!is.numeric(tolerance)){
    warning('argument is not numeric or logical: returning NA')
    return(NA)
  }
  # check whether the tolerance is a number between 0 and 1.
  checkmate::assertNumber(tolerance,lower = 0, upper = 1) 
  
  ozbig <- read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)
  mat2df <- function(mat) {
    df <- data.frame(long = mat[, 1],
                     lat  = mat[, 2], 
                     temporary.group = sum(mat[,1]+mat[,2] *nrow(mat) + stats::rnorm(1, 100, 10)), 
                     order= 1:nrow(mat))
    df
  }
  
  
  #add geographic info to the dataframe
  geo_information <- as.data.frame(cbind(oz$GID_0,
                                         oz$NAME_0,
                                         oz$GID_1,
                                         oz$NAME_1,
                                         oz$TYPE_1,
                                         oz$ENGTYPE_1,
                                         oz$CC_1,
                                         oz$HASC_1))
  names(geo_information) <- c("GID_0","NAME_0","GID_1","NAME_1","TYPE_1","ENGTYPE_1","CC_1","HASC_1")
  add_information <- geo_information[rep(seq_len(nrow(geo_information )), 
                                         times=unlist(lapply(oz$geometry, length))),] 
  add_information %>% mutate(group = as.character(seq(nrow(add_information))))->add_information
  
  
  
  # flatten oz$geometry twice to obtain a list of matrices
  # this is because we have two layers of lists ahead of matrices
  oz.geometry.flattened <- oz$geometry %>% flatten() %>% flatten()
  # Create a dataframe ozplus from the geometry variable using purrr
  # map_df applies a function to elements of a list and bind the dataframes together
  # Instead of temporary.group variable by the function mat2df, 
  # group variable consisting of the index corresponding each matrix will be used.
  ozplus <- oz.geometry.flattened %>% purrr::map_df(.id ="group", .f = mat2df)
  
 
   
  #join geo info to the map info
  ozplus <- full_join(ozplus,add_information,by='group')
  checkmate::testDataFrame(ozplus)# check whether the output is a dataframe.
  
 return (ozplus)
  
}

# plot the result
#team_6(file,0.1) %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon(fill="white",color="black",lwd=1)

