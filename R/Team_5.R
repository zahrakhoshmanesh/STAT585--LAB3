#' Function from Team 5
#'
#'@param file is the location of the data
#'@param tolerance is the value used for thinning the polygon
#'@export 
#'@import ggplot2 
#'@import methods
#'@import dplyr
#'@return ozplus is a data frame about how to plot the map of Australia
#'
Team_5 <- function(file,tolerance=0.1){
  if(!hasArg(file)){
    file=system.file("gadm36_AUS_shp/gadm36_AUS_1.shp", package="Team5")
  }
  else if(!file.exists(as.character(file))){
    file=system.file("gadm36_AUS_shp/gadm36_AUS_1.shp", package="Team5")
  }
  if(!is.numeric(tolerance)){
    warning('argument is not numeric or logical: returning NA')
    return(NA)
  }
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance , minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  Mat2Df <- function(Mat){
    Mat<- Mat[[1]]
    long <- Mat[,1]
    lat <- Mat[,2]
    order <- 1:nrow(Mat)
    #group <- rep(rnorm(1),nrow(Mat))
    df <- data.frame(long=long,lat=lat,order=order)
    df
  }
  oz_flatten <- purrr::flatten(oz$geometry)
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
                          
  ozplus <- purrr::map_df(.x = oz_flatten,
                     .f = Mat2Df, 
                     .id = "group") 
  
  ozplus <- full_join(ozplus,add_information,by='group')
  
  return(ozplus)
}
