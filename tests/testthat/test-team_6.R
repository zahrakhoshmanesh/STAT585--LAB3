context("test-team_6")

test_that("test for team 6 :creating map works", {
  
  #test returning warning if file path not provided by user and set to default path
  expect_warning(team_6(tolerance = 'c'),"file path does not provided by user, set to default file path, gadm36_AUS_shp/gadm36_AUS_1.shp")
  
  #test returning warning if file path does not exist and set to default path
  expect_warning(team_6(file="s",tolerance = 'c'),"file does not exist, set to default file, Australia map")
  
  #test the tolerance , returning warning if is not numeric and greater than 1 and less than 0
  expect_warning(team_6(system.file("gadm36_AUS_shp/gadm36_AUS_0.shp", package="Team5"),tolerance = 'c'),"argument is not numeric or logical: returning NA")
  expect_error(team_6(system.file("gadm36_AUS_shp/gadm36_AUS_0.shp", package="Team5"),3))
  
  #test the result is data frame
  expect_warning(is.data.frame(team_6(tolerance = 0.1)),"file path does not provided by user, set to default file path, gadm36_AUS_shp/gadm36_AUS_1.shp")
 
  #expect_named(team_6(tolerance = 0.1),c("group","long","lat","temporary.group","order","GID_0","NAME_0","GID_1","NAME_1","TYPE_1","ENGTYPE_1","CC_1","HASC_1") )

})
