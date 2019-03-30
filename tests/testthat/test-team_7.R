context("test-team_7")

test_that("test for team_7", {
  #test the file path
  expect_error(team_7("m",0.01))
  #test the tolerance
  expect_error(team_7("gadm36_AUS_1.shp",-0.5))
  #test the result data frame
  expect_s3_class(team_7(system.file("gadm36_AUS_shp/gadm36_AUS_0.shp", package="Team5"),0.01),"data.frame")
})
