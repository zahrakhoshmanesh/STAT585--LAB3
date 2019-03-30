context("test-team_5")

test_that("multiplication works", {
  expect_warning(Team_5(tolerance = 'a'),"argument is not numeric or logical: returning NA")
  expect_equal(is.data.frame(Team_5(tolerance = 0.1)),TRUE) 
  expect_named(Team_5(tolerance = 0.1),
               c("group","long","lat","order","GID_0","NAME_0","GID_1","NAME_1","TYPE_1","ENGTYPE_1","CC_1" , "HASC_1") )
})
