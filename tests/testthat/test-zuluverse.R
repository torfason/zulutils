
test_that("na_if_in works", {
  input <- c(1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1)
  na_values <- 6:10
  output <- c(1,2,3,4,5,NA,NA,NA,NA,NA,NA,NA,5,4,3,2,1)
  expect_equal(na_if_in(input,na_values),output)
})
