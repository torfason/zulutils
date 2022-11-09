
test_that("lappend() works", {

  expected <- list(cars, mtcars, iris)

  object <- list() |>
    lappend(cars) |>
    lappend(mtcars) |>
    lappend(iris)
  expect_equal(object, expected)

  object <- list(cars) |>
    lappend(mtcars, iris)
  expect_equal(object, expected)

  object <- list(cars, mtcars) |>
    lappend(iris)
  expect_equal(object, expected)

  # The way c() fails
  object <- list(cars, mtcars) |>
    c(iris)
  expect_false(identical(object, expected))


})
