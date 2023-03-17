

test_that("sample_strings work", {

  # General format is correct
  x <- sample_strings(17, 19)
  expect_equal(length(x), 19)
  expect_equal(sapply(x, nchar) |> unname() |> zingle(), 17)

  # Defaults are correct
  x <- sample_strings()
  expect_equal(length(x), 1)
  expect_equal(sapply(x, nchar) |> unname() |> zingle(), 3)

  # Named parameters, and upper
  x <- sample_strings(upper=TRUE, size=5, nchar=7)
  expect_equal(x, toupper(x))
  expect_equal(length(x), 5)
  expect_equal(sapply(x, nchar) |> unname() |> zingle(), 7)


  # Test predictablility, in single call or repeated calls
  expected <- c("xyh", "vqn", "tdr", "sls", "ygm")
  set.seed(42)
  x <- sample_strings(size=5)
  expect_equal(x, expected)

  set.seed(42)
  for (i in 1:5) {
    expect_equal(sample_strings(), expected[i])
  }

})
