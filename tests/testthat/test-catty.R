
test_that("catty construction and printing works", {

  # Construct a catty vector
  x <- catty(letters)

  # Validating x should work without error, and printing should work correctly
  expect_equal(validate.zmisc_catty(x),x)
  expect_output(print(x), "a b c d e f g h i j k l m n o p q r s t u v w x y z")

  # Construct a catty vector that separates using newline
  y <- catty(letters[1:5], sep="\n")
  expect_equal(validate.zmisc_catty(y),y)
  expect_output(print(y),
"a
b
c
d
e" )

})
