# Tiny helper function for calculating fractional seats,
# useful in preparing the data, especially creating ties.
seats_fractional <- function(votes, seats=7) {
  seats*votes/sum(votes)
}


#### Test dhont() ####
test_that("dhondt works", {

  # Test vectors (initially created using electoral package)
  votes_2 <-  c(100, 150, 60, 80, 160)
  seats_2_named <- c(V = 3L, W = 4L, X = 1L, Y = 2L, Z = 5L)
  votes_2_named <-  c(100, 150, 60, 80, 160)
  names(votes_2_named) <- names(seats_2_named)

  votes_3_named <- c(DEM=490205, PMDB=1151547, PRB=2449440,
                                        PSB=48274, PSTU=54403, PTC=173151)
  seats_3_named <- c(DEM = 2L, PMDB = 5L, PRB = 12L, PSB = 0L, PSTU = 0L, PTC = 0L)

  set.seed(42)
  votes_4 <- sample(10000, 5)
  seats_4 <- c(0, 1, 3, 0, 3)

  # Test basic case with and without names
  expect_equal(dhondt(votes_2, sum(seats_2_named)), unname(seats_2_named))
  expect_equal(dhondt(votes_2_named, sum(seats_2_named)), seats_2_named)

  # A couple of other values
  expect_equal(dhondt(votes_3_named, sum(seats_3_named)), seats_3_named)
  expect_equal(dhondt(votes_4), seats_4) # Rely on the default 7

  # The function needs to handle the allocation of zero votes or zero seats
  # (zero parties are not allowed)
  expect_equal(dhondt(c(1,0,0)), c(7,0,0))
  expect_equal(dhondt(c(1,0,0), seats=0), c(0,0,0))
})

test_that("dhondt tie handling works", {

  # Test tie handling
  votes_tie_1    <- c(100, 150, 60)
  seats_tie_1_4s <- c(   1,  2,  1) # First four seats, tie between 5 and 6
  seats_tie_1_6s <- c(   2,  3,  1) # First six seats, tie between 5 and 6
  expect_equal(dhondt(votes_tie_1, 4), seats_tie_1_4s)
  expect_equal(dhondt(votes_tie_1, 6), seats_tie_1_6s)

  # Error on four - unless we specify first
  expect_error(dhondt(votes_tie_1, 5), regexp="A tie occured in the seat allocation algorithm.*")
  seats_tie_1_5s_first <- c(   2,  2,  1) # First six seats, tie between 5 and 6
  expect_equal(dhondt(votes_tie_1, 5, ties="first"), seats_tie_1_5s_first)
})

test_that("dhondt handles illegal values",{

  # Make sure bad input results in error
  expect_error(dhondt(-1))
  expect_error(dhondt(-1:5))
  expect_error(dhondt(1:5, -1))
  expect_error(dhondt(1:5, 0.5))
  expect_error(dhondt(1:5, 1.2))
  expect_error(dhondt(integer(0), 1))

  # We refuse to count zero votes (even if they are strictly speaking a tie)
  expect_error(dhondt(c(0,0,0)))
  expect_error(dhondt(c(0,0,0), ties="first"))
})


#### Test largest_remainder() ####
test_that("largest_remainder works",{

  # Test vectors (initially created using electoral package example and)
  votes_2 <-  c(100, 150, 60, 80, 160)
  seats_2_named <- c(V = 3L, W = 4L, X = 2L, Y = 2L, Z = 4L)
  votes_2_named <-  c(100, 150, 60, 80, 160)
  names(votes_2_named) <- names(seats_2_named)

  votes_3_named <- c(DEM=490205, PMDB=1151547, PRB=2449440,
                                        PSB=48274, PSTU=54403, PTC=173151)
  seats_3_named <- c(DEM = 2L, PMDB = 5L, PRB = 11L, PSB = 0L, PSTU = 0L, PTC = 1L )

  # seats from seats_lr(), and names removed
  set.seed(42)
  votes_4 <- sample(10000, 5)
  seats_4 <- c(1L, 1L, 3L, 0L, 2L)

  # Test basic case with and without names
  expect_equal(largest_remainder(votes_2, sum(seats_2_named)), unname(seats_2_named))
  expect_equal(largest_remainder(votes_2_named, sum(seats_2_named)), seats_2_named)

  # A couple of other values
  expect_equal(largest_remainder(votes_3_named, sum(seats_3_named)), seats_3_named)
  expect_equal(largest_remainder(votes_4), seats_4) # Rely on the default 7

  # The function needs to handle the allocation of zero votes or zero seats
  # (zero parties are not allowed)
  expect_equal(largest_remainder(c(1,0,0)), c(7,0,0))
  expect_equal(largest_remainder(c(1,0,0), seats=0), c(0,0,0))
})

test_that("largest_remainder tie handling works",{

  # Silly minimal example tie handling – but breaks as of 22.02.2022
  expect_equal(seats_fractional (c(1,1), 2), c(1,1))
  expect_equal(largest_remainder(c(1,1), 2), c(1,1))

  # Simple case of two equal parties, but three seats
  expect_equal(seats_fractional (c(1,1), 3), c(1.5,1.5))
  expect_error(largest_remainder(c(1,1), 3))
  expect_error(largest_remainder(c(1,1), 3, ties="error"))
  expect_equal(largest_remainder(c(1,1), 3, ties="first"), c(2,1))

  # Two seats are tied for seats 8 and 9
  expect_equal(seats_fractional (c(3,4,3), 8), c(2.4,3.2,2.4))
  expect_error(largest_remainder(c(3,4,3), 8))
  expect_error(largest_remainder(c(3,4,3), 8, ties="error"))
  expect_equal(largest_remainder(c(3,4,3), 8, ties="first"), c(3,3,2))

  # Ties in remainder without absolute ties in votes, so the
  # order affects distribution in a more obvious way
  expect_equal(seats_fractional (c(9,3), 2), c(1.5,0.5))
  expect_error(largest_remainder(c(9,3), 2))
  expect_error(largest_remainder(c(9,3), 2, ties="error"))
  expect_equal(largest_remainder(c(9,3), 2, ties="first"), c(2,0))

  # Flip the order of the above result
  expect_equal(seats_fractional (c(3,9), 2), c(0.5,1.5))
  expect_error(largest_remainder(c(3,9), 2))
  expect_error(largest_remainder(c(3,9), 2, ties="error"))
  expect_equal(largest_remainder(c(3,9), 2, ties="first"), c(1,1))
})

test_that("largest_remainder handles illegal values",{

  # We refuse to count zero votes (even if they are strictly speaking a tie)
  expect_error(largest_remainder(c(0,0,0)))
  expect_error(largest_remainder(c(0,0,0), ties="first"))

  # Make sure bad input results in error
  expect_error(largest_remainder(-1))
  expect_error(largest_remainder(-1:5))
  expect_error(largest_remainder(1:5, -1))
  expect_error(largest_remainder(1:5, 0.5))
  expect_error(largest_remainder(1:5, 1.2))
  expect_error(largest_remainder(integer(0), 1))
})
