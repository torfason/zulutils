

library(testthat)


# Dummy tests
a <- 9
expect_that(a, is_less_than(10))
expect_lt(a, 10)


#### Test zeq() ####

# Positive intervals should equal seq
expect_that(zeq(1,2), equals(seq(1,2)))
expect_that(zeq(1,10), equals(seq(1,10)))
expect_that(zeq(5,10), equals(seq(5,10)))
expect_that(zeq(20,20), equals(seq(20,20)))

# Identity should have length one
expect_that( zeq(1,1), equals(1) )

# End one less than start gives empty sequence
expect_that( zeq(1,0), equals(numeric(0)) )

# End lower than start by two or more gives error
expect_error( zeq(2,0) )

#### Test zample() ####
set.seed(1)
expect_that(zample(1:10), equals(c(3L, 4L, 5L, 7L, 2L, 8L, 9L, 6L, 10L, 1L)))
expect_that(zample(1:10,1), equals(3))
expect_that(zample(10), equals(10))

#### Test zingle() ####

expect_that( zingle(rep(10,10)), equals(10))

expect_error( zingle(1:2) )

expect_error( zingle(c(1,1,NA,1)) )
expect_that ( zingle(c(1,1,NA,1), na.rm=TRUE), equals(1) )

#### Test bgrep() ####
expect_that( bgrep(letters,"a"), equals(c(TRUE,rep(FALSE,25))))
expect_that( bgrep(letters,"z"), equals(c(rep(FALSE,25),TRUE)))
expect_that( bgrep(c("apple","orange","pear"),"r"), equals(c(FALSE,TRUE,TRUE)))
