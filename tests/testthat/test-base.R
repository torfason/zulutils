
#### Dummy tests ####

# Boilerplate dummy test
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# My own dummy tests
# (uncomment next line to force failed test)
# expect_equal( "ThisTestWillFail", TRUE )
test_that("comparison works", {
    a <- 9
    expect_that  (a, is_less_than(10) )
    expect_lt    (a, 10 )
    expect_equal (a, 9 )
})


#### Test noop() ####
test_that("noop works", {
    expect_equal(noop(letters), letters)
    expect_equal(noop(NA), NA)
    expect_equal(noop(1:10), 1:10)
    expect_equal(noop(cars), cars)
})

#### Test zeq() ####
test_that("zeq works", {

    # Positive intervals should equal seq
    expect_equal( zeq(1,2),      seq(1,2)    )
    expect_equal( zeq(1,10),     seq(1,10)   )
    expect_equal( zeq(5,10),     seq(5,10)   )
    expect_equal( zeq(20,20),    seq(20,20)  )

    # Identity should have length one
    expect_equal( zeq(1,1),           1     )

    # End one less than start gives empty sequence
    expect_equal( zeq(1,0),    numeric(0)  )

    # End lower than start by two or more gives error
    expect_error( zeq(2,0) )
})


#### Test zample() ####
test_that("zample works", {

    # Expected first ten samples with seed at 1
    x = c(9L, 4L, 7L, 1L, 2L, 5L, 3L, 10L, 6L, 8L)
    s = c("y", "d", "g", "a", "b", "k", "n", "r", "w", "j", "f", "t",
          "q", "x", "i", "e", "u", "l", "s", "p", "o", "m", "v", "z",
          "c", "h")

    # Basic operations, numeric
    set.seed(1); expect_equal(zample(1:10),   x    )
    set.seed(1); expect_equal(zample(1:10,1), x[1] )
    set.seed(1); expect_equal(zample(1:10,2), x[1:2] )

    set.seed(1); expect_equal(zample(letters),   s    )
    set.seed(1); expect_equal(zample(letters,1), s[1] )
    set.seed(1); expect_equal(zample(letters,2), s[1:2] )

    # Sampling from a vector of length on should only
    # yield a single result (not treat as number of samples)
    set.seed(1); expect_equal(zample(10),      10     )
    set.seed(1); expect_equal(zample("a"),    "a"     )

    # Test that zero-length sampling works
    set.seed(1); expect_equal(zample(1:10,0),        numeric()   )
    set.seed(1); expect_equal(zample(letters,0),     character() )
    set.seed(1); expect_equal(zample(numeric()),     numeric()   )
    set.seed(1); expect_equal(zample(character()),   character() )
    set.seed(1); expect_equal(zample(numeric(),0),   numeric()   )
    set.seed(1); expect_equal(zample(character(),0), character() )

    # But sampling more than zero from zero-length vector should not work
    expect_error(zample(numeric(),   1))
    expect_error(zample(character(), 1))

    # Zample should not try to sample from data.frames
    expect_error(zample(cars))
    expect_error(zample(cars, 4))
})


#### Test zingle() ####
test_that("zingle works", {

    # If all equal, return first item
    expect_equal( zingle(rep(10,10)), 10 )
    expect_equal( zingle(rep("a",20)), "a" )
    expect_equal( zingle(as.factor(rep("a",20))), as.factor("a") )

    # If not all equal,
    expect_error( zingle(1:2) )

    # Check that NAs behave as expected with na.rm
    expect_equal( zingle(c( 1, 1,NA, 1, 1), na.rm=TRUE),   1 )
    expect_equal( zingle(c(NA,NA,NA, 1,NA), na.rm=TRUE),   1 )
    expect_equal( zingle(c(NA,NA,NA,NA,NA), na.rm=TRUE),   NA )

    # Without na.rm=TRUE, NAs should cause an error
    expect_error( zingle(c( 1, 1,NA, 1, 1)) )
    expect_error( zingle(c(NA,NA,NA, 1,NA)) )
    expect_error( zingle(c(NA,NA,NA,NA,NA)) )
})

#### Test na_replace() ####
test_that("na_replace is deprecated", {
  na_replace(c(1, 2, NA, 4, 5, NA), 3) |>
    expect_warning() |>
    expect_equal(c(1:5, 3))
})

#### Test na.replace() ####
test_that("na.replace is deprecated", {
  na.replace(c(1, 2, NA, 4, 5, NA), 3) |>
    expect_warning() |>
    expect_equal(c(1:5,3))
})

#### Test lpad() and rpad() ####
test_that("lpad and rpad work", {

    # with longer argument
    expect_equal(lpad("hello",10), "     hello")
    expect_equal(rpad("hello",10), "hello     ")

    # with shorter argument, return input
    expect_equal(lpad("hello",2),"hello")
    expect_equal(rpad("hello",2),"hello")

    # bail out if skipping argument
    expect_error(lpad("hello"))
    expect_error(rpad("hello"))
})


#### Test bgrep() ####
test_that("bgrep works", {
    expect_equal( bgrep(letters,"a"),                    c(TRUE,rep(FALSE,25))  )
    expect_equal( bgrep(letters,"z"),                    c(rep(FALSE,25),TRUE)  )
    expect_equal( bgrep(c("apple","orange","pear"),"r"), c(FALSE,TRUE,TRUE)     )
})

