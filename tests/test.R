
# These tests use testthat
library(testthat)

# We need some tidyverse for now
library(dplyr)

# The package is required
library(zulutils)

#### Dummy tests ####
# (uncomment next line to force failed test)
# expect_equal( "ThisTestWillFail", TRUE )
    a <- 9
expect_that  (a, is_less_than(10) )
expect_lt    (a, 10 )
expect_equal (a, 9 )


#### Test zeq() ####

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


#### Test zample() ####
    set.seed(1)
expect_equal(zample(1:10),      c(3L, 4L, 5L, 7L, 2L, 8L, 9L, 6L, 10L, 1L)  )
expect_equal(zample(1:10,1),        3     )
expect_equal(zample(10),           10     )


#### Test zingle() ####
expect_equal( zingle(rep(10,10)),               10 )
expect_equal( zingle(c(1,1,NA,1), na.rm=TRUE),   1 )
expect_error( zingle(1:2) )
expect_error( zingle(c(1,1,NA,1)) )


#### Test bgrep() ####
expect_equal( bgrep(letters,"a"),                    c(TRUE,rep(FALSE,25))  )
expect_equal( bgrep(letters,"z"),                    c(rep(FALSE,25),TRUE)  )
expect_equal( bgrep(c("apple","orange","pear"),"r"), c(FALSE,TRUE,TRUE)     )


#### Test noop() ####
expect_equal( noop(letters),     letters )


#### test lookup_enframed() ####
    d.pets = tibble::enframe(c(
        cat="mammal",
        lizard="reptile",
        parrot="bird"
    ))
    x = c("lizard", "cat")
expect_equal( lookup_enframed(x,d.pets),  c("reptile","mammal")  )

# Order should not matter.
d.pets.rearranged = d.pets %>%
    mutate(ga = "ga", rb="rb", age="age") %>%
    select(ga,name,rb,value,age) %>%
    print
expect_equal( lookup_enframed(x,d.pets.rearranged),  c("reptile","mammal")  )


#### test rename_enframed() ####
    data   = tibble::tibble(a=1:4,b=letters[1:4])
    final  = tibble::tibble(`The Numbers`=1:4, `The Letters`=letters[1:4])
    labels = tibble::enframe(c(a="The Numbers",b="The Letters"))
expect_equal(rename_enframed(data,labels), final)


#### DONE ####
cat("test.R : All tests ran successfully ...\n")
