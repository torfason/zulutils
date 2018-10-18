
# Welcome to the zulutils package
#
# This is the base soure file for the zulutils package
#
# Reference for package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
# Roxygen documentation:
#
#   https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html


dummy = function()
{
    unknownpackage::somefunction()
}

# It would also be useful to add psum, pprod and pmean


#' Do return a single argument unchanged
#'
#' It can be useful (when developing pipelines for example),
#' to have acces to a function that simply returns the elemtent
#' that it receives. This is that function
noop = function(x)
{
    x
}


#' Generate sequence in a safe way
#'
#' This function creates an increasing integer sequence, but differs from
#' the standard one in that it will not silently generate a decreasing
#' sequence when the second argument is smaller than the first.
#' If the second arg is one smaller than the first it will generate
#' an empty sequence, if the difference is greater, the function will
#' error out.
zeq = function(from, to)
{
    stopifnot ( round(from) == from )
    stopifnot ( round(to)   == to   )
    stopifnot ( to >= from - 1      )
    return (seq_len(1+to-from)+from-1)
}


#' Sample from a vector or data.frame in a safe way
#'
#' This function duplicates the functionality of sample(), with the exception
#' that it does not attempt the (rather silly in my opinion) user-friendliness
#' of switching the interpretation of the first element to a number if the
#' length of the vector is 1.
#'
#' To sample from an interval between 1 and n, simply use safer.sample(1:n)
#' (or use sample(n) instead)
zample = function (x, size=length(x), replace = FALSE, prob = NULL)
{
    # Special handling to allow sampling from a data.frame
    if (class(x) == "data.frame")
    {
        size = ifelse( is.null(size), nrow(x), size )
        return( x[ safer.sample( zeq(1,nrow(x)), size, replace, prob ), ] )
    }

    # Sampling zero elements from a zero length vector should be allowed
    if ( size==0 & length(x)==0 ) { return(vector(mode=mode(x))) }

    # The code from original sample(), minus the silly numeric handling
    x[.Internal(sample(length(x), size, replace, prob))]
}


#' Return the single (unique) value found in a vector
#'
#' Returns the first element in a vector, but only if all the other
#' elements are identical to the first one (the vector only has a
#' zingle value). If the elements are not all identical, it throws
#' an error. The vector must contain at least one non-na value,
#' or the function errors out as well.
#'
#' This is a useful function in aggregations when
#' all values in a given group should be identical, but
#' you want to make sure.
#'
#' Optionally takes a na.rm parameter, similarly to sum, mean and
#' other aggregate functions.
zingle = function(x, na.rm = FALSE)
{
    if (na.rm) x = x[!is.na(x)]
    stopifnot(all(x[1]==x))
    return(x[1])
}

#' Replace missing values
#'
#' Usage: na.replace(x, replace)
#' Arguments
#'
#' x : vector possibly contining missing (NA) values.
#' replace : scalar replacement value
na.replace = function (x, replace)
{
    x[is.na(x)] <- replace
    x
}

#' Left pad string with whitespace or other characters
#'
#' Left pad a string with whitespace or other characters
#' in order to get a string of a certain length.
#' This function is a thin wrapper around stringr::str_pad()
lpad = function(string, width, pad=" ")
{
    stringr::str_pad(string, width, "left", pad)
}

#' Right pad string with whitespace or other characters
#'
#' Right pad a string with whitespace or other characters
#' in order to get a string of a certain length.
#' This function is a thin wrapper around stringr::str_pad()
rpad = function(string, width, pad=" ")
{
    stringr::str_pad(string, width, "right", pad)
}

#' Boolean grep function
#'
#' This function looks for pattern in each of the elements
#' of string, returning TRUE for each element that contains
#' pattern, and FALSE for the other elements. The argument
#' order follows the conventions of the stringr package,
#' and the underlying matching is done using stringr::str_extract()
bgrep = function(string, pattern)
{
    v.match = stringr::str_extract(string, pattern)
    return(!is.na(v.match))
}
