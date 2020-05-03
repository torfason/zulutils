
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


# It would also be useful to add psum, pprod and pmean


#' Return a single argument unchanged
#'
#' It can be useful (when developing pipelines for example),
#' to have access to a function that simply returns the element
#' that it receives. This is that function.
#'
#' @param x The single argument to (not) process
#' @return The argument \code{x} is returned unchanged
#' @export
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
#'
#' @param from The lower bound of the sequence
#' @param to   The higher bound of the sequence
#' @return     A sequence ranging from \code{from} to \code{to}
#' @export
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
#' length of the vector is 1.  To sample from an interval between 1 and n,
#' simply use zample(1:n) (or use sample(n) instead)
#'
#' @param x       The vector to sample from
#' @param size    The number of elements to sample from \code{x} (defaults to \code{length(x)})
#' @param replace Should elements be replaced after sampling (defaults to \code{false})
#' @export
zample = function (x, size=length(x), replace = FALSE, prob = NULL)
{
    # Bail out of sampling from data.frames, use dplyr::sample_X() for that
    if (class(x) == "data.frame")
    {
        stop("zulutils::zample() does not support data.frames")
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
#'
#' @export
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
#'
#' @export
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
#'
#' @export
lpad = function(string, width, pad=" ")
{
    stringr::str_pad(string, width, "left", pad)
}

#' Right pad string with whitespace or other characters
#'
#' Right pad a string with whitespace or other characters
#' in order to get a string of a certain length.
#' This function is a thin wrapper around stringr::str_pad()
#'
#' @export
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
#'
#' @param string The (possibly vectorized) string to process
#' @param pattern The (possibly vectorized) regexp pattern to use
#' @export
bgrep = function(string, pattern)
{
    v.match = stringr::str_extract(string, pattern)
    return(!is.na(v.match))
}

