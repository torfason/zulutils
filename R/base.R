
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

dummy = function()
{
    unknownpackage::somefunction()
}

# It would also be useful to add psum, pprod and pmean

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
