
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
