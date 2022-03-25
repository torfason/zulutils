
##
## Default source file for functions in the zulutils package
##

# It would also be useful to add psum, pprod and pmean


#' Return a single argument unchanged
#'
#' It can often be useful (when developing pipelines for example),
#' to have access to a function that simply returns the element
#' that it receives. This is that function.
#'
#' @param x The single argument to (not) process.
#' @param ... Any other arguments are ignored
#' @return The argument `x` is returned unchanged.
#' @md
#' @export
noop = function(x, ...)
{
    x
}


#' Replace missing values
#'
#' @description
#'
#' Replaces NA values found in a vector with a specified scalar value
#'
#' `NOTE:` The [dplyr::coalesce()] function now provides this functionality in a
#' more general way, so this function may merit deprecation.
#'
#' @param x Vector possibly containing missing (`NA`) values.
#' @param replace Scalar replacement value.
#' @return Copy of x with any missing values replaced with replace.
#'
#' @family functions for `NA` handling
#' @md
#' @export
na_replace = function (x, replace)
{
  .Deprecated("na.replace", msg="na.replace() is deprecated in favor of dplyr::coalesce().")
  x[is.na(x)] <- replace
  x
}

#' @rdname zulutils-deprecated
#' @section `na.replace`:
#' `na.replace()` is deprecated in favor of [dplyr::coalesce()].
#'
#' @md
#' @export
na.replace <- function(x, replace) {
  .Deprecated("na.replace", msg="na.replace() is deprecated in favor of dplyr::coalesce().")
  na_replace(x, replace)
}

#' Replace values in x that are contained in y with NA.
#'
#' @description
#' This function is a variant/extension of [dplyr::na_if()]. Whereas that
#' version requires `y` to be of length one, this function allows `y` to be a
#' vector and replaces any elements of `x` that are found in `y` with `NA`
#' values.
#'
#' This function can be very useful when cleaning data and
#' setting multiple any annoying values to `NA`.
#'
#' @param x Vector to modify.
#' @param y Value to replace with `NA`.
#' @return A modified version of `x` that replaces any values that are found in `y` with `NA`.
#'
#' @family functions for `NA` handling
#' @md
#' @export
na_if_in <- function(x,y)
{
    x[x %in% y] <- NA
    x
}

#' Pad string with space or other characters
#'
#' Left or right pad a string with space or other
#' characters in order to get a string of a certain length.
#' This function is a thin wrapper around [stringr::str_pad()].
#'
#' @param string Character vector to pad.
#' @param width How many characters at minimum in the output.
#' @param pad The string to pad with.
#' @rdname padding
#' @name   padding
#' @md
#' @export
lpad = function(string, width, pad=" ")
{
  stringr::str_pad(string, width, "left", pad)
}

#' @rdname padding
#' @name   padding
#' @md
#' @export
rpad = function(string, width, pad=" ")
{
  stringr::str_pad(string, width, "right", pad)
}

#' Boolean grep function
#'
#' This function looks for pattern in each of the elements of string, returning
#' TRUE for each element that contains pattern, and FALSE for the other
#' elements. The argument order follows the conventions of the `stringr`
#' package, and the underlying matching is done using [stringr::str_extract()].
#'
#' @param string The (possibly vectorized) string to process.
#' @param pattern The (possibly vectorized) regexp pattern to use.
#' @return A vector with boolean values indicating in which elements
#'         of `string` matched the corresponding elements in `pattern`.
#' @md
#' @export
bgrep = function(string, pattern)
{
  # THIS SHOULD JUST USE GREPL
  # (or be replaced with a zfun(grepl, x) for us in pipes
  v.match = stringr::str_extract(string, pattern)
  return(!is.na(v.match))
}



#' Sample a set of strings, each string of a given length
#'
#' @param nchar The length of each string.
#' @param size The number of strings.
#' @param upper Should the strings be upper case?
#' @return A random vector of `size` strings of length `nchar`.
#'
#' @examples
#' sample_strings()
#' d <- data.frame(key = sample_strings(7,9), value=1:9)
#' d[order(d$key),]
#'
#' @md
#' @export
sample_strings <- function(nchar=3, size=1, upper=FALSE) {
  ltr <- if (upper) {LETTERS} else {letters}
  sample(ltr, nchar*size, replace=TRUE) |>
    matrix(nrow = nchar, ncol = size) |>
    apply(MARGIN=2, paste, collapse="")
}
