
##
## Default source file for functions in the zulutils package
##

# It would also be useful to add psum, pprod and pmean


#' Return argument unchanged
#'
#' @description
#' It can often be useful (when developing pipelines for example), to have
#' access to a function that simply returns the element that it receives. The
#' [base::identity()] function works well for only one argument, but in some
#' cases following arguments should be ignored. In those cases the
#' `identity_ellipsis()` function can come in handy.
#'
#' The function was previously called `noop()` but that name is now deprecated.
#'
#' @param x The single argument to (not) process.
#' @param ... Any other arguments are ignored
#' @return The argument `x` is returned unchanged.
#'
#' @md
#' @export
identity_ellipsis <- function(x, ...) {
  identity(x)
}

#' @rdname identity_ellipsis
#' @export
noop <- function(x, ...)  {
  .Deprecated("noop", msg=paste0("noop() is deprecated in favor of base::identity(), ",
                                 "or identity_ellipsis() in case of multiple arguments"))
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

#' Calculate standard error (of the mean estimate) of a vector
#'
#'
#' creating the need for this function.
#'
#' @param x A numeric vector, or of a type allowable in the `sd()` function.
#' @param na.rm A logical indicating whether missing values should be removed.
#' @return The standard error of mean estimate of x
#'
#' @family Statistical functions
#' @md
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @export
se <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sd(x) / sqrt(length(x))
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
#' elements. This function is a thin wrapper around [stringr::str_detect()]
#'
#' @param string The (possibly vectorized) string to process.
#' @param pattern The (possibly vectorized) regexp pattern to use.
#' @return A vector with boolean values indicating in which elements
#'         of `string` matched the corresponding elements in `pattern`.
#' @md
#' @export
bgrep = function(string, pattern)
{
  stringr::str_detect(string, pattern)
}

#' Sample a set of strings, each string of a given length
#'
#' @param nchar The length of each string.
#' @param size The number of strings.
#' @param upper Should the strings be upper case?
#' @return A random vector of `size` strings of length `nchar`.
#'
#' @examples
#' set.seed(42)
#' sample_strings()
#' d <- data.frame(key = sample_strings(7,9), value=1:9)
#' d[order(d$key),]
#'
#' @md
#' @export
sample_strings <- function(nchar=3, size=1, upper=FALSE) {
  if (upper) pattern<-"[A-Z]" else pattern <- "[a-z]"
  stringi::stri_rand_strings(n=size, length=nchar, pattern=pattern)
}

#' A modified version of the str() function that limits the length of displayed lists.
#'
#' @param object the object to be printed.
#'
#' @param max.level the maximum number of nested levels to be printed. Default is 1.
#'
#' @param list.len the maximum number of elements to be printed for a list.
#'   Default is determined by the value of max.level and the nth prime number.
#'
#' @param ... additional arguments to be passed to str().
#'
#' @return A printed representation of the object.
#'
#' @md
#' @export
strh <- function(object, max.level = 1, list.len = nth_prime(5-max.level), ...) {
  print(list.len)
  utils::str(object, max.level=max.level, list.len=list.len, ...)
}
