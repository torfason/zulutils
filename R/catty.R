
#' Construct a catty vector
#'
#' @description
#' [catty()] constructs a `catty` vector (of class `zmisc_catty`). A  `catty`
#' vector is a `character` vector that uses [cat()] for print output, rather
#' than the standard `print()` command. Apart from this, it behaves like a
#' standard character vector.
#'
#' Objects of this class are convenient return values for functions that should
#' generally just print a desired value without the index information of a
#' normal print command. This is what happens if a function returning a `catty`
#' object is called interactively. However, the return value can also be
#' assigned to a variable for later use, making this more flexible than having
#' the function output the text directly.
#'
#' @param x The `character` vector we want to behave in a `catty` manner.
#' @param sep The separator to output between elements of the `catty` vector.
#'
#' @return The `catty` vector.
#'
#' @examples
#' x <- catty(letters)
#' x
#' y <- catty(month.name, sep="\n")
#' y
#'
#' @md
#' @export
catty <- function(x, sep = " ") {
  stopifnot(is.character(x))
  class(x) <- c("zmisc_catty", class(x))
  attr(x, "sep") <- sep
  x
}

#' Print a catty vector
#'
#' `print.zmisc_catty(x, ...)` prints a `catty` vector (using `cat()` instead of
#' `print()` for the output).
#'
#' @param x The catty vector
#' @param ... Any other arguments to print (they will be ignored)
#'
#' @return The input is returned invisibly.
#'
#' @keywords internal
#' @md
#' @export
print.zmisc_catty <- function(x, ...) {
  validate.zmisc_catty(x)
  cat(x, sep = attr(x, "sep"))
  invisible(x)
}


#' Internal validation function for catty objects
#'
#' @param x The object to validate
#'
#' @keywords internal
#' @md
#' @return The object
validate.zmisc_catty <- function(x) {
  stopifnot(inherits(x, "zmisc_catty"))
  stopifnot(is.character(x))
  stopifnot(is.character(attr(x, "sep")))
  invisible(x)
}


