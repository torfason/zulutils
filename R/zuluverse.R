
##
## A set of utility functions related to dplyr and the tidyverse
##


#' Mutate only rows of data.frame/tibble that satisfy condition
#'
#' @description The [mutate_cond()] function allows simple conditional mutations
#' of `data.frames` by combining a conditional to select rows, followed by
#' `dplyr::mutate()` syntax to specify how to change columns. This function is
#' inspired by, and based on, a function proposed in [a discussion on Stack
#' Overflow](https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows).
#' This function, however, extends the original `mutate_cond()` function
#' proposed in that thread by falling back on using `base::transform()` if the
#' `dplyr` package is not installed.
#'
#' @param .data The data.frame to mutate.
#' @param .condition Conditional statement determining which rows to modify.
#' @param ... One or more statements determining which columns to mutate and how.
#' @param .envir Which environment to use for evaluation.
#'
#' @param .method Which underlying method to use for the mutation.
#'   Acceptable values are:
#'
#'   * `"default"`, the default, uses `dplyr::mutate()` if available, but
#'     falls back on `base::transform()` (with a warning) if not.
#'   * `"dplyr"` uses `dplyr::mutate()` and throws an error if it is not
#'     available.
#'   * `"base"` always uses `base::transform()`.
#'
#' @details
#' All named parameters are prefixed with `.` to reduce the probability of
#' conflict with the column names that are to be mutated, which are specified in
#' the `...` parameter.
#'
#' The function relies on either `dplyr::mutate()` if available, or
#' `base::transform()` if the `dplyr` package is available. Specifying multiple
#' mutations in `...` is allowed, but, if doing so, it is important to be aware
#' of the differences between `mutate()` and `transform()`. The two
#' functions are very similar, apart from the fact that `mutate()` executes the
#' transformations in an iterative manner, so that later transformations can use
#' the columns created by earlier transformations, whereas `transform()` uses
#' the values from the original `data.frame`, regardless of the number of steps.
#'
#' @examples
#' # Set dist to 3 where speed is 7 or less
#' result <- mutate_cond(cars, speed<=7, dist=3)
#' head(result)
#'
#' @family functions extending `dplyr`
#' @md
#' @export
mutate_cond <- function(.data, .condition, ..., .envir = parent.frame(), .method=c("default", "dplyr", "base"))
{
  .method <- match.arg(.method)

  # Select underlying mutate function based on specified method and package availability
  if ( requireNamespace("dplyr") && .method != "base" ) {
    mutate_function <- dplyr::mutate
  } else if ( .method == "base" ) {
    mutate_function <- base::transform
  } else if ( !requireNamespace("dplyr") && .method == "default" ) {
    warning("dplyr::mutate() not found, using base::transform()\n which has slightly different behavior.")
    mutate_function <- base::transform
  } else {
    stop("'dplyr' method specified, but the 'dplyr' package was not found. Install 'dplyr' or use another method.")
  }

  # Do the conditional mutation
  .condition <- eval(substitute(.condition), .data, .envir)
  .condition[is.na(.condition)] = FALSE
  .data[.condition, ] <- .data[.condition, ] %>% mutate_function(...)
  .data
}


#' @rdname zulutils-defunct
#' @section `move`:
#' `move()` is defunct. Use [dplyr::relocate()].
#'
#' @md
#' @export
move <- function(...) {
  .Defunct("relocate", package = "dplyr", msg="move() is defunct. Use dplyr::relocate().")
}


