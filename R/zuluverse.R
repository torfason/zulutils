
##
## A set of utility functions related to dplyr and the tidyverse
##


#' Mutate only rows of data.frame/tibble that satisfy condition
#'
#' https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
#'
#' @family zuluverse
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame())
{
    condition <- eval(substitute(condition), .data, envir)
    condition[is.na(condition)] = FALSE
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}


#' @rdname zulutils-defunct
#' @section \code{move}:
#' \code{move()} is defunct. Use \code{\link{dplyr::relocate()}}.
#'
#' @export
move <- function(...) {
  .Defunct("relocate", package = "dplyr", msg="move() is defunct. Use dplyr::relocate().")
}


