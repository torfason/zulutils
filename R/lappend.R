
#' Append individual values to a list
#'
#' @description
#' If passed two lists, the standard [append()] function will *concatenate* the
#' lists, rather than treating the second list as a value that should be
#' appended to the list. In contrast, the [lappend()] function will always
#' append individual values to a list (it does this by making sure that each
#' element is encapsulated in its own list before using [base::append()] to put
#' it in the original list).
#'
#' Importantly, this approach correctly allows adding a `data.frame` to a list,
#' whereas calling `base::append()` directly will add each column as a separate
#' element.
#'
#' @param l The list to append to.
#' @param ... One or more values to be appended to the list, each as a separate
#'   list element.
#' @param .after At what position to append.
#'
#' @return A list with the specified values appended at the correct position.
#'
#' @examples
#' l <- list()
#' l <- append(l, cars)
#' l <- append(l, mtcars)
#' length(l) # l is a list of 13 vectors
#'
#' l <- list()
#' l <- lappend(l, cars)
#' l <- lappend(l, mtcars)
#' length(l) # l is a list of 2 data.frames
#'
#' @md
#' @export
lappend <- function(l, ..., .after=length(l)) {

  # Process arguments
  stopifnot(is.list(l))
  values <- list(...)

  # Now that the values are already encapsulated in
  # a list, we can offload the appending to the base
  base::append(l, values, after=.after)
}
