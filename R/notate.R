
#' Embed factor levels and value labels in values.
#'
#' @description
#' This function adds level/label information as an annotation to either factors
#' or `labelled` variables. This function is called `notate()` rather than
#' `annotate()` to avoid conflict with [ggplot2::annotate()].
#'
#' When printing `labelled` variables from a `tibble` in a console, both the
#' numeric value and the text label are shown, but no variable labels. When
#' using the `View()` function, only variable labels are shown but no value
#' labels. For factors, there is no way to view the integer levels and values at
#' the same time.
#'
#' In order to allow the viewing of both variable and value labels at the same
#' time, this function converts both `factor` and `labelled` variables to
#' `character`, including both numeric levels (`labelled` values) and character
#' values (`labelled` labels) in the output.
#'
#' The output is not intended for further processing, only for viewing, for
#' example using the `View()` function. This can be achieved directly by using
#' the `ViewNotated()` or `ViewAnnotated()` functions (`ViewAnnotated()` is a
#' synonym for `ViewNotated()`.
#'
#' @param d The `data.frame` that one desires to annotate and/or view.
#'
#' @return The processed `data.frame`, suitable for viewing.
#'
#' @md
#' @export
notate <- function(d) {

  stopifnot(is.data.frame(d))

  num_to_zero_prefix_string <- function(x) {
    stopifnot(is.numeric(x))
    gsub(" ", "0", format(x))
  }

  # Converts labelled, using as_factor to prepend level
  notate.labelled <- function(x) {
    stopifnot(is.labelled(x))
    r <- x |>
      haven::as_factor(levels="both") |>
      as.character()
    labelled::var_label(r) <- paste0("<lbl> ", labelled::var_label(x))
    r
  }

  # Converts factors, prepending level manually
  # NAs should stay as NAs
  notate.factor <- function(x) {
    stopifnot(is.factor(x))
    r <- rep(c(character(0), NA), length(x))
    r[!is.na(x)] <- paste0("[", as.numeric(x[!is.na(x)]), "] ", as.character(x[!is.na(x)]))
    labelled::var_label(r) <- paste0("<fct> ", labelled::var_label(x))
    r
  }

  # Apply to individual columns
  result <- d |>
    dplyr::mutate_if(labelled::is.labelled, notate.labelled) |>
    dplyr::mutate_if(is.factor, notate.factor)

  # Return the result
  result
}


#' @rdname notate
#' @export
ViewNotated <- function(d) {
  View(notate(d))
}

#' @rdname notate
#' @export
ViewAnnotated <- ViewNotated
