
##
## A set of utility functions related to ggplot
##

#' Apply a function to each label and character/factor variable in the plot object.
#'
#' Applies the string function `fun` to each label present in the plot
#' object `p`, as well as to any character or factor variables in the
#' underlying data. The function, `fun`, should accept and return character
#' vectors. It can either be a simple prettifying function or it can perform
#' more complex lookup to replace variable names with variable labels. If
#' variables are factors, they are converted to character before applying the
#' function after which they are reconverted to factor. Care is taken to
#' preserve the factor ordering.
#'
#' @param p A `ggplot2` object
#' @param fun A function to be applied to the labels in `p`
#' @param ... Other variables to be passed to `fun`
#'
#' @param .labs logical or character, indicating whether to apply the function
#'   to the labels in p. If TRUE, apply to all character or factor variables in
#'   `p`. If `FALSE`, `NULL` or empty vector, do not apply the function to any
#'   variables. If a character vector containing the names of variables, apply
#'   the function to those particular variables. An error may be thrown if the
#'   vector refers to non-existing labels. Defaults to `TRUE`.
#'
#' @param .vars logical or character. If `TRUE`, apply to all character or factor
#'   variables in p. If `FALSE`, `NULL` or empty vector, do not apply the function
#'   to any variables. If a character vector containing the names of variables,
#'   apply the function to those particular variables. An error may be thrown if
#'   the vector refers to non-existing variables or variables that are neither
#'   characters nor factors. Defaults to `TRUE`.
#'
#' @return A `ggplot2` object with changed labels
#'
#' @examples
#' # This uses snakecase::to_sentence_case to prettify the labels
#' # Note: The plot is assigned to a named variable before piping to gg_apply_labs()
#' #       This is to avoid issues due to the precedence of operators,
#' #       (%>% has higher precedence than +)
#' library(tidyverse)
#' library(snakecase)
#' p <- starwars %>%
#'     filter(mass < 1000) %>%
#'     mutate(species = species %>% fct_infreq %>%  fct_lump(5) %>% fct_explicit_na) %>%
#'     ggplot(aes(height, mass, color=species, size=birth_year)) +
#'     geom_point()
#' p %>% gg_apply(snakecase::to_sentence_case)
#'
#' @family functions extending `ggplot`
#' @md
#' @export
gg_apply <- function(p, fun, ..., .labs=TRUE, .vars=TRUE) {

  # Calculate new label values, and test that fun returns a sane result
  labels_new <- lapply(p$labels, fun, ...)
  stopifnot(
    all(sapply(labels_new, is.character)),
    length(labels_new) == length(p$labels)
  )

  # If .labs is true, we replace the labels in p
  if ( isTRUE(.labs) ) {
    .labs <- names(p$labels)
  }
  if ( isFALSE(.labs) || is.null(.labs) || all(is.na(.labs)) ) {
    .labs <- character()
  }
  stopifnot(is.character(.labs))

  # .labs is now a character vector of labels to replace
  for ( lab_name in .labs ) {
    p$labels[[lab_name]] <- labels_new[[lab_name]]
  }

  # Process non-character list values for labs.
  # If neither of these conditions is true, vars MUST be a
  # character vector, or we bail.
  if ( isTRUE(.vars) ) {
    .vars <- names(p$data)
  }
  if ( isFALSE(.vars) || is.null(.vars) || all(is.na(.vars)) ) {
    .vars <- character()
  }
  stopifnot(is.character(.vars))

  # .vars is now a character vector of variables to replace
  for ( var_name in .vars ) {

    # Process a character variable and do some sanity testing
    if ( is.character(p$data[[var_name]]) ) {
      var_new <- fun(p$data[[var_name]], ...)
      stopifnot(
        is.character(var_new),
        length(var_new) == length(p$data[[var_name]])
      )
      p$data[[var_name]] <- var_new
    }

    # Process a factor variable and do some sanity testing
    if ( is.factor(p$data[[var_name]]) ) {
      var_fct_old <- p$data[[var_name]]
      var_chr_new <- fun(as.character(var_fct_old), ...)
      levels_new  <- fun(levels(var_fct_old), ...)
      stopifnot(
        is.character(var_chr_new),
        length(var_chr_new) == length(var_fct_old)
      )
      p$data[[var_name]] <- factor(var_chr_new, levels=levels_new)
    }

  }

  p
}


#' Apply a string function to each label present in the plot object
#'
#' Applies the string function `fun to each label present in the plot object
#' `p`. The function, `fun`, should accept and return character vectors. It can
#' either be a simple prettifying function or it can perform more complex lookup
#' to replace variable names with variable labels
#'
#' @param p A `ggplot2` object
#' @param fun A function to be applied to the labels in `p`
#'
#' @return A `ggplot2` object with changed labels
#'
#' @examples
#' # This uses snakecase::to_sentence_case to prettify the labels
#' # Note: The plot is assigned to a named variable before piping to gg_apply_labs()
#' #       This is to avoid issues due to the precedence of operators,
#' #       (%>% has higher precedence than +)
#' library(tidyverse)
#' library(snakecase)
#' p <- starwars %>%
#'     filter(mass < 1000) %>%
#'     mutate(species = species %>% fct_infreq %>%  fct_lump(5) %>% fct_explicit_na) %>%
#'     ggplot(aes(height, mass, color=species, size=birth_year)) +
#'     geom_point()
#' p %>% gg_apply_labs(snakecase::to_sentence_case)
#'
#' @family functions extending `ggplot`
#' @md
#' @export
gg_apply_labs <- function(p, fun) {
  .Deprecated("gg_apply")
  p$labels <- lapply(p$labels, fun)
  p
}


#' Apply integer breaks to a graph.
#'
#' Calculates a vector of breaks that contains all integers between
#' 0 and max(x) (with a little bit of extra headroom in case the
#' maximum element is an integer)
#'
#' @param x The input vector for which the breaks should be calculated.
#'   Note that there is no fancy NSE here, the actual variable must be
#'   passed to the function, and also that the function currently does
#'   not handle negative data and will error out if passed such data.
#'
#' @return A vector of breaks suitable for use with `ggplot` breaks parameter.
#'
#' @examples
#' if ( require(ggplot2) ) {
#'   ggplot(mtcars) +
#'     aes(wt,drat) +
#'     geom_point() +
#'     scale_y_continuous(breaks=gg_integer_breaks(mtcars$drat))
#' }
#'
#' @family functions extending `ggplot`
#' @md
#' @export
gg_integer_breaks <-function(x){
  stopifnot(x>0)
  unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
}
