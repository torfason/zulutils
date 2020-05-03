


#' Apply a string function to each label present in the plot object
#'
#' Applies the string function \code{fun} to each label present in
#' the plot object \code{p}. The function, \code{fun}, should accept
#' and return character vectors. It can either be a simple prettyfying
#' function or it can perform more complex lookup to replace
#' variable names with variable labels
#'
#' @param p A \code{ggplot2} object
#' @param fun A function to be applied to the labels in \code{p}
#'
#' @return A \code{ggplot2} object with changed labels
#'
#' @examples
#' # This uses snakecase::to_sentence_case to prettify the labels
#' # Note: The plot is assigned to a named variable before piping to apply_labs()
#' library(tidyverse)
#' library(snakecase)
#' p <- starwars %>%
#'     filter(mass < 1000) %>%
#'     mutate(species = species %>% fct_infreq %>%  fct_lump(5) %>% fct_explicit_na) %>%
#'     ggplot(aes(height, mass, color=species, size=birth_year)) +
#'     geom_point()
#' p %>% gg_apply_labs(snakecase::to_sentence_case)
#'
#' @family zulugg
#' @export
#'
gg_apply_labs <- function(p, fun) {
    p$labels <- lapply(p$labels, fun)
    p
}



# Apply integer breaks to a graph

#' @family zulugg
#' @export
gg_integer_breaks <-function(x){
    unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
}
