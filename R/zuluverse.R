# zuluverse - Currently part of zulutils
#
# A collection of utility functions applicable to the tidyverse.
# Hopefully this will remain a pretty small collection.
#
# Some useful keyboard shortcuts for package authoring:
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'




#' Mutate only rows of data.frame/tibble that satisfy condition
#'
#' https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
#'
#' @export
#'
mutate_cond <- function(.data, condition, ..., envir = parent.frame())
{
    condition <- eval(substitute(condition), .data, envir)
    condition[is.na(condition)] = FALSE
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}


#' Move column or selection of columns
#'
#' Column(s) described by \code{cols} are moved before (default) or after the reference
#'   column described by \code{ref}
#'
#' All credit to Moody Mudskipper:
#' https://stackoverflow.com/questions/52096919/move-a-column-conveniently/52096938#52096938
#'
#' See also:
#' https://github.com/tidyverse/dplyr/issues/2047
#'
#' @param data A \code{data.frame}
#' @param cols unquoted column name or numeric or selection of columns using a select helper
#' @param ref unquoted column name
#' @param side \code{"before"} or \code{"after"}
#'
#' @return A data.frame with reordered columns
#'
#' @examples
#' iris2 <- head(iris,2)
#' move(iris2, Species, Sepal.Width)
#' move(iris2, Species, Sepal.Width, "after")
#' move(iris2, 5, 2)
#' move(iris2, 4:5, 2)
#' move(iris2, one_of("Sepal.Width","Species"), Sepal.Width)
#' move(iris2, starts_with("Petal"), Sepal.Width)
#'
#' @export
#'
move <- function(data, cols, ref, side = c("before","after")){
    if(! requireNamespace("dplyr"))
        stop("Make sure package 'dplyr' is installed to use function 'move'")
    side <- match.arg(side)
    cols <- rlang::enquo(cols)
    ref  <- rlang::enquo(ref)
    if(side == "before")
        dplyr::select(data,1:!!ref,-!!ref,-!!cols,!!cols,dplyr::everything())
    else
        dplyr::select(data,1:!!ref,-!!cols,!!cols,dplyr::everything())
}

