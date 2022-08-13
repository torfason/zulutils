
##
## Functions for working with enframed and/or labelled tibbles
##


#' @rdname zulutils-deprecated
#' @section `lookup_enframed`:
#' `lookup_enframed()` is deprecated in favor of [zmisc::lookup()].
#'
#' @md
#' @export
lookup_enframed <- function(x, lookup_table) {
  .Deprecated("zmisc::lookup", msg="lookup_enframed() is deprecated in favor of zmisc::lookup().")
  zmisc::lookup(x, lookup_table)
}


#' Rename `tibble` columns according to values in another `tibble`.
#'
#' @description
#' This function renames variables from `d` according to the columns in
#' `d.enframed`, which should be in `enframed` format (two columns named `name`
#' and `value`, see [tibble::enframe()] for details). It will rename *from*
#' according to `d.enframed$name`, and *to* according to `d.enframed$value`
#'
#' `NOTE:` This function may merit deprecation, because [dplyr::rename_with()], coupled
#' with [lookup()], deals quite well with this use-case (and is more general).
#'
#' ```
#' d.enframed = tibble::enframe(c(a="New A", b="New B"))
#' ```
#'
#' @param d The `tibble` whose columns we want to rename.
#' @param d.enframed The `enframed tibble` with the old and new names.
#'
#' @return A tibble that is identical to `d` but with renamed columns.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr vars
#'
#' @md
#' @export
rename_enframed = function(d, d.enframed)
{
    # Some minor input checking
    if ( !("data.frame" %in% class(d))          ) stop("rename_enframed(): First argument must be data.frame")
    if ( !("data.frame" %in% class(d.enframed)) ) stop("rename_enframed(): Second argument must be data.frame")
    # Remove any extra values from d.enframed
    d.enframed = d.enframed %>%
        dplyr::filter(d.enframed$name %in% names(d))
    # Do the renaming of d
    result = d %>%
        dplyr::rename_at(dplyr::vars(d.enframed$name), ~d.enframed$value)
    return(result)
}


#' Get the variable labels of a `labelled tibble` in an `enframed tibble`
#'
#' @description
#' Given a data `tibble`/`data.frame`, this function will return a `tibble` in
#' `enframed` format (two columns named name and value, see [tibble::enframe()]
#' for details), where the name column contains the names of the variables in
#' the original `tibble` and the value column contains the labels.
#'
#' This can be used to review the labels, and could (if coupled with a
#' `set_labels_enframed()` function, which is not currently implemented) be used
#' to preserve labels across label-destructive operations, or export labels to a
#' file (or a sheet in an excel file).
#'
#' Finally, it could be used in conjunction with [zmisc::lookup()] and
#' [dplyr::rename_with()] in a pipe to rename variables in order to replace the
#' names of variables with their labels for display in a plot or table function
#' that does not support the display of labels in a simple way.
#'
#' @param d The `tibble` whose columns have variable labels.
#' @return A `tibble` in `enframed` format containing variable names and labels.
#'
#' @md
#' @export
get_labels_enframed = function(d)
{
    # This used to be called zt.codebook
    #
    # Alternative codebook functions
    #   memisc::codebook(...)
    #   codebook::codebook_table(..)

    # helper function
    get_single_label <- function(x){
        result <- attr(x,which="label", exact=TRUE)
        ifelse(is.null(result),NA,result)
    }

    v.names  = names(d)
    v.labels = unname(sapply(d, get_single_label))
    d.result = tibble::tibble(name=v.names,value=v.labels)
    return(d.result)
}
