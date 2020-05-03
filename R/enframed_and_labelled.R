
#' Lookup values from a two-column tibble
#'
#' This function implements lookup of certain strings (such as
#' variable names) from an (enframed) tibble which maps keys
#' onto values (such as variable descriptions). Original values
#' are returned if they are not found in the lookup tibble.
#' Any names in x are not included in the result.
#'
#' @param x          A string vector whose elements shall be looked up
#' @param d.enframed The (enframed) tibble to use as a lookup table. The
#'                   lookup columns should be named \code{name} and \code{value}.
#' @return           A string vector based on \code{x}, with values replaced
#'                   with the lookup values from \code{d.enframed}. Any values
#'                   not found in the lookup table are returned unchanged.
#' @importFrom dplyr %>%
#' @export
lookup_enframed = function(x, d.enframed)
{
    d = d.enframed %>% dplyr::select(name,value)
    l.lookup = d %>% tibble::deframe() %>% as.list()
    result = l.lookup[x]
    names(result) = NULL
    not.found = unlist(lapply(result, is.null))
    result[not.found] = x[not.found]
    return(unlist(result))
}


#' Rename tibble columns according to values in another tibble.
#'
#' Renames variables from d according to the columns
#' name (rename from) and value (rename to) in d.enframed.
#' The expectation is that d.framed has been created
#' with \code{enframe()}, such as
#' \code{d.framed = enframe(c(a="New A", b="New B"))}.
#'
#' @param d          The tibble whose columns we want to rename
#' @param d.enframed The (enframed) tibble with the old and new names
#' @return           A tibble that is identical to \code{d} but with renamed columns
#' @importFrom dplyr %>%
#' @importFrom dplyr vars
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

#' Get all the variable labels of a labelled tibble in an enframed tibble
#'
#' Given a data tibble/data.frame, this function will return a tibble in
#' enframed format (two columns named name and value), where the name
#' column contains the names of the variables in the original tibble and
#' the value column contains the labels. This can be used to review the
#' labels, and (if coupled with a set_labels_enframed function) could be
#' used to preserve labels across label-destructive operations, or export
#' labels to a file (or a sheet in an excel file). Finally, it could be
#' used with rename_enframed() in a pipe to replace the names of variables
#' with their labels for display in a plot or table function that does not
#' support the display of labels in a simple way.
#'
#' @param d          The tibble whose columns have variable labels
#' @return           A tibble in enframed format containing variable names and labels
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
