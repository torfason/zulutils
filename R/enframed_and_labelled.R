
##
## Functions for working with enframed and/or labelled tibbles
##

#' Lookup values from a lookup table
#'
#' This function implements lookup of certain strings (such as
#' variable names) from an lookup table which maps keys
#' onto values (such as variable descriptions). Original values
#' are returned if they are not found in the lookup table.
#'
#' The lookup table can be in the form of an enframed data.frame,
#' in the form of a named vector, or in the form of a list.
#'
#' Any names in x are not included in the result.
#'
#' @param x            A string vector whose elements shall be looked up
#' @param lookup_table The lookup table to use. If the table is in the form of
#'                     a data.frame, the lookup columns should be named
#'                     \code{name} (for the key) and \code{value} (for the value).
#'                     If the lookup table is in the form of a named vector or list,
#'                     the name is used for the key, and the returned value is
#'                     taken from the values in the vector or list.
#' @return             A string vector based on \code{x}, with values replaced
#'                       with the lookup values from \code{d.enframed}. Any values
#'                   not found in the lookup table are returned unchanged.
#' @importFrom dplyr %>%
#' @export
lookup = function(x, lookup_table)
{
  if ( is.data.frame(lookup_table) ) {
    stopifnot(
      "name"  %in% names(lookup_table),
      "value" %in% names(lookup_table)
    )
    lookup_table <- tibble::deframe(lookup_table[,c("name","value")] )
  }
  if ( is.vector(lookup_table) && !is.list(lookup_table)) {
    stopifnot( is.character(lookup_table) )
    lookup_table <- as.list(lookup_table)
  }
  stopifnot( is.list(lookup_table) )
  result = lookup_table[x]
  names(result) = NULL
  not.found = unlist(lapply(result, is.null))
  result[not.found] = x[not.found]
  unlist(result)
}


#' @rdname zulutils-deprecated
#' @section \code{lookup_enframed}:
#' \code{lookup_enframed()} is deprecated in favor of \code{\link{lookup()}}.
#'
#' @export
lookup_enframed <- function(x, lookup_table) {
  .Deprecated("lookup", msg="lookup_enframed() is deprecated in favor of lookup().")
  lookup(x, lookup_table)
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
