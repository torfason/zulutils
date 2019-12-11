
# Welcome to the zulutils package
#
# This is the base soure file for the zulutils package
#
# Reference for package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
# Roxygen documentation:
#
#   https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html


dummy = function()
{
    unknownpackage::somefunction()
}

# It would also be useful to add psum, pprod and pmean


#' Return a single argument unchanged
#'
#' It can be useful (when developing pipelines for example),
#' to have access to a function that simply returns the element
#' that it receives. This is that function.
#'
#' @param x The single argument to (not) process
#' @return The argument \code{x} is returned unchanged
#' @export
noop = function(x)
{
    x
}


#' Generate sequence in a safe way
#'
#' This function creates an increasing integer sequence, but differs from
#' the standard one in that it will not silently generate a decreasing
#' sequence when the second argument is smaller than the first.
#' If the second arg is one smaller than the first it will generate
#' an empty sequence, if the difference is greater, the function will
#' error out.
#'
#' @param from The lower bound of the sequence
#' @param to   The higher bound of the sequence
#' @return     A sequence ranging from \code{from} to \code{to}
#' @export
zeq = function(from, to)
{
    stopifnot ( round(from) == from )
    stopifnot ( round(to)   == to   )
    stopifnot ( to >= from - 1      )
    return (seq_len(1+to-from)+from-1)
}


#' Sample from a vector or data.frame in a safe way
#'
#' This function duplicates the functionality of sample(), with the exception
#' that it does not attempt the (rather silly in my opinion) user-friendliness
#' of switching the interpretation of the first element to a number if the
#' length of the vector is 1.  To sample from an interval between 1 and n,
#' simply use zample(1:n) (or use sample(n) instead)
#'
#' @param x       The vector to sample from
#' @param size    The number of elements to sample from \code{x} (defaults to \code{length(x)})
#' @param replace Should elements be replaced after sampling (defaults to \code{false})
#' @export
zample = function (x, size=length(x), replace = FALSE, prob = NULL)
{
    # Special handling to allow sampling from a data.frame
    if (class(x) == "data.frame")
    {
        size = ifelse( is.null(size), nrow(x), size )
        return( x[ zample( zeq(1,nrow(x)), size, replace, prob ), ] )
    }

    # Sampling zero elements from a zero length vector should be allowed
    if ( size==0 & length(x)==0 ) { return(vector(mode=mode(x))) }

    # The code from original sample(), minus the silly numeric handling
    x[.Internal(sample(length(x), size, replace, prob))]
}


#' Return the single (unique) value found in a vector
#'
#' Returns the first element in a vector, but only if all the other
#' elements are identical to the first one (the vector only has a
#' zingle value). If the elements are not all identical, it throws
#' an error. The vector must contain at least one non-na value,
#' or the function errors out as well.
#'
#' This is a useful function in aggregations when
#' all values in a given group should be identical, but
#' you want to make sure.
#'
#' Optionally takes a na.rm parameter, similarly to sum, mean and
#' other aggregate functions.
#'
#' @export
zingle = function(x, na.rm = FALSE)
{
    if (na.rm) x = x[!is.na(x)]
    stopifnot(all(x[1]==x))
    return(x[1])
}

#' Replace missing values
#'
#' Usage: na.replace(x, replace)
#' Arguments
#'
#' x : vector possibly contining missing (NA) values.
#' replace : scalar replacement value
#'
#' @export
na.replace = function (x, replace)
{
    x[is.na(x)] <- replace
    x
}

#' Left pad string with whitespace or other characters
#'
#' Left pad a string with whitespace or other characters
#' in order to get a string of a certain length.
#' This function is a thin wrapper around stringr::str_pad()
#'
#' @export
lpad = function(string, width, pad=" ")
{
    stringr::str_pad(string, width, "left", pad)
}

#' Right pad string with whitespace or other characters
#'
#' Right pad a string with whitespace or other characters
#' in order to get a string of a certain length.
#' This function is a thin wrapper around stringr::str_pad()
#'
#' @export
rpad = function(string, width, pad=" ")
{
    stringr::str_pad(string, width, "right", pad)
}

#' Boolean grep function
#'
#' This function looks for pattern in each of the elements
#' of string, returning TRUE for each element that contains
#' pattern, and FALSE for the other elements. The argument
#' order follows the conventions of the stringr package,
#' and the underlying matching is done using stringr::str_extract()
#'
#' @param string The (possibly vectorized) string to process
#' @param pattern The (possibly vectorized) regexp pattern to use
#' @export
bgrep = function(string, pattern)
{
    v.match = stringr::str_extract(string, pattern)
    return(!is.na(v.match))
}


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
#' name (rename from) and value (rename to) in d.framed.
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
    # Remove any extra values from d.framed
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
    # An alternative codebook function is in memisc:
    # memisc::codebook(...)

    v.names  = names(d)
    v.labels = sapply( d, attr, which="label" )
    d.result = tibble(name=v.names,value=v.labels)
    return(d.result)
}
