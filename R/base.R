
##
## Default source file for functions in the zulutils package
##

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
    # Bail out of sampling from data.frames, use dplyr::sample_X() for that
    if (class(x) == "data.frame")
    {
        stop("zulutils::zample() does not support data.frames")
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
#' @usage na_replace(x, replace)
#'
#' @param x Vector possibly contining missing (NA) values.
#' @param replace Scalar replacement value
#' @return Copy of x with any missing values replaced with replace
#'
#' @family na_functions
#'
#' @export
na_replace = function (x, replace)
{
    x[is.na(x)] <- replace
    x
}

#' @rdname zulutils-deprecated
#' @section \code{na.replace}:
#' \code{na.replace()} is deprecated in favor of \code{\link{na_replace()}}.
#'
#' @export
na.replace <- function(x, replace) {
  .Deprecated("na_replace", msg="na.replace() is deprecated in favor of na_replace().")
  na_replace(x, replace)
}

#' Replace values in x that are contained in y with NA
#'
#' This function is a variant/extension of dplyr::na_if()
#'
#' @family na_functions
#'
#' @export
na_if_in <- function(x,y)
{
    x[x %in% y] <- NA
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

#' Allocate discrete seats given continuous votes
#'
#' @description
#' Allocate a discrete number of seats (or other resources) between
#' parties according to a continuous number of votes (or other claims
#' on the resource), in a way that approximates proportionality.
#' Semi-proportionality is achieved with the D'Hondt method, which is
#' a commonly used approach in electoral allocation.
#'
#' Other implementations of D'Hondt allocation in \code{R} can be found
#' in \code{electoral::seats_ha()} and \code{coalitions::dHondt()}. The
#' motivation behind this implementation is to have a simple interface
#' with reasonable defaults, suitable for use in pipes operating on
#' data in data.frames or tibbles. Party names are also optional,
#' which is useful if there are no natural names for the parties.
#'
#' @param votes A vector with the votes of different parties. The
#'   number of parties is determined based on the length of the vector.
#'   Votes can be given in whole or real numbers, and there is no
#'   requirement that they sum up to 1 or 100.
#'
#' @param seats An integer giving the number of seats to allocate.
#'   Defaults to 7, which is a common size of boards and other
#'   deliberative bodies (and a pretty nice number in general),
#'   but one would almost always want to specify this.
#'
#' @param ties A character variable specifying what to do in case of
#'   ties. The D'Hondt method does not specify any tie handling, so
#'   two options are allowed. Specifying \code{"error"} (the default)
#'   results in an error if there is a tie for the marginal allocated
#'   seat. Specifying \code{"first"} means that the party that comes
#'   first (in the \code{votes} vector) wins. Methods such as
#'   allocating randomly or favoring the largest party can be achived
#'   by ordering the \code{votes} vector prior to calling the function.
#'
#' @return An integer vector with allocations of seats to parties, in
#'   the same order as the parties were specified in the \code{votes}
#'   vector. The length of the result vector always equals the length
#'   of the \code{votes} vector. The sum of the result vector always
#'   equals the value in \code{seats}. If the \code{votes} vector
#'   has names, the results vector has the same names.
#'
#' @examples
#' dhondt(c(13,11,7,5,3))
#' dhondt(c(liberals=4876, conservatives=5212, greens=2349), seats=21)
#'
#' @export
dhondt <- function(votes, seats=7, ties=c("error","first")) {

  # Defend against bad input
  stopifnot(is.numeric(votes) && length(votes)>=1 && all(votes>=0))
  stopifnot(is.numeric(seats) && length(seats)==1 && seats>=0 && seats==round(seats))
  ties <- match.arg(ties)

  # Let's just handle zero seats separately
  if (seats==0) {
    result <- rep(0,length(votes))
    names(result) <- names(votes)
    return(result)
  }


  # Prepare a tibble with the results
  parties <- seq_along(votes)
  score_table <- tibble::tibble(
    parties = rep( parties, each = seats ),
    scores  = as.vector(sapply( votes, function(x) {x / zeq(1,seats)} ))
  ) %>%
    dplyr::arrange(desc(scores)) %>% # We assume arrange is stable so first party wins
    dplyr::mutate(allocated=c(rep(1L,seats), rep(0L,(seats*length(votes)-seats))))

  # Handle ties
  score_last_in   <- score_table$scores[seats]
  score_first_out <- score_table$scores[seats+1]
  if (isTRUE(all.equal(score_last_in,score_first_out))) {
    # If errors on ties are specified, we bail out, otherwise
    # we proceed to check for dirty ties.
    if (ties == "error") {
      stop(paste0( "A tie occured in dhondt() allocation.\n",
                   "  To let the function resolve ties, set ties='first'\n",
                   "  (but be sure that this is what you want."))
    }

    if (score_last_in != score_first_out) {
      # We have a dirty tie, very close scores but not absolutely equal, so
      # we cannot trust sorting to be stable on the ties.
      warning(paste0( "A very-near-tie occured in dhondt() allocation.\n",
                      "  The most likely cause is a rounding error, which means that the\n",
                      "  ordering used to resolve ties cannot be trusted. You may want to\n",
                      "  double-check your results. Also, if you are able to share the data\n",
                      "  that caused this warning with the creator of this package, it would\n",
                      "  be greatly appreciated." ))
    }
  }

  # Aggregate
  result_table <- score_table %>%
    dplyr::group_by(parties) %>%
    dplyr::summarize(allocated=sum(allocated), .groups="drop")

  # Construct and return result.
  # As promised, if votes has names, results will have them too
  result <- result_table$allocated
  names(result) <- names(votes)
  result
}

