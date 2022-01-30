#' Allocate discrete seats given continuous votes
#'
#' @description
#' The `dhondt()` function can be used to allocate a discrete number of seats
#' (or other resources) between parties according to a continuous number of
#' votes (or other claims on the resource), in a way that approximates
#' proportionality. Semi-proportionality is achieved with the D'Hondt method,
#' which is a commonly used approach in electoral allocation.
#'
#' Other implementations of D'Hondt allocation in `R` can be found in
#' [electoral::seats_ha()] and [coalitions::dHondt()]. The motivation behind
#' this implementation is to have a simple interface with reasonable defaults,
#' suitable for use in pipes operating on data in data.frames or tibbles. The
#' error handling is also more explicit here than in other implementations.
#' Finally, party names are optional, which is useful if there are no natural
#' names for the parties.
#'
#' @param votes A vector with the votes of different parties. The number of
#'   parties is determined based on the length of the vector. Votes can be given
#'   in whole or real numbers, and there is no requirement that they sum up to 1
#'   or 100.
#'
#' @param seats An integer giving the number of seats to allocate. Defaults to
#'   7, which is a common size of boards and other deliberative bodies (and a
#'   pretty nice number in general), but one would almost always want to specify
#'   this.
#'
#' @param ties A character variable specifying what to do in case of ties. The
#'   D'Hondt method does not specify any tie handling, so two options are
#'   allowed. Specifying `"error"` (the default) results in an error if there is
#'   a tie for the marginal allocated seat. Specifying `"first"` means that the
#'   party that comes first (in the `votes` vector) wins. Methods such as
#'   allocating randomly or favoring the largest party can be achieved by
#'   ordering the `votes` vector prior to calling the function.
#'
#' @return An integer vector with allocations of seats to parties, in the same
#'   order as the parties were specified in the `votes` vector. The length of
#'   the result vector always equals the length of the `votes` vector. The sum
#'   of the result vector always equals the value in `seats`. If the `votes`
#'   vector has names, the results vector has the same names.
#'
#' @examples
#' dhondt(c(13,11,7,5,3))
#' dhondt(c(liberals=4876, conservatives=5212, greens=2349), seats=21)
#'
#' @md
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom dplyr arrange mutate desc group_by summarize
#' @family functions related to voting
#' @rdname dhondt
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
  score_table <- tibble(
    parties = rep( parties, each = seats ),
    scores  = as.vector(sapply( votes, function(x) {x / zeq(1,seats)} ))
  ) %>%
    arrange(desc(.data$scores)) %>% # We assume arrange is stable so first party wins
    mutate(allocated=c(rep(1L,seats), rep(0L,(seats*length(votes)-seats))))

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
    group_by(.data$parties) %>%
    summarize(allocated=sum(.data$allocated), .groups="drop")

  # Construct and return result.
  # As promised, if votes has names, results will have them too
  result <- result_table$allocated
  names(result) <- names(votes)
  result
}



#' Vote allocation using largest remainder method
#'
#' @description
#' The `largest_remainder()` allocates votes in a way parallel to the
#' `dhondt()` function, but using the least remainder method, instead of a
#' greatest divisors method. An alternative implementation exists in
#' [electoral::seats_lr()], and possibly in [coalitions::hare_niemeyer()].
#' The rationale for implementation of this function is comparable to that for
#' `dhondt()`.
#'
#' `NOTE:` Tie handling has currently not been fully implemented for
#' `largest_remainder()`.
#'
#' @examples
#' largest_remainder(c(13,11,7,5,3))
#' largest_remainder(c(liberals=4876, conservatives=5212, greens=2349), seats=21)
#'
#' @rdname dhondt
#' @family functions related to voting
#' @md
#' @export
largest_remainder <- function(votes, seats=7, ties=c("undefined", "error","first")) {

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


  result_float <- seats*votes/sum(votes)
  result_base  <- floor(result_float)
  result_remainders <- result_float-result_base

  remaining_seats <- seats-sum(result_base)

  # https://stackoverflow.com/questions/17619782/how-to-find-the-largest-n-elements-in-a-list-in-r/17619825
  which_get_extra <- order(result_remainders, decreasing=TRUE)[1:remaining_seats]
  result <- result_base
  result[which_get_extra] = result[which_get_extra]+1

  # Return the result
  result

}
