


#' Find all primes up to a given positive integer
#'
#' This function implements the Sieve of Eratosthenes algorithm to find all
#' prime numbers up to the given positive integer `n`. It returns a vector of
#' integers representing the prime numbers.
#'
#' @param n A positive integer specifying the upper limit for the prime numbers.
#'
#' @return A numeric vector of prime numbers up to `n`.
#'
#' @examples
#' zulutils:::sieve_primes(10)
#' zulutils:::sieve_primes(20)
#'
#' @md
#' @keywords internal
sieve_primes <- function(n)
{
   if(n > 1e9) stop("n too large")
   n <- as.integer(n)
   primes <- rep(TRUE, n)
   primes[1] <- FALSE
   last.prime <- 2L
   fsqr <- floor(sqrt(n))
   while (last.prime <= fsqr)
   {
      primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
      sel <- which(primes[(last.prime+1):(fsqr+1)])
      if(any(sel)){
        last.prime <- last.prime + min(sel)
      }else last.prime <- fsqr+1
   }
   which(primes)
}

#'
#' Calculate and return the nth prime number
#'
#' This function will use the `primes` package unless it either is missing, or
#' if options("zulutils.zerodeps") has been explicitly set to false, in which
#' case it will use a local implementation of the sieve of Erastosthenes.
#'
#' @param n the position of the prime number to be calculated
#' @return the nth prime number
#' @examples
#' nth_prime(1) # 2
#' nth_prime(10) # 29
#' nth_prime(100) # 541
#' nth_prime(1000) # 7919
#' nth_prime(10000) # 104729
#'
#' @md
#' @export
nth_prime <- function(n) {
  if (n<2) {
    2
  } else if (!isFALSE(getOption("zulutils.zerodeps")) & requireNamespace("primes")) {
    # generate_n_primes() generates exactly n first primes
    primes::generate_n_primes(n)[n]
  } else {
    # The local sieve is passed an scaled value to ensure it generates enough primes
    sieve_primes(ceiling(20*(n^1.3)/log(n)))[n]
  }
}

