
#' A lookup table for the phonetic alphabet
#'
#' A dataset containing the letters of the alphabet and their
#' representaion in the phonetic alphabet (alpha, bravo, charlie,
#' ..., zulu). The data is formatted as an `enframed tibble`
#' (see [tibble::enframe()]), suitable for use in contexts such
#' as the [lookup()] function.
#'
#' @format A `tibble` (data frame) with 26 rows and 2 variables:
#' \describe{
#'   \item{name}{letters of the alphabet}
#'   \item{value}{phonetic representation of the letters}
#' }
#' @md
"zulu_letters"
