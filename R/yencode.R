
#' Yet (another urlencode compatible) encoding scheme
#'
#' @description
#' Encode and decode using an encoding scheme that is a superset of the
#' [utils::URLencode()] encoding. With default settings, `yencode()` and
#' `ydecode()` produce strings that are fully compatible with `urlencode`
#' encoded strings. However, these functions allow a custom whitelist of symbols
#' that should not be escaped by the encoding process, and a configurable escape
#' character to use in place of the `%` symbol, for example to work with storage
#' layers that do not like the `%` symbol.
#'
#' The `yencoder()` and `ydecoder()` functions are convenience function, which
#' return the corresponding a function with the escape and whitelist already
#' set, allowing easy use in contexts that expect a single-argument function.
#'
#' @details
#' In addition to the supplied white-list, `A-Z`, `a-z`, and `0-9` are always
#' white-listed. There are no restrictions on the white-list, except that the
#' escape character must not be part of it (and will be removed from it with a
#' warning). Of course, it is important that the underlying storage layer
#' handles all white-listed characters gracefully.
#'
#' Note that any `ascii` letter or number will work perfectly fine as an escape
#' character, the output will be well-formed and decoded correctly, even if some
#' of them, such as `1` will result in escape sequences that contain the letter
#' itself.
#'
#' In particular, `yencoder("Z", whitelist="")` returns a encoder that will
#' encode any string to a pure `A-Z`, `a-z`, and `0-9` representation, suitable
#' for extremely limited storage layers (it will encode `Z` as `Z5A`).
#'
#' @param string The string to process.
#' @param escape The escape character to use.
#' @param whitelist Any characters that should not be escaped. See details.
#' @return The processed (encoded or decoded) string.
#'
#' @md
#' @export
yencode <- function(string, escape="%", whitelist=c("._~-", "][!$&'()*+,;=:/?@#")) {

  # The following characters are always whitelisted and cannot be escaped
  whitelist_core  <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  whitelist_final <- paste0(whitelist_core, paste0(whitelist, collapse=""))

  # The escape string must be one single-byte character
  stopifnot( is.character(escape)           ,
             length(escape) == 1            ,
             length(charToRaw(escape)) == 1 )

  # Treat NULL or NA whitelist as empty string, but other types should error
  if (is.null(whitelist)) { whitelist <- "" }
  whitelist[is.na(whitelist)] <- ""
  stopifnot(is.character(whitelist))

  # The escape character must NOT be in the final whitelist
  if (stringr::str_detect(whitelist_final, stringr::fixed(escape))) {
    warning(paste0("The escape character (", escape, ") cannot be present in the whitelist,\n",
                   "and has been removed. The results are, strictly speaking, well-specified\n",
                   "and robust, but you should be sure you know what you are doing."))
    whitelist_final <- stringr::str_remove(whitelist_final, stringr::fixed(escape))
  }

  # Paste whitelists together, then remove duplicates, and escape any special chars
  wl <- whitelist_final |>
    stringr::str_split("") |>
    unlist() |>
    # The following could probably be vectorized in pattern to optimize replacement
    stringr::str_replace("\\/","\\\\/") |>
    stringr::str_replace("\\-","\\\\-") |>
    stringr::str_replace("\\]","\\\\]") |>
    stringr::str_replace("\\[","\\\\[") |>
    paste0(collapse = "")
  # Some defensive programming, the above should result in character of length 1
  stopifnot(length(wl) == 1)

  vapply(string, function(string) {
    OK <- paste0("[^", wl, "]")
    x <- strsplit(string, "")[[1L]]
    #print(x)
    #z <- grep(OK, x) # base R regexp syntax not standard?
    z <- stringr::str_detect(x,OK)
    #print(z)
    if (length(z)) {
        y <- vapply(x[z], function(x) paste0(escape, toupper(as.character(charToRaw(x))),
            collapse = ""), "")
        x[z] <- y
    }
    paste(x, collapse = "")
}, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' @rdname yencode
#' @md
#' @export
yencoder <- function(escape="%", whitelist=c("._~-", "][!$&'()*+,;=:/?@#")) {
  # We trigger the "escape in whitelist" warning once on creation, but
  # then suppress that specific warning in the subsequent calls to
  # the encoder function
  yencode("", escape, whitelist)
  function(string) {
    suppress_warnings(
      yencode(string, escape, whitelist),
      "The escape character .* cannot be present in the whitelist")
  }
}

#' @rdname yencode
#' @md
#' @export
ydecode <- function (string, escape = "%")
{
    vapply(string, function(string) {
        x <- charToRaw(string)
        pc <- charToRaw(escape)
        out <- raw(0L)
        i <- 1L
        while (i <= length(x)) {
            if (x[i] != pc) {
                out <- c(out, x[i])
                i <- i + 1L
            }
            else {
                y <- as.integer(x[i + 1L:2L])
                y[y > 96L] <- y[y > 96L] - 32L
                y[y > 57L] <- y[y > 57L] - 7L
                y <- sum((y - 48L) * c(16L, 1L))
                out <- c(out, as.raw(as.character(y)))
                i <- i + 3L
            }
        }
        rawToChar(out)
    }, character(1), USE.NAMES = FALSE)
}

#' @rdname yencode
#' @md
#' @export
ydecoder <- function(escape="%") {
  function(string) {
    ydecode(string, escape)
  }
}


#' Suppress warnings that match specific regular expressions
#'
#' Based on [this stack overflow answer](https://stackoverflow.com/questions/16517795/selective-suppresswarnings-that-filters-by-regular-expression/55182432#55182432),
#' except it does not allow function arguments, only regular expressions. This removes the dependency on non-base-R packages.
#'
#' @md
#' @keywords internal
suppress_warnings <- function(expr, regexp) {
  eval.parent(substitute(
  withCallingHandlers( expr, warning = function(w) {
    cm <- conditionMessage(w)
    cond <- grepl(regexp, cm)
    if (cond) {
      invokeRestart("muffleWarning")
    }
  })
  ))
}
