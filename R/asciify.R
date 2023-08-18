#' Convert non-ASCII characters to their ASCII equivalents
#'
#' This function replaces non-ASCII characters in a string with their ASCII
#' equivalents. It supports a range of European non-ASCII characters, including
#' Icelandic, Swedish, Norwegian, Danish, Finnish, German, Estonian, Latvian,
#' Lithuanian, Polish, Hungarian, Slovenian, Czech, Slovak, Maltese, Romanian,
#' Albanian, and Croatian.
#'
#' @param x A character vector to be processed.
#'
#' @param verify A logical value indicating whether to verify that the result is
#'   ASCII. Defaults to `TRUE`. If `FALSE`, the function will not check that the
#'   result is ASCII and it may return non-ASCII characters.
#'
#' @return A character vector with non-ASCII characters replaced by their ASCII
#'   equivalents.
#'
#' @examples
#' asciify("Jón Þór Birgisson") # "Jon Thor Birgisson"
#' asciify("förståndshandikapp") # "forstandshandikapp"
#' asciify("Viðareiði") # "Vidareidi"
#' asciify("übermensch") # "uebermensch"
#' asciify("Jürgen Klopp") # "Juergen Klopp"
#' asciify("rõõmsameelsus") # "roomsameelsus"
#' asciify("Mężczyzna") # "Mezczyzna"
#' asciify("Škoda") # "Skoda"
#'
#' @md
#' @export
asciify <- function(x, verify = TRUE) {

  # Sanity
  checkmate::assert_character(x)
  checkmate::assert_atomic_vector(x)
  checkmate::assert_flag(verify)

  # Icelandic (and other European) non-ascii characters
  # The utf-8 version is escaped to satisfy R CMD check
  # org <-  c("Á", "á", "Ð", "ð", "É", "é", "Í", "í", "Ó", "ó", "Ú", "ú", "Ý",
  # "ý", "Þ", "þ", "Æ", "æ", "Ö", "ö", "Å", "å", "Ä", "ä", "Ø", "ø", "ß", "À",
  # "à", "Â", "â", "Ç", "ç", "È", "è", "Ê", "ê", "Î", "î", "Ï", "ï", "Ô", "ô",
  # "Œ", "œ", "Ù", "ù", "Û", "û", "Ü", "ü", "Ÿ", "ÿ", "Ñ", "ñ", "Ì", "ì", "Ò",
  # "ò", "Õ", "õ", "Ā", "ā", "Ē", "ē", "Ģ", "ģ", "Ī", "ī", "Ķ", "ķ", "Ļ", "ļ",
  # "Ņ", "ņ", "Ū", "ū", "Č", "č", "Š", "š", "Ž", "ž", "Ą", "ą", "Ę", "ę", "Ė",
  # "ė", "Į", "į", "Ų", "ų", "Ć", "ć", "Ł", "ł", "Ń", "ń", "Ś", "ś", "Ź", "ź",
  # "Ż", "ż", "Ő", "ő", "Ű", "ű", "Ď", "ď", "Ě", "ě", "Ň", "ň", "Ř", "ř", "Ť",
  # "ť", "Ů", "ů", "Ĺ", "ĺ", "Ľ", "ľ", "Ŕ", "ŕ", "Ċ", "ċ", "Ġ", "ġ", "Ħ", "ħ",
  # "Ă", "ă", "Ș", "ș", "Ț", "ț", "Ë", "ë", "Đ", "đ")
  org <-   c("\\u00c1", "\\u00e1", "\\u00d0", "\\u00f0", "\\u00c9", "\\u00e9",
  "\\u00cd", "\\u00ed", "\\u00d3", "\\u00f3", "\\u00da", "\\u00fa", "\\u00dd",
  "\\u00fd", "\\u00de", "\\u00fe", "\\u00c6", "\\u00e6", "\\u00d6", "\\u00f6",
  "\\u00c5", "\\u00e5", "\\u00c4", "\\u00e4", "\\u00d8", "\\u00f8", "\\u00df",
  "\\u00c0", "\\u00e0", "\\u00c2", "\\u00e2", "\\u00c7", "\\u00e7", "\\u00c8",
  "\\u00e8", "\\u00ca", "\\u00ea", "\\u00ce", "\\u00ee", "\\u00cf", "\\u00ef",
  "\\u00d4", "\\u00f4", "\\u0152", "\\u0153", "\\u00d9", "\\u00f9", "\\u00db",
  "\\u00fb", "\\u00dc", "\\u00fc", "\\u0178", "\\u00ff", "\\u00d1", "\\u00f1",
  "\\u00cc", "\\u00ec", "\\u00d2", "\\u00f2", "\\u00d5", "\\u00f5", "\\u0100",
  "\\u0101", "\\u0112", "\\u0113", "\\u0122", "\\u0123", "\\u012a", "\\u012b",
  "\\u0136", "\\u0137", "\\u013b", "\\u013c", "\\u0145", "\\u0146", "\\u016a",
  "\\u016b", "\\u010c", "\\u010d", "\\u0160", "\\u0161", "\\u017d", "\\u017e",
  "\\u0104", "\\u0105", "\\u0118", "\\u0119", "\\u0116", "\\u0117", "\\u012e",
  "\\u012f", "\\u0172", "\\u0173", "\\u0106", "\\u0107", "\\u0141", "\\u0142",
  "\\u0143", "\\u0144", "\\u015a", "\\u015b", "\\u0179", "\\u017a", "\\u017b",
  "\\u017c", "\\u0150", "\\u0151", "\\u0170", "\\u0171", "\\u010e", "\\u010f",
  "\\u011a", "\\u011b", "\\u0147", "\\u0148", "\\u0158", "\\u0159", "\\u0164",
  "\\u0165", "\\u016e", "\\u016f", "\\u0139", "\\u013a", "\\u013d", "\\u013e",
  "\\u0154", "\\u0155", "\\u010a", "\\u010b", "\\u0120", "\\u0121", "\\u0126",
  "\\u0127", "\\u0102", "\\u0103", "\\u0218", "\\u0219", "\\u021a", "\\u021b",
  "\\u00cb", "\\u00eb", "\\u0110", "\\u0111") |> stringi::stri_unescape_unicode()


  # The most appropriate ascii representations of the non-ascii characters
  ascii <- c("A", "a", "D", "d", "E", "e", "I", "i", "O", "o", "U", "u", "Y",
  "y", "Th", "th", "Ae", "ae", "O", "o", "A", "a", "A", "a", "O", "o", "ss",
  "A", "a", "A", "a", "C", "c", "E", "e", "E", "e", "I", "i", "I", "i", "O",
  "o", "OE", "oe", "U", "u", "U", "u", "U", "u", "Y", "y", "N", "n", "I", "i",
  "O", "o", "O", "o", "A", "a", "E", "e", "G", "g", "I", "i", "K", "k", "L",
  "l", "N", "n", "U", "u", "C", "c", "S", "s", "Z", "z", "A", "a", "E", "e",
  "E", "e", "I", "i", "U", "u", "C", "c", "L", "l", "N", "n", "S", "s", "Z",
  "z", "Z", "z", "O", "o", "U", "u", "D", "d", "E", "e", "N", "n", "R", "r",
  "T", "t", "U", "u", "L", "l", "L", "l", "R", "r", "C", "c", "G", "g", "H",
  "h", "A", "a", "S", "s", "T", "t", "E", "e", "D", "d")


  # Replace  Icelandic (and other European) non-ascii characters with ascii representations
  # This call is fully - and independently - vectorized in x and then in x/ascii
  result <- stringi::stri_replace_all_fixed(x, org, ascii, vectorize_all = FALSE)

  # Verify the result
  if (verify) {
    max_byte <- result |> stringr::str_c(collapse = "") |> charToRaw() |> as.numeric() |> max()
    if (max_byte >= 128) {
      stop( stringr::str_c(
          "The result of asciify() still contains non-ascii characters. \n",
          "  The input probably contains characters that are not handled \n",
          "  by the function. Use 'verify = FALSE' if this is what you want."
      ) )
    }
  }

  # Return the result
  result
}
