
# https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables("where")

# R CMD check static checking does not understand textConnection receivers
utils::globalVariables("level_string")
utils::globalVariables("spec_string")

#' Construct code to use as `col_spec` for factors
#'
#' @description
#' This function examines all character columns in a data.frame (typically one
#' that was read from comma separated text file using a reader function such as
#' `readdr::read_csv()`), and generates a specification suitable for reading
#' those columns from the underlying file as factors.
#'
#' The key benefit is that it will find all variables that are using the same
#' factor levels and group them together, so editing the `col_spec` to reorder
#' factor levels or make other changes is straightforward.
#'
#' The result is returned as a `catty` vector to provide a more readable output
#' by default.
#'
#' The output can then be edited before the `col_spec` is used to read the data
#' in fresh from the CSV file. Assuming the `cols()` result is assigned to a
#' variable `cspec`, one might have the following:
#'
#' ```
#' d <- read_csv(csv_file)
#' cb_as_col_spec_factors(d)
#' # edit the output to fit your needs
#' cspec <- cols(...)
#' d <- read_csv(csv_file, col_types=cspec)
#' ```
#'
#' @param d The `data.frame` to use when generating the `col_spec`.
#' @return A `catty` string with column definitions, which is suitable for
#'   defining a `col_spec`, after any needed editing to reorder factors.
#'
#' @importFrom dplyr select
#' @importFrom zmisc zeq
#'
#' @md
#' @export
cb_as_col_spec_factors <- function(d) {

  d <- select(d, where(is.character))
  all_spec       <- lapply(d, function(x) { sort(unique(x)) }) # |> print()
  unique_spec    <- unique(all_spec)                           # |> print()
  srt_levelcount <- sapply(unique_spec, length)  # Number of levels in this spec
  srt_varcount   <-                              # Number of vars using this spec (ugly)
    sapply(unique_spec, function(x, y_list){sum(y_list %in% list(x))}, all_spec)

  # Order descending by number of variables using the spec,
  # and then increasing by the number of levels in the spec
  unique_spec <- unique_spec[order(-srt_varcount, srt_levelcount)]

  # Define connections (and the receiving variables to please R CMD check)
  level_conn <- textConnection("level_string", "w")
  spec_conn  <- textConnection("spec_string", "w")

  # Write each unique factor level to variable
  for (i in zeq(1, length(unique_spec))) {

    # This code prepares the level variables
    cat(paste0("factor_levels_", i, " = "), file = level_conn)
    #dput(unique_spec[[i]], file=level_conn) # dput() appends newline
    cat(deparse1(unique_spec[[i]]), "\n", file = level_conn, sep = "") # cat does not append a newline, so we do it ourselves

    # This code prepares the spec, using the above variables
    #   Ugly way to find a list of all names using a particular
    #   set of levels (corresponding to the current unique_spec)
    #   cur_specs can be longer than one!
    cur_specs = names(all_spec)[all_spec %in% list(unique_spec[[i]])]
    cur_specdefs = paste0("  ", cur_specs, " = col_factor(factor_levels_", i, "),\n")
    cat(cur_specdefs, file = spec_conn, sep = "")
  }

  close(level_conn)
  close(spec_conn)

  # Remove the last comma, and encapsulate in a cols() closure
  spec_string[length(spec_string)] <- sub(",", "", spec_string[length(spec_string)])
  spec_string <- c("cols(", spec_string, ")")

  # Calculate and return the result
  result <- paste0(c(level_string, "", spec_string), "\n", collapse = "")

  # We return the result as a catty object, so that it prints better
  # We first split it into a vector with each line as a separate element,
  # to facilitate processing it with code if desired.
  # One of several ugly aspects of this function.
  result |>
    strsplit("\n") |>
    unlist() |>
    catty("\n")
}


#' Convert column types according to a col_spec
#'
#' Custom wrapper around `readr::type_convert()` that suppresses warnings and
#' copies labels, in order to (mostly) match the behavior of
#' `cb_apply_col_spec()`. but without re-implementing a lot of functionality.
#'
#' A known key difference compared `cb_apply_col_spec()` is that `df` is fully
#' processed according to the `col_types` argument, meaning that other columns
#' may be converted in addition to the factor columns.
#'
#' @param df The `data.frame` to process.
#' @param col_types The `col_spec` to apply.
#' @param ... Other arguments to `readr::type_convert()`.
#' @return A `data.frame` based on `d`, with any factor columns specified in
#'   `col_types` converted to `factor` *and* any other columns processed
#'   according to the functionality of `readr::type_convert()`
#'
#' @md
#' @export
#'
type_convert_with_labels <- function(df, col_types = NULL, ...) {
  readr::type_convert(df, col_types, ...) |>
    suppressWarnings() |>
    labelled::copy_labels_from(df)
}


#' Apply a col_spec to a data.frame
#'
#' @description
#' `cb_apply_col_spec()` applies column definitions contained in a `col_spec`
#' object (typically created using [readr::cols()]), to the chosen columns in a
#' `data.frame`.
#'
#' NOTE: This is currently only implemented for column specifications specifying
#' conversion to a factor variable.
#'
#' @param d The `data.frame` to which the `col_spec` should be applied.
#' @param cspec The `col_spec` to apply.
#' @param warn_missing_levels Warn if any values in `d` don't match the levels
#'   of the corresponding factor.
#' @param set_spec_attribute Should the `spec` attribute of the result be set to
#'   the `col_spec` (for compatibility with `readr` behavior).
#' @param set_problems_attribute Should the `problems` attributes of the result
#'   be set in case of problems with parsing.
#' @return A `data.frame` based on `d`, with corresponding columns changed
#'   according to the values specified in `cspec`
#'
#' @md
#' @importFrom purrr imap
#' @importFrom readr parse_factor
#' @importFrom dplyr mutate_all
#' @importFrom labelled copy_labels
#' @export
cb_apply_col_spec <- function(d, cspec,
      warn_missing_levels = TRUE,
      set_spec_attribute = FALSE,
      set_problems_attribute = TRUE ) {

  # Store the original for label copying
  d.org <- d

  # A bit of input checking
  if (!all(inherits(d, "data.frame"), inherits(cspec, "col_spec"),
           is.logical(set_spec_attribute), is.logical(set_spec_attribute))) {
    stop("apply_col_spec(): wrong input types")
  }
  if (!all(sapply(cspec$cols, inherits, "collector_factor"))) {
    stop("apply_col_spec(): only implemented for factor columns")
  }

  # Do the actual application of the col_spec
  if (warn_missing_levels) {
    # parse_factor() creates elaborate warnings on missing levels,
    # but no option to turn it off. It also sets the "problems"
    # attribute on the resulting data.frame
    d[names(cspec$cols)] <- imap(cspec$cols, ~ parse_factor(d[[.y]],
       levels = .x$levels, ordered = .x$ordered,
       include_na = .x$include_na))
  } else {
    # factor() just silently ignores missing levels
    d[names(cspec$cols)] <- imap(cspec$cols, ~ factor(d[[.y]],
       levels = .x$levels, ordered = .x$ordered,
       exclude=ifelse(.x$include_na, NULL, NA)))
  }

  # If requested, set col_spec as an attribute, for consistency with readr
  if (set_spec_attribute) {
    attr(d, "spec") <- cspec
  }

  # Unless specifically requested, any "problems" attributes should be cleared
  # from the result
  if (!set_problems_attribute) {
    attr(d, "problems") <- NULL
    d <- mutate_all(d, function(x){attr(x, "problems") <- NULL; x})
  }

  # Any labels from the original should be copied to the result
  d <- copy_labels(from = d.org, to = d)

  d
}
