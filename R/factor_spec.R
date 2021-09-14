

#' Construct code to use as col_spec for factors
#'
#' @description
#' This function examines all character columns in a data.frame (typically one that
#' was read from a csv file using `readdr::read_csv()`, and generates
#' a specification suitable for reading those columns from the underlying file
#' as factors.
#'
#' The key benefit is that it will find all variables that are using the same
#' factor levels and group them together, so editing the col_spec to reorder
#' factor levels or make other changes is straightforward.
#'
#' Because the string contains quotation marks, it can be useful to pipe the
#' output directly into cat(), and then copy the output into a source file
#' before using it. The output can then be edited before the colspec
#' is used to read the data in fresh from the CSV file. Assuming the `cols()`
#' result is assigned to a variable `cspec`, one might have the following.
#'
#' ```
#' d <- read_csv(csv_file)
#' factor_spec(d) |> cat()
#' # edit the output to fit ones needs
#' cspec <- cols(...)
#' d <- read_csv(csv_file, col_types=cspec)
#' ```
#'
#' @param   d The data frame to use when generating the col_spec
#' @return    A string with definitions suitable for defining a col_spec,
#'            after any needed editing to reorder factors.
#' @md
#' @export
factor_spec <- function(d) {

  d <- dplyr::select(d, where(is.character))
  all_spec       <- lapply(d, function(x){sort(unique(x))})#  %>% print()
  unique_spec    <- unique(all_spec)# %>% print()
  srt_levelcount <- sapply(unique_spec,length) # Number of levels in this spec
  srt_varcount   <- # Number of vars using this spec (ugly)
    sapply(unique_spec, function(x, y_list){sum(y_list %in% list(x))}, all_spec)

  # We order descending by number of variables using the spec,
  # and then increasing by the number of levels in the spec
  unique_spec   <- unique_spec[order(-srt_varcount, srt_levelcount)]

  level_conn <- textConnection("level_string", "w")
  spec_conn  <- textConnection("spec_string", "w")
  # Write each unique factor level to variable
  for (i in zeq(1,length(unique_spec))) {

    # This code prepares the level variables
    cat(paste0("factor_levels_", i, " = "), file=level_conn)
    dput(unique_spec[[i]], file=level_conn) # dput() appends newline

    # This code prepares the spec, using the above variablesl
    #   Ugly way to find a list of all names using a particular
    #   set of levels (corresponding to the current unique_spec)
    #   cur_specs can be longer than one!
    cur_specs = names(all_spec)[all_spec %in% list(unique_spec[[i]])]
    cur_specdefs = paste0("  ", cur_specs, " = col_factor(factor_levels_", i, "),\n" )
    cat(cur_specdefs, file=spec_conn, sep="")
  }

  close(level_conn)
  close(spec_conn)

  # Remove the last comma, and encapsulate in a cols() closure
  spec_string[length(spec_string)] <- sub(",","",spec_string[length(spec_string)])
  spec_string <- c("cols(",spec_string,")")

  # Calculate and return the result
  result <- paste0(c(level_string, "",spec_string), "\n", collapse="")
  result
}
