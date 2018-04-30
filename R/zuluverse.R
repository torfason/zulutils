# Zuluverse
#
# A collection of utility functions applicable to the tidyverse.
# Hopefully this will remain a pretty small collection.
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

col_name <- function(x, default = abort("Please supply column name")) {
    if (identical(x, quote(expr = ))) return(default)
    switch_type(x,
                NULL = NULL,
                string = x,
                symbol = as_string(x),
                abort("Invalid column specification")
    )
}

# https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    condition[is.na(condition)] = FALSE
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}

codebook <- function(...)
{
    memisc::codebook(...)
}

zt.codebook = function(data)
{
    v.names  = names(data)
    v.labels = sapply( d, attr, which="label" )
    d.codebook = tibble(Name=v.names,Label=v.labels)
    #kable(d.codebook)
    return(d.codebook)
}


