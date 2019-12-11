

#' Return the size of a drake target
#'
#' This function returns the size of a given drake target.
#'
#' @param target Name of the target
#' @return       Size of the target
#'
#' @export
#'
get_drake_target_size <- function(target) {
    cache <- drake::get_cache() # or storr::storr_rds(".drake")
    root  <- cache$driver$path
    hash  <- cache$driver$get_hash(target, namespace = "objects")
    path  <- file.path(root, "data", paste0(hash, ".rds"))
    if ( file.exists(path) ) {
        file.size(path)
    } else {
        NA
    }
}

#' Return the size of all targets in a drake plan
#'
#' This function returns the size of all targets
#' in a given drake plan
#'
#' @param my_plan Name of the plan from which to get target names
#' @return        Tibble containing names and sizes of the targets
#'
#' @export
#'
get_drake_target_sizes <- function(my_plan) {
    result <- tibble::tibble( target = my_plan$target,
                             size = sapply(my_plan$target, get_drake_target_size))
    result <- dplyr::arrange(result,-size)
    return(result)
}
