
##
## Deprecated in favor of the zfit package
##

#' @rdname zulutils-deprecated
#' @section \code{zlm}:
#' \code{zlm()} is deprecated in favor of \code{\link{zfit::zlm()}}.
#'
#' @export
zlm <- function(data, formula, ...) {
  .Deprecated("zlm", msg="zlm() is deprecated in favor of zfit::zlm().")
  zfit::zlm(data, formula, ...)
}

#' @rdname zulutils-deprecated
#' @section \code{zglm}:
#' \code{zglm()} is deprecated in favor of \code{\link{zfit::zglm()}}.
#'
#' @export
zglm <- function(data, formula, ...) {
  .Deprecated("zglm", msg="zglm() is deprecated in favor of zfit::zglm().")
  zfit::zglm(data, formula, ...)
}

#' @rdname zulutils-deprecated
#' @section \code{zlogit}:
#' \code{zlogit()} is deprecated in favor of \code{\link{zfit::zlogit()}}.
#'
#' @export
zlogit <- function(data, formula, ...) {
  .Deprecated("zlogit", msg="zlogit() is deprecated in favor of zfit::zlogit().")
  zfit::zlogit(data, formula, ...)
}

#' @rdname zulutils-deprecated
#' @section \code{zprobit}:
#' \code{zprobit()} is deprecated in favor of \code{\link{zfit::zprobit()}}.
#'
#' @export
zprobit <- function(data, formula, ...) {
  .Deprecated("zprobit", msg="zprobit() is deprecated in favor of zfit::zprobit().")
  zfit::zprobit(data, formula, ...)
}

#' @rdname zulutils-deprecated
#' @section \code{zn}:
#' \code{zn()} is deprecated in favor of \code{\link{zfit::zn()}}.
#'
#' @export
zn <- function(x, ..., labels=FALSE) {
  .Deprecated("zn", msg="zlogit() is deprecated in favor of zfit::zn().")
  zfit::zn(x, ..., labels=FALSE)
}

#' @rdname zulutils-deprecated
#' @section \code{zprint}:
#' \code{zprint()} is deprecated in favor of \code{\link{zfit::zprint()}}.
#'
#' @export
zprint <- function(x, f, ...) {
  .Deprecated("zprint", msg="zprint() is deprecated in favor of zfit::zprint().")
  zfit::zprint(x, f, ...)
}
