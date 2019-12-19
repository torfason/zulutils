# zfit - Currently part of zulutils
#
# A collection of utility functions included to make model
# fitting pipe friendlier.
#
# Some useful keyboard shortcuts for package authoring:
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'




#' Run an lm model in a pipe
#'
#' This function wraps around the \code{lm} function in order to make it
#' more friendly to pipe syntax (with the data first)
#'
#' @param data A \code{data.frame}
#' @param ... Other arguments to be passed to \code{lm}
#'
#' @return A fitted model
#'
#' @examples
#' cars %>% zlm( speed ~ dist )
#'
#' iris %>%
#'   filter(Species=="setosa") %>%
#'   zlm(Sepal.Length ~ Sepal.Width + Petal.Width)
#'
#' @family zfit
#' @export
#'
zlm = function(data, formula, ...) {
    lm(formula=formula, ..., data=data)
}

#' Run a glm model in a pipe (see \code{zlm})
#'
#' @family zfit
#' @export
#'
zglm = function(data, formula, ...) {
    glm(formula=formula, ..., data=data)
}

#' Run a logit model in a pipe (see \code{zlm})
#'
#' @family zfit
#' @export
#'
zlogit = function(data, formula, ...) {
    glm(formula=formula, family=binomial(link="logit"), ..., data=data)
}

#' Run a probit model in a pipe (see \code{zlm})
#'
#' @family zfit
#' @export
#'
zprobit = function(data, formula, ...) {
    glm(formula=formula, family=binomial(link="probit"), ..., data=data)
}



#' Print the result of a function in a pipe but return original object
#'
#' This function passes \code{x} to \code{f} and prints the result, but then
#' returns the original \code{x}. It is useful in a pipe, when one wants a
#' to print the derivative of an object in the pipe but then return or assign
#' the original object. An example is printing the \code{summary()} of an
#' estimated model but
#'
#'
#' @param x An object, typically in a pipe
#' @param f A function to be applied to \code{x} before printing
#' @param ... Other arguments to be passed to \code{f}
#'
#' @return The original object \code{x}
#'
#' @examples
#' m <- lm( speed ~ dist, cars) %>%
#'   zprint(summary) # prints summary(x)
#' m                 # m is the original model object
#'
#' # Pipe example using tidyverse example data
#' sw_subset <- starwars %>%
#'   zprint(count, homeworld, sort=TRUE) %>% # prints counts by homeworld
#'   filter(homeworld=="Tatooine")
#' sw_subset  # sw_subset is ungrouped, but filtered by homeworld
#'
#' @family zfit
#' @export
#'
zprint = function(x, f, ...) {
    print(f(x, ...))
    return(x)
}

