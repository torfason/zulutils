% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zuluverse.R
\name{mutate_cond}
\alias{mutate_cond}
\title{Mutate only rows of data.frame/tibble that satisfy condition}
\usage{
mutate_cond(
  .data,
  .condition,
  ...,
  .envir = parent.frame(),
  .method = c("default", "dplyr", "base")
)
}
\arguments{
\item{.data}{The data.frame to mutate.}

\item{.condition}{Conditional statement determining which rows to modify.}

\item{...}{One or more statements determining which columns to mutate and how.}

\item{.envir}{Which environment to use for evaluation.}

\item{.method}{Which underlying method to use for the mutation.
Acceptable values are:
\itemize{
\item \code{"default"}, the default, uses \code{dplyr::mutate()} if available, but
falls back on \code{base::transform()} (with a warning) if not.
\item \code{"dplyr"} uses \code{dplyr::mutate()} and throws an error if it is not
available.
\item \code{"base"} always uses \code{base::transform()}.
}}
}
\description{
The \code{\link[=mutate_cond]{mutate_cond()}} function allows simple conditional mutations
of \code{data.frames} by combining a conditional to select rows, followed by
\code{dplyr::mutate()} syntax to specify how to change columns. This function is
inspired by, and based on, a function proposed in \href{https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows}{a discussion on Stack Overflow}.
This function, however, extends the original \code{mutate_cond()} function
proposed in that thread by falling back on using \code{base::transform()} if the
\code{dplyr} package is not installed.
}
\details{
All named parameters are prefixed with \code{.} to reduce the probability of
conflict with the column names that are to be mutated, which are specified in
the \code{...} parameter.

The function relies on either \code{dplyr::mutate()} if available, or
\code{base::transform()} if the \code{dplyr} package is available. Specifying multiple
mutations in \code{...} is allowed, but, if doing so, it is important to be aware
of the differences between \code{mutate()} and \code{transform()}. The two
functions are very similar, apart from the fact that \code{mutate()} executes the
transformations in an iterative manner, so that later transformations can use
the columns created by earlier transformations, whereas \code{transform()} uses
the values from the original \code{data.frame}, regardless of the number of steps.
}
\examples{
# Set dist to 3 where speed is 7 or less
result <- mutate_cond(cars, speed<=7, dist=3)
head(result)

}
\concept{functions extending \code{dplyr}}
