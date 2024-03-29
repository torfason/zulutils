% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zulugg.R
\name{gg_integer_breaks}
\alias{gg_integer_breaks}
\title{Apply integer breaks to a graph.}
\usage{
gg_integer_breaks(x)
}
\arguments{
\item{x}{The input vector for which the breaks should be calculated.
Note that there is no fancy NSE here, the actual variable must be
passed to the function, and also that the function currently does
not handle negative data and will error out if passed such data.}
}
\value{
A vector of breaks suitable for use with \code{ggplot} breaks parameter.
}
\description{
Calculates a vector of breaks that contains all integers between
0 and max(x) (with a little bit of extra headroom in case the
maximum element is an integer)
}
\examples{
if ( require(ggplot2) ) {
  ggplot(mtcars) +
    aes(wt,drat) +
    geom_point() +
    scale_y_continuous(breaks=gg_integer_breaks(mtcars$drat))
}

}
\seealso{
Other functions extending \code{ggplot}: 
\code{\link{gg_apply_labs}()},
\code{\link{gg_apply}()}
}
\concept{functions extending \code{ggplot}}
