% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.naivesubspacedensityratio}
\alias{predict.naivesubspacedensityratio}
\title{Obtain predicted density ratio values from a \code{naivesubspace} object}
\usage{
\method{predict}{naivesubspacedensityratio}(object, newdata = NULL, log = FALSE, ...)
}
\arguments{
\item{object}{A \code{naivesubspace} object}

\item{newdata}{Optional \code{matrix} new data set to compute the density}

\item{log}{A logical indicating whether to return the log of the density ratio}

\item{...}{Additional arguments to be passed to the function}
}
\value{
An array with predicted density ratio values from possibly new data,
but otherwise the numerator samples.
}
\description{
Obtain predicted density ratio values from a \code{naivesubspace} object
}
\examples{
x <- rnorm(100) |> matrix(100)
y <- rnorm(200, 1, 2) |> matrix(200)
fit1 <- naivesubspace(x, y)
predict(fit1)
predict(fit1, newdata = rbind(x, y))
predict(fit1, newdata = rbind(x, y), log = TRUE)
}
\seealso{
\code{\link{predict}}, \code{\link{naivesubspace}}
}
\keyword{naivedensityratio}
\keyword{predict}
