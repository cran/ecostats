% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cresiduals.R
\name{cresiduals}
\alias{cresiduals}
\title{Extract Conditional Residuals from Multivariate Linear Model Fits}
\usage{
cresiduals(object, standardize = TRUE, ...)
}
\arguments{
\item{object}{a \code{mlm} object, typically the result of calling \code{lm} with a matrix response.}

\item{standardize}{logical defaults to \code{TRUE}, to return studentized residuals
using \code{\link{rstandard}} so they are comparable across responses.}

\item{...}{further arguments passed to \code{\link{residuals.lm}} or \code{\link{rstandard}}.}
}
\value{
A matrix of residuals
}
\description{
Residuals from full conditionals of a Multivariate 
Linear Model (\code{mlm}) object. The full conditional for each response is a
linear model with all other responses used as predictors in addition to the
regressors specified in the formula of the \code{mlm} object. This is used to 
diagnose the multivariate normality assumption in \code{\link{plotenvelope}}.
}
\details{
A \code{residuals} function for \code{mlm} objects, which returns residuals from a full 
conditional model, that is, a linear model of each response against all responses
as well as predictors, which can be used to diagnose the multivariate normality assumption.
These can be standardized (\code{standardize=TRUE}) to facilitate overlay plots of multiple
responses, as in \code{\link{plotenvelope}}.
}
\examples{
data(iris)
# fit a mlm:
iris.mlm=lm(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)~Species,data=iris)
# construct full conditional residuals:
cresiduals(iris.mlm)

}
\references{
Warton DI (2022) Eco-Stats - Data Analysis in Ecology, from \emph{t}-tests to multivariate abundances. Springer, ISBN 978-3-030-88442-0
}
\seealso{
\code{\link{cpredict}}, \code{\link{lm}}, \code{\link{plotenvelope}}, \code{\link{residuals}}, \code{\link{rstandard}}
}
\author{
David Warton <david.warton@unsw.edu.au>
}
