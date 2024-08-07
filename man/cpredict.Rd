% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpredict.R
\name{cpredict}
\alias{cpredict}
\title{Conditional Predictions for Multivariate Linear Model Fits}
\usage{
cpredict(object, standardize = TRUE, ...)
}
\arguments{
\item{object}{a \code{mlm} object, typically the result of calling \code{lm} with a matrix response.}

\item{standardize}{logical defaults to \code{TRUE}, standardising responses so they
are comparable across responses.}

\item{...}{further arguments passed to \code{\link{predict.lm}}, in particular, \code{newdata}.
However, this function was not written to accept non-default values for \code{se.fit}, 
\code{interval} or \code{terms}.}
}
\value{
A matrix of predicted values from full conditional models.
}
\description{
Predicted values using full conditional models derived from a multivariate 
linear model (\code{mlm}) object. The full conditionals model each response as a
linear model with all other responses used as predictors in addition to the
regressors specified in the formula of the \code{mlm} object.
}
\details{
Predictions using an \code{mlm} object but based on the full conditional model,
that is, from a linear model for each response as  a function of all responses 
as well as predictors. This can be used in plots to diagnose the multivariate 
normality assumption.

By default predictions are standardised to facilitate overlay plots of multiple
responses, as in \code{\link{plotenvelope}}.

This function behaves much like \code{\link{predict.lm}}, but currently, standard 
errors and confidence intervals around predictions are not available.
}
\examples{
data(iris)
# fit a mlm:
iris.mlm=lm(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)~Species,data=iris)
# predict each response conditionally on the values of all other responses:
cpredict(iris.mlm)

}
\references{
Warton DI (2022) Eco-Stats - Data Analysis in Ecology, from \emph{t}-tests to multivariate abundances. Springer, ISBN 978-3-030-88442-0
}
\seealso{
\code{\link{cresiduals}}, \code{\link{lm}}, \code{\link{plotenvelope}}, \code{\link{predict.lm}}
}
\author{
David Warton <david.warton@unsw.edu.au>
}
