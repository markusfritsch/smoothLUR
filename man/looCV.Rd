% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/looCV.R
\name{looCV}
\alias{looCV}
\title{Function for conducting leave-one-out cross-validation}
\usage{
looCV(data, x, ID, spVar1, spVar2, y, dirEff, thresh = 0.95)
}
\arguments{
\item{data}{A data set which contains the dependent variable and the
potential predictors.}

\item{x}{A character vector stating the variable names of the
potential predictors (names have to match the column names of
`data`).}

\item{ID}{A character vector stating the variable name referring to the
monitoring sites' ID (name has to mach the column name of `data`).}

\item{spVar1}{A character vector stating the variable name referring to
longitude (name has to match the column name of `data`).}

\item{spVar2}{A character vector stating the variable name referring to
latitude (name has to match the column name of `data`).}

\item{y}{A character string indicating the name of the dependent
variable (name has to match the column name of `data`).}

\item{dirEff}{A vector that contains one entry for each potential
predictor and indicates the expected direction of the effect of the
potential predictor (1 for positive, -1 for negative and 0 if the
expected effect sign is unclear). Argument is only required for
parametric model fitting.}

\item{thresh}{A numeric value that indicates the maximum share of
zero values; if the share is exceeded, the corresponding potential
predictor is excluded.}
}
\value{
An object of class `loocvLUR` with the following elements:

\item{df.err}{data.frame with four columns: ID (ID of monitoring
   site), Err.par (Errors derived from parametric LUR model),
   Err.smooth (Errors derived from smooth LUR model)}
\item{ls.models}{list with elements according to lines of data set;
   each list element is named according to the ID of the omitted
   monitoring site is itself a list containing two elements:
   mod.par (parametric model based on remaining sites), mod.smooth
   (smooth model based on remaining sites)}

It has `...`, `...`, and `...` methods.
}
\description{
\code{looCV} conducts a leave-one-out cross-validation for parametric
   and smooth land use regression (LUR) models fitted with the functions
   \code{parLUR} and \code{smoothLUR}, respectively.
}
\examples{
## Load data set
data(monSitesDE, package="smoothLUR")

}
\seealso{
\code{\link{parLUR}} for parametric land use regression (LUR)
   modeling.
\code{\link{smoothLUR}} for smooth land use regression (LUR)
   modeling.
\code{\link{kFoldCV}} for k-fold cross-validation for
   parLUR and smoothLUR objects.
}
\author{
Svenia Behm and Markus Fritsch
}
