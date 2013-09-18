\name{summary.panelAR}
\alias{summary.panelAR}
\alias{print.summary.panelAR}
\title{
Summary method for fitted objects of class \code{"panelAR"}
}
\description{
\code{summary} method for class \code{"panelAR"}.
}
\usage{
\method{summary}{panelAR}(object, \dots)

\method{print}{summary.panelAR}(x,digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"),\dots)
}

\arguments{
  \item{object}{an object of class \code{"panelAR"}.
}
  \item{x}{an object of class \code{"summary.panelAR"}.
}
  \item{digits}{integer. the number of significant digits to use when printing.
}
  \item{signif.stars}{logical. If \code{TRUE}, \sQuote{significance stars} are printed for each coefficient.
}
  \item{\dots}{further arguments passed to or from other methods.
}
}

\value{
The function \code{summary.panelAR} returns a list of summary statistics from the fitted model in \code{object}. The list contains the following components:
	\item{call}{the matched call.}
	\item{terms}{the terms object used.}
	\item{coefficients}{the named vector of coefficients.}
	\item{residuals}{the residuals.}
	\item{aliased}{named logical vector designating if original coefficients are aliased.}
	\item{df}{vector of the form \eqn{(k,N-k,k^*)}, where \eqn{k} is the rank of the model matrix, \eqn{N-k} gives the residual degrees of freedom, and \eqn{k^*} is the number of total coefficients.}
	\item{rho}{autocorrelation parameters. Scalar if \code{"ar1"} option was used, vector of length \eqn{N_p} (number of panels) if \code{"psar1"} option was used, and \code{NULL} if \code{"none"} option was used.}
	\item{Sigma}{\eqn{N_p \times N_p} matrix of estimated panel covariances.}
	\item{r2}{\eqn{R^2} based on quasi-differenced data from the Prais-Winsten regression. Set to \code{NULL} if PWLS or Parks-Kmenta procedures are used.}
	\item{wald}{results of Wald test.}
	\item{vcov}{estimated variance-covariance matrix of coefficients.}
	\item{na.action}{information passed on from \code{obj} about the handling of \code{NA}s.}
	\item{panelStructure}{a list of several objects which contain information on the panel structure of the data. See details below.}

Contents of \code{panelStructure}:
\item{N}{number of observations.}
\item{N.panel}{number of panels.}
\item{N.time}{number of times.}
\item{balanced}{logical indicating whether panels are balanced.}
\item{N.min}{minimum number of observations per panel.}
\item{N.max}{maximum number of observations per panel.}
\item{N.avg}{average number of observations per panel.}
\item{N.per.panel}{named vector giving number of observations per panel.}
}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}
\seealso{The function \code{\link{panelAR}}. Function \code{\link{coef}} will extract the table of coefficients, standard errors, t-statistics, and p-values.
}
\examples{
}