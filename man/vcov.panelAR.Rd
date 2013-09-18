\name{vcov.panelAR}
\alias{vcov.panelAR}
\title{
Variance-covariance method for fitted objects of class \code{"panelAR"}
}
\description{
\code{vcov} method for class \code{"panelAR"}.
}
\usage{
\method{vcov}{panelAR}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"panelAR"}.
}
\item{\dots}{further arguments passed to or from other methods.
	}
}

\value{A matrix of estimated covariances between parameter estimates. The row and column names correspond to the parameter names obtained using the \code{\link{coef}} method.
}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}
\seealso{The function \code{\link{panelAR}}.
}