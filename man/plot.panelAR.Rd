\name{plot.panelAR}
\alias{plot.panelAR}
\title{
Plot Panel Structure
}
\description{Plots grid of panels and times, colored to distinguish observed and missing data.
}
\usage{
\method{plot}{panelAR}(x,legend=TRUE,rot.axis=c(0,0),\dots)
}
\arguments{
  \item{x}{an object of class \code{"panelAR"}.
}
  \item{legend}{logical. If \code{TRUE}, legend is printed.
}
  \item{rot.axis}{vector of degrees for rotation of axis labels. First element corresponds to time labels and second element corresponds to panel labels.
  	}
  \item{\dots}{further arguments passed to or from other methods.}
}

\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}
\seealso{\code{\link{panelAR}}.
}
\examples{
data(WhittenWilliams)
# expect warning urging to use 'complete.case=FALSE'
out <- panelAR(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+
lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, 
data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", 
panelCorrMethod="pcse", complete.case=TRUE)

par(cex.axis=0.6)
plot(out, rot.axis=c(45,0))
}