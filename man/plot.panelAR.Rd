\name{plot.panelFit}
\alias{plot.panelFit}
\title{
Plot Panel Structure
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
## S3 method for class 'panelFit'
plot.panelFit(object,legend=TRUE,rot.axis.x=c(0,0),...)
}
\arguments{
  \item{object}{an object of class \code{"panelFit"}.
}
  \item{legend}{logical. If \code{TRUE}, legend is printed.
}
  \item{rot.axis}{vector of degrees for rotation of axis labels. First element corresponds to time labels and second element corresponds to panel labels.
  	}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
}
\references{
}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}
\seealso{The function \code{\link{panelFit}}.
}
\examples{
data(WhittenWilliams)
out <- panelAR(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", panelCorrMethod="pcse", complete.case=TRUE)
par(cex.axis=0.6)
plot(out, rot.axis=c(45,0))
}