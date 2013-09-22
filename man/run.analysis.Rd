\name{run.analysis}
\alias{run.analysis}
\title{
Run Analysis for Panel Data
}
\description{
Provides a run analysis within each panel. Calculates number of runs per panel and returns start time, end time, and the length of each run. 
}
\usage{
run.analysis(object)
}
\arguments{
  \item{object}{an object of class \code{"panelAR"}.
}
}

\value{
	The output of the function is an object of class \code{"panelAR.runs"}.
	
	\item{run.count}{a named vector indicating the number of runs per panel.}
	\item{runs}{a data frame, with each row corresponding to a run in the data. Panels with multiple runs appear in multiple rows. The data frame contains the start time of the run, the end time of the run, and the length of the run.}
	\item{rho}{autocorrelation parameters. Scalar if \code{"ar1"} option was used, vector of length \eqn{N_p} (number of panels) if \code{"psar1"} option was used, and \code{NULL} if \code{"none"} option was used.}
}

\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}

\seealso{The function \code{\link{panelAR}}.
}
\examples{
data(WhittenWilliams)
# expect warning urging to use 'complete.case=FALSE'
out <- panelAR(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+
lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, 
data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", 
panelCorrMethod="pcse", complete.case=TRUE)

run.analysis(out) # overview
run.analysis(out)$runs # details of each run
}