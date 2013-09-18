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

}

\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}

\seealso{The function \code{\link{panelAR}}.
}
\examples{
}