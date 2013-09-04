\name{runs.analysis}
\alias{runs.analysis}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runs.analysis(object)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class "\code{panelAR}."
}
}

\details{
	
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	The output of the function is an object of class "\code{panelAR.runs}".
	
	\item{run.count}{a named vector indicating the number of runs per panel.}
	\item{runs}{a data frame, with each row corresponding to a run in the data. Panels with multiple runs appear in multiple rows. The data frame contains the start time of the run, the end time of the run, and the length of the run.}
	%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{The function \code{\link{panelAR}}.
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
