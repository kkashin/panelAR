\name{predict.panelAR}
\alias{predict.panelAR}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
## S3 Method for class 'panelAR'
predict.panelAR(object,newdata=NULL,se.fit = FALSE,
				conf.interval = FALSE, conf.level = 0.95, na.action=na.pass)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class "\code{panelAR}."
}
  \item{newdata}{an optional data frame used for prediction. If omitted, the fitted values are used.
}
  \item{se.fit}{logical. If \code{TRUE}, standard errors of predicted values are reported."
}
  \item {conf.interval}{UPDATE THIS!!!
}
  \item{conf.level}{the confidence level.}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	\item{fit}{either a vector or a data frame containing the fitted values, as well as standard errors and/or intervals (if specified). If \code{se.fit="TRUE"}, \code{se.fit} column provides the standard errors. If \code{interval} is set, \code{lb} and \code{ub} provide the lower and upper bounds of the interval, respectively.}
	\item{df}{degrees of freedom.}
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
