\name{panelAR}
\alias{panelAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{print.panelAR}
\title{Estimation of Linear AR(1) Panel Data Models with Cross-Sectional Heteroskedasticity and/or Correlation
}
\description{
	Description goes here.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function is robust to unbalanced panels and multiple runs per panel. 
}
\usage{
panelAR(formula, data, panelVar, timeVar, autoCorr = c("ar1", 
    "none", "psar1"), panelCorrMethod = c("none","phet","pcse","pwls","parks-kmenta"), rhotype ="br", bound.rho = FALSE, rho.na.action = c("none", "omit"), panel.weight = c("t-1", "t"), dof.correction = FALSE, complete.case = FALSE, seq.times = FALSE,  singular.ok=TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class "\code{\link{formula}}" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
}
  \item{data}{a data frame containing the variables in the model, as well as a variables defining the units and time.
}
  \item{panelVar}{the column name of \code{data} that contains the panel ID. It cannot contain any \code{NA}s. May be set to \code{NULL}, in which case all observations are assumed to belong to the same unit.
}
  \item{timeVar}{the column of \code{data} that contains the time ID. It must be a vector of integers and cannot contain any \code{NA}s.
}
  \item{autoCorr}{character string denoting structure of autocorrelation in the data: \code{ar1} denotes AR(1)-type autocorrelation with a common correlation coefficient across all panels, \code{psar1} denotes AR(1)-type autocorrelation with a unique correlation coefficient for each panel, and \code{none} denotes no autocorrelation. Default: \code{AR1}.
}
  \item{panelCorrMethod}{character string denoting method used for dealing with panel heteroskedasticity and/or correlation. \code{none} denotes homoskedasticity and no correlation across panels, \code{phet} denotes a Huber-White style sandwich estimator for panel heteroskedasticity, \code{pcse} denotes panel-corrected standard errors that are robust to both heteroskedasticity and contemporaneous correlation across panels, \code{pwls} denotes means that FGLS is used to deal with panel heteroskedasticity, and \code{parks-kmenta} means that FGLS is used to estimate both panel heteroskedasticity and correlation. Default: \code{none}.
}
#####
  \item{rhotype}{character string denoting method used for estimating autocorrelation coefficient, rho. Possible options are \code{br},\code{fr},\code{dw},\code{ts},\code{theil}, and \code{theil-nagar}. See \sQuote{Details}. Default: \code{br}.
}
  \item{bound.rho}{logical. If \code{TRUE}, the panel-specific autocorrelation coefficient \eqn{\rho_i} is bounded to [-1,1] in the calculation of \eqn{\rho}; used only for \code{autoCorrType="common"}. Default: \code{TRUE}.
}
  \item{rho.na.action}{logical. If \code{FALSE}, set panel-specific autocorrelation coefficient \eqn{\rho_i} to 0 for panels where \eqn{\rho_i} returns \code{NA}. Otherwise, omit panels where \eqn{\rho_i} is \code{NA}. Default: \code{TRUE}.
  	%% change to rho.na.omit
}
  \item{panel.weight}{the weight to be used for each panel when combining panel-specific autocorrelations \eqn{\rho_i} to a common \eqn{\rho}. Weight is either the number of time periods in the corresponding panel (\code{t}) or the number of time periods minus 1 (\code{t-1}). Default: \code{t}.
}

  \item{dof.correction}{logical. If \code{TRUE}, standard errors are adjusted by a factor of \eqn{N/(N-k)}, where \eqn{N} is total number of observations and \code{k} is the rank of the linear model. Default: \code{FALSE}.
}
  \item{complete.case}{logical. If \code{TRUE}, use only complete cases in the estimation of PCSEs. Otherwise, use pairwise procedure. Default: \code{FALSE}.
}
  \item{seq.times}{logical. If \code{TRUE}, .
}
  \item{singular.ok}{logical. If \code{FALSE}, a singular failure results in an error. Default: \code{TRUE}.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
Specification of the unit and time variables:
panel ID must always be defined.

time ID must be defined for all data structures with autocorrelation and/or PCSE. Time must be measured as an integer. Every observation within a given panel must have unique times.

Spacing of time observations:
\code{seq.times} option

Dealing with panels with just one observation:

PCSE is robust to unbalanced panels, as the presence of panels without any overlapping observations (in which case panel correlation is assumed to be 0) - this is relevant for pairwise calculation (NA elements of Sigma are set to 0).
Two ways to estimate PCSE: complete case which uses only the time periods where every panel has a valid observation. Pairwise option bases calculation on pairs of obs.

%%not sure: For any time periods where a given two panels don't overlap, correlation is assumed to be 0. (in casewise option too I think)

}
\value{
	\code{panelAR} returns an object of class \sQuote{\code{panelAR}}.
	
	The function \code{summary} can be used to obtain and print a summary of the results. Note that default methods \code{\link{coefficients}}, \code{\link{fitted.values}}, and \code{\link{residuals}} returns vectors of regression coefficients, fitted values, and residuals, respectively.
	
	An object of class \sQuote{\code{panelAR}} contains the following components, very similar to the outputs of the standard \code{\link{lm}} function:
	\item{coefficients}{the named vector of coefficients.}
	\item{residuals}{the residuals.}
	\item{fitted.values}{the fitted mean values.}
	\item{rank}{the numeric rank of the fitted linear model.}
	\item{df.residual}{ the residual degrees of freedom.}
	\item{call}{the matched call.}
	\item{terms}{the terms object used.}
	\item{model}{the model frame used.}
	\item{na.action}{information returned by model.frame in the handling of \code{NA}s.}
	\item{vcov}{Estimated variance-covariance matrix of coefficients.}
	\item{panelStructure}{A list of several objects which contain information on the panel structure of the data. See details below.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Details of \code{panelStructure}:
\item{obs.mat}{Boolean matrix of dimension \eqn{N_p \times T}. If cell value is \code{TRUE}, panel \eqn{i} at time \eqn{t} has a valid observation. Panel structure is balanced if entire matrix is \code{TRUE}.}
\item{rho}{Contains autocorrelation parameters. Scalar if \code{"common"} option was used, vector of length \eqn{N_p} (number of panels) if \code{"psar"} option was used, and \code{NULL} if \code{"none"} option was used.}
\item{Sigma}{\eqn{N_p \times N_p} matrix of estimated panel correlations.}
\item{N.cov}{Number of panel covariances estimated.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{summary.panelAR}} for summary.

\code{\link{predict.panelAR}} for prediction.

\code{\link{plot.panelAR}} to plot image of panel structure.

\code{\link{runs.analysis}} for analysis of runs.
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
