\name{panelAR}
\alias{panelAR}
\alias{print.panelAR}
\title{Estimation of Linear AR(1) Panel Data Models with Cross-Sectional Heteroskedasticity and/or Correlation
}
\description{The function estimates linear models on panel data structures in the presence of AR(1)-type autocorrelation as well as panel heteroskedasticity and/or contemporaneous correlation. First, AR(1)-type autocorrelation is addressed via a two-step Prais-Winsten feasible generalized least squares (FGLS) procedure, where the autocorrelation coefficients may be panel-specific. Subsequently, one can choose to implement \sQuote{sandwich}-type robust standard errors with OLS, panel weighted least squares (WLS), panel-corrected standard errors (PCSEs), or the Parks-Kemnta FGLS estimator.
}
\usage{
panelAR(formula, data, panelVar, timeVar, autoCorr = c("ar1", 
    "none", "psar1"), panelCorrMethod = c("none","phet","pcse","pwls",
    "parks-kmenta"), rhotype ="br", bound.rho = FALSE, rho.na.rm = FALSE, 
    panel.weight = c("t-1", "t"), dof.correction = FALSE, complete.case = FALSE, 
    seq.times = FALSE,  singular.ok=TRUE) 
}
\arguments{
  \item{formula}{an object of class "\code{\link{formula}}" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
}
  \item{data}{a data frame containing the variables in the model, as well as a variables defining the units and time.
}
  \item{panelVar}{the column name of \code{data} that contains the panel ID. It cannot contain any \code{NA}s. May be set to \code{NULL}, in which case all observations are assumed to belong to the same unit.
}
  \item{timeVar}{the column of \code{data} that contains the time ID. It must be a vector of integers and cannot contain any \code{NA}s. Duplicate time observations per panel are not allowed. At least two time periods are required.
}
  \item{autoCorr}{character string denoting structure of autocorrelation in the data: \code{ar1} denotes AR(1)-type autocorrelation with a common correlation coefficient across all panels, \code{psar1} denotes AR(1)-type autocorrelation with a unique correlation coefficient for each panel, and \code{none} denotes no autocorrelation. Default: \code{AR1}.
}
  \item{panelCorrMethod}{character string denoting method used for dealing with panel heteroskedasticity and/or correlation. \code{none} denotes homoskedasticity and no correlation across panels, \code{phet} denotes a Huber-White style sandwich estimator for panel heteroskedasticity, \code{pcse} denotes panel-corrected standard errors that are robust to both heteroskedasticity and contemporaneous correlation across panels, \code{pwls} denotes that a panel weighted least squares procedure is to deal with panel heteroskedasticity, and \code{parks-kmenta} means that FGLS is used to estimate both panel heteroskedasticity and correlation. Default: \code{none}.
}
  \item{rhotype}{character string denoting method used for estimating autocorrelation coefficient, rho. Possible options are \code{breg},\code{scorr},\code{freg},\code{theil},\code{dw}, and \code{theil-nagar}. See \sQuote{Details}. Default: \code{breg}.
}
  \item{bound.rho}{logical. If \code{TRUE}, the panel-specific autocorrelation coefficient \eqn{\rho_i} is bounded to \eqn{[-1,1]} in the calculation of \eqn{\rho}; used only for \code{autoCorr="common"}. Default: \code{TRUE}.
}
  \item{rho.na.rm}{logical. If \code{FALSE} and \eqn{\rho_i} cannot be calculated for a panel, function returns error. If \code{TRUE}, \eqn{\rho_i}s that are \code{NA} are ignored if calculating a common AR(1) coefficient or set to 0 if calculating panel-specific AR(1) coefficients. Default: \code{FALSE}.
}
  \item{panel.weight}{the weight to be used for each panel when combining panel-specific autocorrelations \eqn{\rho_i} to a common \eqn{\rho}. Weight is either the number of time periods in the corresponding panel (\code{t}) or the number of time periods minus 1 (\code{t-1}). Default: \code{t}.
}
  \item{dof.correction}{logical. If \code{TRUE}, standard errors are adjusted by a factor of \eqn{N/(N-k)}, where \eqn{N} is total number of observations and \code{k} is the rank of the linear model. Default: \code{FALSE}.
}
  \item{complete.case}{logical. If \code{TRUE}, use only the time periods where every panel has a valid observation in the estimation of PCSEs or the Parks-Kmenta estimator. Otherwise, use pairwise procedure. Default: \code{FALSE}.
}
  \item{seq.times}{logical. If \code{TRUE}, observations are temporally ordered by panel and assigned a sequential time variable that ignores any gaps in the runs. Default: \code{FALSE}.
}
  \item{singular.ok}{logical. If \code{FALSE}, a singular failure results in an error. Default: \code{TRUE}.
}
}

\details{Function for running two-step Prais-Winsten models on panel data that exhibit AR(1)-type autocorrelation. Following the two-step estimation, one can choose to use a \sQuote{sandwich}-type robust standard error estimator with OLS or a panel weighted least squares estimator to address panel heteroskedasticity. Alternatively, if panels are both heteroskedastic and contemporaneously correlated, the package supports panel-corrected standard errors (PCSEs) as well as the Parks-Kmenta FGLS estimator. Note that the Parks-Kmenta estimator should ideally be reserved for use only when the number of time periods is significantly greater than the number of panels (see Beck and Katz). The function is robust to unbalanced panel structures, panels with just one observation, multiple runs per panel, and the presence of panels without any overlapping observations. 

While generally designed to estimate Prais-Winsten models on panel data, setting \code{panelVar} to \code{NULL} will estimate an AR(1) time-series model treating the entire dataset as one unit. In this case, the \code{panelCorrMethod} is ignored since equal variances are assumed across all observations.

A number of common estimators for the autocorrelation coefficient are supported. Specifically:
\describe{
\item{\code{breg}}{Linear regression estimator:
	\eqn{\hat{\rho}_{reg} = \dfrac{\sum_{t=2}^{T_i} \hat{\epsilon}_{i,t}\hat{\epsilon}_{i,t-1}}{\sum_{t=1}^{T_i-1} \hat{\epsilon}_{i,t}^2}}
}
\item{\code{scorr}}{Sample correlation coefficient estimator:
	\eqn{\hat{\rho}_{scorr} = \dfrac{\sum_{t=2}^{T_i} \hat{\epsilon}_{i,t}\hat{\epsilon}_{i,t-1}}{\sum_{t=1}^{T_i} \hat{\epsilon}_{i,t}^2}}
}
\item{\code{freg}}{Forward linear regression estimator:
	\eqn{\hat{\rho}_{fr} = \dfrac{\sum_{t=1}^{T_i-1} \hat{\epsilon}_{i,t}\hat{\epsilon}_{i,t+1}}{\sum_{t=1}^{T_i-1} \hat{\epsilon}_{i,t+1}^2}}
}
\item{\code{theil}}{Theil estimator:
	\eqn{\hat{\rho}_{theil} = \hat{\rho}_{scorr} \dfrac{T_i-k}{T_i-1}}
}
\item{\code{dw}}{Durbin-Watson estimator:
	\eqn{\hat{\rho}_{dw} = 1-\dfrac{1}{2}\dfrac{\sum_{t=2}^{T_i} (\hat{\epsilon}_{i,t}-\hat{\epsilon}_{i,t-1})^2}{\sum_{t=1}^{T_i} \hat{\epsilon}_{i,t}^2}}
}
\item{\code{theil-nagar}}{Theil-Nagar estimator:
	\eqn{\hat{\rho}_{theil-nagar} = \dfrac{T_i^2 \hat{\rho}_{dw} + k^2}{T_i^2-k^2}}
}
} % end of describe

In the expressions above, \eqn{\hat{\epsilon}} denotes observed residuals from the first stage OLS regression, \eqn{T_i} is the number of observations in panel \eqn{i}, and \eqn{k} is the rank of the model matrix. Some of these estimators cannot be calculated for panels with one observation or multiple runs of one observation. In these cases, \code{rho.na.action} controls the treatment of these autocorrelation coefficients. If \code{FALSE}, set panel-specific autocorrelation coefficient \eqn{\rho_i} to 0 for panels where \eqn{\rho_i} returns \code{NA}. Otherwise, omit panels where \eqn{\rho_i} is \code{NA}.

If PCSEs or the Parks-Kmenta estimator are selected, the default is to use all pairwise observations to estimate the time-constant covariances across units. In the case of no overlapping observations between panels, the panel covariance is assumed to be 0. If \code{complete.case} is set to \code{TRUE}, then only the time periods where every panel has a valid observation are used for the calculation of the contemporaneous correlation matrix. 
}
\value{
	\code{panelAR} returns an object of class \sQuote{\code{panelAR}}.
	
	The function \code{summary} can be used to obtain and print a summary of the results. Note that default methods \code{\link{coefficients}}, \code{\link{fitted.values}}, and \code{\link{residuals}} returns vectors of regression coefficients, fitted values, and residuals, respectively. \code{\link{vcov}} returns the estimated variance-covariance matrix of the coefficients.
	
	An object of class \sQuote{\code{panelAR}} contains the following components, very similar to the outputs of the standard \code{\link{lm}} function:
	\item{coefficients}{the named vector of coefficients.}
	\item{residuals}{the residuals.}
	\item{fitted.values}{the fitted mean values.}
	\item{rank}{the numeric rank of the fitted linear model.}
	\item{df.residual}{the residual degrees of freedom.}
	\item{call}{the matched call.}
	\item{terms}{the terms object used.}
	\item{model}{the model frame used.}
	\item{aliased}{named logical vector designating if original coefficients are aliased.}
	\item{na.action}{information returned by model.frame in the handling of \code{NA}s.}
	\item{vcov}{estimated variance-covariance matrix of coefficients.}
	\item{r2}{\eqn{R^2} based on quasi-differenced data from the Prais-Winsten regression. Set to \code{NULL} if PWLS or Parks-Kmenta procedures are used.}
	\item{panelStructure}{a list of several objects which contain information on the panel structure of the data. See details below.}

Details of \code{panelStructure}:
\item{obs.mat}{logical matrix of dimension \eqn{N_p \times T}. If cell value is \code{TRUE}, panel \eqn{i} at time \eqn{t} has a valid observation. Panel structure is balanced if entire matrix is \code{TRUE}.}
\item{rho}{autocorrelation parameters. Scalar if \code{"ar1"} option was used, vector of length \eqn{N_p} (number of panels) if \code{"psar1"} option was used, and \code{NULL} if \code{"none"} option was used.}
\item{Sigma}{\eqn{N_p \times N_p} matrix of estimated panel covariances.}
\item{N.cov}{number of panel covariances estimated.}
} % end of \value
\references{
%% ~put references to the literature/web site here ~
}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}

\seealso{
\code{\link{summary.panelAR}} for summary.

\code{\link{predict.panelAR}} for prediction.

\code{\link{plot.panelAR}} to plot image of panel structure.

\code{\link{runs.analysis}} for analysis of runs.
}
\examples{
# Panel-specific AR(1) with PCSE
data(WhittenWilliams)
out <- panelAR(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", panelCorrMethod="pcse", complete.case=TRUE)
summary(out)
summary(out)$rho

# Panel-specific AR(1) correlation with PWLS	
data(BrookKurtz)
out2 <- panelAR(kaopen ~ ldiffpeer + ldiffisi + ldiffgrowth + ldiffinflation  + ldiffneg + ldiffembi + limf + isi_objective + partisan + checks +  lusffr + linflation + lbankra + lcab + lgrowth +  ltradebalance + lngdpcap + lngdp + brk + timetrend + y1995, data=BrookKurtz, panelVar='country', timeVar='year', autoCorr='psar1', panelCorrMethod='pwls',rho.na.rm=TRUE, panel.weight='t', seq.times=TRUE)
summary(out2)
}
\keyword{regression}
\keyword{ts}
