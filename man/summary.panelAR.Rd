\name{summary.panelAR}
\alias{summary.panelAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{print.summary.panelAR}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.panelAR(object, ...)
}

\usage{
## S3 method for class 'summary.panelAR'
print(x, digit=max(3,getOption("digits")-3),
signif.stars=getOption("show.signif.stars"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class "\code{panelAR}."
}
  \item{x}{an object of class "\code{summary.panelAR}."
}
  \item{digits}{the number of significant digits to use when printing."
}
  \item {signif.stars}{logical. If \code{TRUE}, ‘significance stars’ are printed for each coefficient.
}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	\item{call}{the matched call.}
	\item{terms}{the terms object used.}
	\item{coefficients}{the named vector of coefficients.}
	\item{residuals}{the residuals.}
	\item{Sigma}{\eqn{N_p \times N_p} matrix of estimated panel correlations.}
	\item{vcov}{Estimated variance-covariance matrix of coefficients.}
	\item{na.action}{information passed on from \code{object} about the handling of \code{NA}s.}
	\item{panelStructure}{A list of several objects which contain information on the panel structure of the data. See details below.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Contents of \code{panelStructure}:
\item{N}{Number of observations.}
\item{N.panel}{Number of panels.}
\item{N.time}{Number of times.}
\item{balanced}{Boolean indicating whether panels are balanced.}
\item{N.max}{Maximum number of observations per panel.}
\item{N.min}{Minimum number of observations per panel.}
\item{N.avg}{Average number of observations per panel.}
\item{N.per.panel}{Number of observations per panel.}
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

\seealso{The function \code{\link{panelAR}}. Function \code{\link{coef}} will extract the table of coefficients, standard errors, t-statistics, and p-values.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (obj, ...) 
{
    N <- length(obj$residuals)
    k <- length(obj$coefficients)
    df.resid <- N - k
    se <- sqrt(diag(obj$vcov))
    coef <- obj$coefficients
    t.stat <- (coef)/se
    p.val <- 2 * pt(abs(t.stat), df.resid, lower.tail = FALSE)
    tab <- cbind(coef, se, t.stat, p.val)
    dimnames(tab) <- list(names(coef), c("Estimate", "Std. Error", 
        "t value", "Pr(>|t|)"))
    wald <- coef[-1] \%*\% solve(obj$vcov[-1, -1]) \%*\% coef[-1]
    pchisq(wald, k - 1, lower.tail = FALSE)
    N.rho <- length(obj$panelStructure$rho)
    df.pearson <- N - k - obj$N.cov - N.rho
    obj$call
    obj$coef
    obj$vcov
    obj$panelStructure
    N.panel <- rowSums(obj$panelStructure$obs.mat)
    N.min <- min(N.panel)
    N.max <- max(N.panel)
    avgperpanel <- N/length(N.panel)
    out <- list(call = obj$call, residuals = obj$residuals, coefficients = tab, 
        terms = obj$terms, N.panel = N.panel, vcov = obj$vcov, 
        Sigma = obj$panelStructure$Sigma, obs.mat = obj$panelStructure$obs.mat)
    class(out) <- "summary.panelAR"
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
