\name{Rehm}
\alias{Rehm}
\docType{data}
\title{Rehm (2011) Replication Data}

\description{Replication data for macro-level study in Rehm (2011), specifically to replicate results reported in Table 3. Data structure is panels of OECD countries from 2001-2004. Data contains measurements of unemployment benefit generosity, Rehm's homogeneity of the risk pool measure, and various controls.}

\usage{Rehm}

\format{A dataframe with the following variables: 
	\itemize{
		\item{\code{year}: time identifier.}
		\item{\code{ccode}: country identifier.}
		\item{\code{NURR}: net unemployment replacement rate, as a proxy for unemployment benefit generosity.}
		\item{\code{gini}: Gini coefficient of unemployment risk.}
		\item{\code{mean_ur}: national unemployment rate, calculated as weighted mean of occupational unemployment rates (weights are occupation sizes).}
		\item{\code{selfemp}: self-employment as a percentage of civilian employment.}
		\item{\code{cum_right}: cumulative percent of total cabinet posts held by right-wing parties since 1990 (weighted by days).}
		\item{\code{tradeunion}: trade union density.}
		\item{\code{deficit}: budget deficit as a percentage of GDP.}
		\item{\code{tradeopen}: trade openness (sum of imports and exports as proportion of GDP).}
		\item{\code{gdp_growth}: GDP growth.}
	}
}

\source{Rehm, Philipp. 2011. \dQuote{Social Policy by Popular Demand.} \emph{World Politics} 63(2): 271-299.}

\seealso{\code{\link{panelAR}}. Run \code{demo(Rehm)} for examples which use \code{Rehm}.}
\keyword{datasets}