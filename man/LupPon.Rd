\name{LupPon}
\alias{LupPon}
\docType{data}
\title{Lupu and Pontusson (2011) Replication Data}

\description{Replication data for Table 2, \dQuote{Determinants of Redistribution}, in Lupu and Pontusson (2011). Data structure is panels of OECD countries from 1969 to 2005. Data contains measurements of redistribution, various summaries of the earnings distribution, and controls.}

\usage{LupPon}

\format{A dataframe with the following variables: 
	\itemize{
		\item{\code{country}: country name.}
		\item{\code{id}: country identifier.}
		\item{\code{year}: time identifier.}
		\item{\code{redist}: percentage change in Gini coefficients as move from gross market income to disposable income.}
		\item{\code{ratio9050}: ratio of earnings of a worker in the 90th percentile of the earnings distribution to the earnings of the worker with median income.}
		\item{\code{ratio5010}: ratio of earnings of a worker with median income to the earnings of a worker in the 10th percentile of the earnings distribution.}
		\item{\code{ratio9010}: ratio of earnings of a worker in the 90th percentile of the earnings distribution to the earnings of a worker in the 10th percentile of the earnings distribution.}
		\item{\code{skew}: ratio of the 90-50 ratio to the 50-10 ratio.}
		\item{\code{turnout}: turnout (as a percentage of eligible voters) in the mostrecent national election.}
		\item{\code{fempar}: proportion of working-age women in the labor force.}
		\item{\code{propind}: electoral system proportionality index (between 0 and 1, where 1 is the highest level of proportionality).}
		\item{\code{pvoc}: enrollment in vocational training programs as percentof secondary school enrollment.}
		\item{\code{union}: annual net union density.}
		\item{\code{unempl}: annual rate of unemployment.}
	}
}

\source{Lupu, Noam and Jonas Pontusson. 2011. \dQuote{The Structure of Inequality and the Politics of Redistribution.} \emph{APSR} 105(2): 316-336.
	
Full dataset is available at: \url{http://www.noamlupu.com/LupPon_APSR.dta}.

See Appendix of Lupu and Pontusson (2011) for details of variable sources.
}

\seealso{\code{\link{panelAR}}. Run \code{demo(LupPon)} for examples which use \code{LupPon}.}
\keyword{datasets}