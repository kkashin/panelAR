\name{BrooksKurtz}
\alias{BrooksKurtz}
\docType{data}
\title{Brooks and Kurtz (2012) Replication Data}

\description{Replication data for Brooks and Kurtz (2012). Data structure is panels of Latin American countries from 1983-2007. Data contains measurements of capital account openness, capital account openness diffusion variables, and a variety of economic and financial controls.}

\usage{BrooksKurtz}

\format{A dataframe with the following variables: 
	\itemize{
		\item{\code{country}: country identifier (string).}
		\item{\code{conum}: country identifier (numeric).}
		\item{\code{year}: year identifier.}
		}
	
	Dependent variable:
	\itemize{
		\item{\code{kaopen}: capital account openness. Higher value indicates more open capital account.}
		}
		
	Diffusion variables, constructed to be a weighted average of the lagged outcome variable for all countries in a given government's \dQuote{neighborhood}. Proximity is defined via a weight matrix W such that the diffusion variable is constructed as \eqn{WY_{t-1}}. The weight matrix differs across the following diffusion variables. See pages 112-114 of Brooks and Kurtz (2012) for details.
	\itemize{
		\item{\code{ldiffpeer}: baseline diffusion effect / all countries diffusion variable.}
		\item{\code{ldiffisi}: import-substituting industrialization (ISI) peer diffusion variable.}
		\item{\code{ldiffgrowth}: economic success diffusion variable.}
		\item{\code{ldiffinflation}: inflation performance diffusion variable.}
		\item{\code{ldiffneg}: negative learning diffusion variable.}
		\item{\code{ldiffembi}: competition diffusion variable}
		\item{\code{limf}: dummy variable that is equal to 1 if a country entered into an agreement with the IMF at time \eqn{t-1}.}
		}
		
	Controls:
	\itemize{
		\item{\code{isi_objective}: dummy variable for country-years with ISI policy.}
		\item{\code{partisan}: partisanship coded as 0 for leftist, 1 for centrist, and 2 for conservative governments.}
		\item{\code{checks}: checks on authority.}
		\item{\code{y1995}: dummy for year 1995.}
		\item{\code{lngdpcap}: log of GDP per capita.}
		\item{\code{lngdp}: log of GDP.}
		\item{\code{timetrend}: linear time trend.}
		\item{\code{brk}: dummy for structural break in the data source for \code{kaopen} in 1996.}
		\item{\code{lusffr}: US federal funds rate at time \eqn{t-1}.}
		\item{\code{linflation}: log of inflation at time \eqn{t-1}.}
		\item{\code{lbankra}: bank reserve-to-asset ratio at time \eqn{t-1}.}
		\item{\code{lcab}: current account deficit at time \eqn{t-1}.}
		\item{\code{lgrowth}: growth of per capita GDP at time \eqn{t-1}.}
		\item{\code{ltradebalance}: trade deficit at time \eqn{t-1}.}
		}
	}

\source{Brooks, Sarah M. and Marcus J. Kurtz. 2012. \dQuote{Paths to Financial Policy Diffusion: Statist Legacies in Latin America's Globalization.} \emph{International Organization} 66:95-128.}

\seealso{\code{\link{panelAR}}. Run \code{demo(BrooksKurtz)} for examples which use \code{BrooksKurtz}.}
\keyword{datasets}