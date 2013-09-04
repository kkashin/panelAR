\name{BrookKurtz}
\alias{BrookKurtz}

\title{Brooks and Kurtz (2012) Replication Data}

\description{Replication data for Brooks and Kurtz (2012). Data structure is panels of Latin American countries from 1983-2007. Data contains measurements of capital account openness, capital account openness diffusion variables, and a variety of economic and financial controls.}

\usage{BrookKurtz}

\format{A dataframe with the following variables: 
	\itemize{
		\item{\code{country}: country identifier (string).}
		\item{\code{conum}: country identifier (numeric).}
		\item{\code{year}: year identifier.}
		
		Dependent variable:
		\item{kaopen: capital account openness. Higher value indicates more open capital account.}
		
		Diffusion variables, constructed to be a weighted average of the lagged outcome variable for all countries in a given government's \dQuote{neighborhood}. Proximity is defined via a weight matrix W such that the diffusion variable is constructed as $WY_{t-1}$. See pages 18-20 of Brook and Kurtz (2012) for details.
		\item{\code{ldiffpeer}: All countries. historical legacies diffusion variable where country-pairs with similar import-substituting industrialization legacies have higher weights.}
		\item{\code{ldiffisi}: ISI PEER.}
		\item{\code{ldiffgrowth}: SUCCESS.}
		\item{\code{ldiffinflation}:  INFLATION PERFORMANCE.}
		\item{\cpde{ldiffneg}: NEGATIVE LEARNING negative learning diffusion variable, where .}
		\item{\code{ldiffembi}: COMPETITION.}
		\item{\code{limf}: IMF at time \emph{t}-1}
		
		% move limf in the data. rename the diffusion variables
		
		Controls:
		\item{\code{isi_objective}: dummy variable for country-years with import-substituting industrialization (ISI) policy.}
		\item{\code{partisan}: partisanship coded as 0 for leftist, 1 for centrist, and 2 for conservative governments.}
		\item{\code{checks}: checks on authority.}
		\item{\code{y1995}: dummy for year 1995.}
		\item{\code{lngdpcap}: log of GDP per capita.}
		\item{\code{lngdp}: log of GDP.}
		\item{\code{timetrend}: linear time trend.}
		\item{\code{brk}: dummy for structural break in the data source for \code{kaopen} in 1996.}
		\item{\code{lusffr}: US federal funds rate at time \emph{t}-1.}
		\item{\code{linflation}: log of inflation at time \emph{t}-1.}
		\item{\code{lbankra}: bank reserve-to-asset ratio at time \emph{t}-1.}
		\item{\code{lcab}: current account deficit at time \emph{t}-1.}
		\item{\code{lgrowth}: growth of per capita GDP at time \emph{t}-1.}
		\item{\code{ltradebalance}: trade deficit at time \emph{t}-1.}
	}
	}

\source{Brooks, Sarah M. and Marcus J. Kurtz. 2012. \dQuote{Paths to Financial Policy Diffusion: Statist Legacies in Latin America's Globalization.} \emph{International Organization} 66:95-128.}

%\references{
%}

\seealso{\code{\link{panelAR}} for examples which use \code{BrookKurtz}.}