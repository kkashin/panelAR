\name{WhittenWilliams}
\alias{WhittenWilliams}

\title{Whitten and Williams (2011) Replication Data}

\description{Replication data for Whitten and Williams (2011). Data structure is panels of 19 advanced democracies from 1945-2000. Data contains measurements of military spending as a percentage of GDP as well as other variables.}

\usage{WhittenWilliams}

\format{A dataframe with the following variables: 
	\itemize{
		\item{\code{ccode}: country identifier (Correlates of War).}
		\item{\code{year}: year identifier.}
		\item{\code{milex_gdp}: military expenditures as a percentage of GDP.}
 		\item{\code{lag_milex_gdp}: military expenditures as a percentage of GDP for t-1.}
        \item{\code{GOV_rl}: left-right position of government.}
        \item{\code{GOV_welfare}: government welfare position.}
        \item{\code{GOV_hawk1}: government international position.}
        \item{\code{gthreat}: conflict involvement index.}
        \item{\code{gthreat_GOV_rl}: conflict involvement interacted with left-right position of government.}
        \item{\code{gthreat_GOV_welfare}: conflict involvement interacted with government welfare position.}
        \item{\code{gthreat_GOV_hawk1}: conflict involvement interacted with government international position.}
        \item{\code{GOV_min}: percentage of year that country had a minority government.}
        \item{\code{GOV_npty}: average number of government parties.}
        \item{\code{election_yr}: dummy variable for election year (1=election year).}
        \item{\code{cinc}: Composite Index of National Capabilities (CINC).}           
        \item{\code{alliance}: alliance with US.}
        \item{\code{real_GDP_gr}: growth in real GDP per capita.}
        \item{\code{lag_real_GDP_gr}: growth in real GDP per capita for t-1.}
        \item{\code{cinclag}: CINC for t-1.}
        \item{\code{cinc_ratio}: ratio of US to Soviet CINC scores.}
        \item{\code{us_change_milex_gdp}: growth in US defense spending as a fraction of GDP.}
        \item{\code{lag_alliance}: alliance with US for t-1.}
        \item{\code{lag_us_change_milex_gdp}: growth in US defense spending as a fraction of GDP for t-1.}
        \item{\code{lag_cinc_ratio}: ratio of US to Soviet CINC scores for t-1.}    
	}
}

\source{Whitten, Guy D. and Laron K. Williams. 2011. \dQuote{Buttery Guns and Welfare Hawks: The Politics of Defense Spending in Advanced Industrial Democracies.} \emph{American Journal of Political Science} 55(1): 117-134.}

%\references{
%}

\seealso{\code{\link{panelAR}} for examples which use \code{WhittenWilliams}.}