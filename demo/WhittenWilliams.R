user.prompt <- function (x) {
 	ANSWER <- readline(paste("\nType 'y' to continue to", x,"or 'n' to quit: ",sep=" "))  
	if (substr(ANSWER, 1, 1) == "n")
         {stop("Function terminated by user.")}
	}
	
WhittenWilliams <- function(){
	message("Replication of Whitten and Williams (2012)'...")
    
    cat("\n")
    cat("Loading data...\n")
    data(WhittenWilliams)
    cat("> data(WhittenWilliams)\n")	

	# Table 3:
	user.prompt("Additive One-Dimensional Model (Table 3)")
	cat("> out3.1 <- panelAR(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar='ccode', timeVar='year', autoCorr='psar1', panelCorrMethod='pcse', complete.case=TRUE)")
	out3.1 <- panelAR(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", panelCorrMethod="pcse", complete.case=TRUE)
	cat("> summary(out3.1)")
    summary(out3.1)
	
	user.prompt("Additive Two-Dimensional Model (Table 3)")
	cat("> out3.2 <- panelAR(milex_gdp~lag_milex_gdp+GOV_welfare+GOV_hawk1+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar='ccode', timeVar='year', autoCorr='psar1', panelCorrMethod='pcse', complete.case=TRUE)")
	out3.2 <- panelAR(milex_gdp~lag_milex_gdp+GOV_welfare+GOV_hawk1+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", panelCorrMethod="pcse", complete.case=TRUE)
	cat("> summary(out3.2)")
    summary(out3.2)
    
    user.prompt("Interactive Two-Dimensional Model (Table 4)")
    cat("> out4.1 <- panelAR(milex_gdp ~ lag_milex_gdp+GOV_rl+gthreat+gthreat_GOV_rl+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar='ccode', timeVar='year', autoCorr='psar1', panelCorrMethod='pcse', complete.case=TRUE)")
	out4.1 <- panelAR(milex_gdp ~ lag_milex_gdp+GOV_rl+gthreat+gthreat_GOV_rl+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", panelCorrMethod="pcse", complete.case=TRUE)
	cat("> summary(out4.1)")
    summary(out4.1)

    user.prompt("Interactive Two-Dimensional Model (Table 4)")
    cat("> out4.2 <- panelAR(milex_gdp ~ lag_milex_gdp+GOV_welfare+GOV_hawk1+gthreat+gthreat_GOV_welfare+gthreat_GOV_hawk1+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar='ccode', timeVar='year', autoCorr='psar1', panelCorrMethod='pcse', complete.case=TRUE)")
	out4.2 <- panelAR(milex_gdp ~ lag_milex_gdp+GOV_welfare+GOV_hawk1+gthreat+gthreat_GOV_welfare+gthreat_GOV_hawk1+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorr="psar1", panelCorrMethod="pcse", complete.case=TRUE)
	cat("> summary(out4.2)")
    summary(out4.2)
}

WhittenWilliams()