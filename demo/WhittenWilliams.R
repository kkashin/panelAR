load("/Users/Kostya/Desktop/panelFit/R/panelFit/data/WhittenWilliams.RData")

### Table 3:
## Additive One-Dimensional Model
summary(panelFit(milex_gdp~lag_milex_gdp+GOV_rl+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorrType="psar", panelCorrType="pcse", method="OLS", complete.case=TRUE))

## Additive Two-Dimensional Model
summary(panelFit(milex_gdp~lag_milex_gdp+GOV_welfare+GOV_hawk1+gthreat+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorrType="psar", panelCorrType="pcse", method="OLS", complete.case=TRUE))

### Table 4:
## Interactive One-Dimensional Model
summary(panelFit(milex_gdp ~ lag_milex_gdp+GOV_rl+gthreat+gthreat_GOV_rl+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorrType="psar", panelCorrType="pcse", method="OLS", complete.case=TRUE))

## Interactive Two-Dimensional Model
summary(panelFit(milex_gdp ~ lag_milex_gdp+GOV_welfare+GOV_hawk1+gthreat+gthreat_GOV_welfare+gthreat_GOV_hawk1+GOV_min+GOV_npty+election_yr+lag_real_GDP_gr+cinclag+lag_alliance+lag_cinc_ratio+lag_us_change_milex_gdp, data=WhittenWilliams, panelVar="ccode", timeVar="year", autoCorrType="psar", panelCorrType="pcse", method="OLS", complete.case=TRUE))