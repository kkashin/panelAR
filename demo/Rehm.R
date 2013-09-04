load("/Users/Kostya/Desktop/panelFit/R/panelFit/data/Rehm.RData")

### Replication of Rehm (2011), Table 3
### PCSE, Common AR1

## Specification 1
out <- panelFit(NURR ~ gini, data=Rehm, panelVar="ccode", timeVar="year", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
summary(out)

## Specification 2
summary(panelFit(NURR ~ gini + mean_ur, data=Rehm, panelVar="ccode", timeVar="year", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE))

### Specification 9
out <- panelFit(NURR ~ gini + mean_ur + selfemp + cum_right + tradeunion + deficit + tradeopen + gdp_growth, data=Rehm, panelVar="ccode", timeVar="year", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
# table of coefficients
summary(out)