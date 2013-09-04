load("/Users/Kostya/Desktop/panelFit/R/panelFit/data/LupPon.RData")
LupPon.sub <- LupPon[!is.na(LupPon$redist),]

LupPon.sub$redist.lag <- unlist(by(LupPon.sub,LupPon.sub$id,function(x){
	lag.var <- c(NA,x[,"redist"][1:(length(x[,"redist"])-1)])
	lag.var
	}))

LupPon.sub$time <- unlist(by(LupPon.sub,LupPon.sub$id,function(x) seq(1:nrow(x))))

### Table 2

# Specification 1 
out <- panelFit(redist ~ redist.lag + dvpratio9050 + dvpratio5010 + dvturnout + dvfempar + dvstddisp_gall + dvpvoc + dvunion + dvunempl, data=LupPon.sub, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
summary(out)

# Specification 2 (remove outliers)
mod1.resid <- out$residuals
index <- which(abs((mod1.resid-mean(mod1.resid))/sd(mod1.resid)) <= 1.5)
LupPon.nooutlier <- out$model[index,]

out2 <- panelFit(redist ~ redist.lag + dvpratio9050 + dvpratio5010 + dvturnout + dvfempar + dvstddisp_gall + dvpvoc + dvunion + dvunempl, data=LupPon.nooutlier, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="het", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
summary(out2)$rho
summary(out2)
plot(out2)
runs.analysis(out2)
runs.analysis(out2)$runs # row.names are units, start, end, and length of each run

# Specification 3
out3 <- panelFit(redist ~ dvpratio9050 + dvpratio5010 + as.factor(id), data=LupPon.sub, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
summary(out3)

# Specification 4
mod3.resid <- out3$residuals
index <- which(abs((mod3.resid-mean(mod3.resid))/sd(mod3.resid)) <= 1.5)
LupPon.nooutlier <- out3$model[index,]

summary(panelFit(redist ~ dvpratio9050 + dvpratio5010 + as.factor(id), data=LupPon.nooutlier, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE))

# Specification 5
out5 <- panelFit(redist ~ redist.lag + dvratio9010 + dvskew + dvturnout + dvfempar + dvstddisp_gall + dvpvoc + dvunion + dvunempl, data=LupPon.sub, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
summary(out5)

# Specification 6
mod5.resid <- out5$residuals
index <- which(abs((mod5.resid-mean(mod5.resid))/sd(mod5.resid)) <= 1.5)
LupPon.nooutlier <- out5$model[index,]

summary(panelFit(redist ~ redist.lag + dvratio9010 + dvskew + dvturnout + dvfempar + dvstddisp_gall + dvpvoc + dvunion + dvunempl, data=LupPon.nooutlier, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE))

# Specification 7
out7 <- panelFit(redist ~ dvratio9010 + dvskew + as.factor(id), data=LupPon.sub, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE)
summary(out7)

# Specification 8
mod7.resid <- out7$residuals
index <- which(abs((mod7.resid-mean(mod7.resid))/sd(mod7.resid)) <= 1.5)
LupPon.nooutlier <- out7$model[index,]

summary(panelFit(redist ~ dvratio9010 + dvskew + as.factor(id), data=LupPon.sub, panelVar="id", timeVar="time", autoCorrType="common", panelCorrType="pcse", method="OLS",rho.na.action="omit", panel.weight="t-1", bound.rho=TRUE))