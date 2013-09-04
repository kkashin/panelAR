# data
load("~/Desktop/panelFit/R/panelFit/data/BrookKurtz.RData")

### Table 1 

# Model 1: 
out <- panelFit(kaopen ~ ldiffpeer + ldiffisi + ldiffgrowth + ldiffinflation  + ldiffneg + ldiffembi + limf + isi_objective + partisan + checks +  lusffr + linflation + lbankra + lcab + lgrowth +  ltradebalance + lngdpcap + lngdp + brk + timetrend + y1995, data=BrookKurtz, panelVar="country", timeVar="year", autoCorrType="psar", panelCorrType="het", method="GLS",rho.na.action="omit", panel.weight="t", seq.times=TRUE)
summary(out)

# Model 2: 
out2 <- panelFit(kaopen ~ ldiffisi + ldiffgrowth + ldiffinflation  + ldiffneg + ldiffembi + limf + isi_objective + partisan + checks +  lusffr + linflation + lbankra + lcab + lgrowth +  ltradebalance + lngdpcap + lngdp + brk + timetrend + y1995, data=BrookKurtz, panelVar="country", timeVar="year", autoCorrType="psar", panelCorrType="het", method="GLS",rho.na.action="omit", panel.weight="t", seq.times=TRUE)
summary(out2)