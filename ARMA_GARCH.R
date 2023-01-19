rm(list = ls())

library(quantmod)
library(forecast)
library(fpp2)
library(binhf)
library(dplyr)
library(urca)
library(tseries)
library(ggplot2)
library(rugarch)
library(zoo)
library(SBAGM)
library(moments)

### importing VIX
vix <- as.data.frame(read.csv('vixlogret.csv', row.names='Date'))

vixarima <- auto.arima(vix)

### checking distribution density
plot(density(vixarima$residuals))
kurtosis(vixarima$residuals)

### india
indiavix <- as.data.frame(read.csv('indvix.csv', row.names='Date'))
indiavixlog <- log(indiavix)
indiavixlogret <- apply(indiavixlog, 2, diff)
indiavixlogret
auto.arima(indiavixlogret)

### europe
eurovix <- as.data.frame(read.csv('v2x.csv', row.names='Date'))
eurovixlog <- log(eurovix)
eurovixlogret <- apply(eurovixlog, 2, diff)
eurovixlogret
auto.arima(eurovixlogret)

### hong kong
hkvix <- as.data.frame(read.csv('hkvix.csv', row.names='Date'))
hkvixlog <- log(hkvix)
hkvixlogret <- apply(hkvixlog, 2, diff)
hkvixlogret
auto.arima(hkvixlogret)

model=ugarchspec(variance.model = list(model = "eGARCH",
                                       garchOrder= c(1,1)), 
                 mean.model = list(armaOrder = c(2,1)
                 ), 
                 distribution.model = "sstd")
indiaoutS <- 143
eurooutS <- 100
hkoutS <- 215
outS <- 227

fit=ugarchfit(spec=model,data=vix,
                 out.sample = outS
)
fit
forecast = ugarchforecast(fitORspec = fit, 
                        data=NULL, 
                        n.ahead=1, 
                        n.roll=outS)
mean = fitted(forecast)
write.csv(mean,"vixlogretfore_vx4.csv")

indfit=ugarchfit(spec=model,data=indiavixlogret,
                   out.sample = indiaoutS
                )
indfit
indfor = ugarchforecast(fitORspec = indfit, 
                        data=NULL, 
                        n.ahead=1, 
                        n.roll=indiaoutS)
indmean = fitted(indfor)
write.csv(indmean,"indvixforecast_vo6.csv")

eurfit=ugarchfit(spec=model,data=eurovixlogret,
                 out.sample = eurooutS
)
eurfit
eurfor = ugarchforecast(fitORspec = eurfit, 
                        data=NULL, 
                        n.ahead=1, 
                        n.roll=eurooutS)
eurmean = fitted(eurfor)
write.csv(eurmean,"eurovixforecast_vo6.csv")

hkfit=ugarchfit(spec=model,data=hkvixlogret,
                 out.sample = hkoutS
)
hkfit
hkfor = ugarchforecast(fitORspec = hkfit, 
                        data=NULL, 
                        n.ahead=1, 
                        n.roll=hkoutS)
hkmean = fitted(hkfor)
write.csv(hkmean,"hkvixforecast_vo6.csv")
