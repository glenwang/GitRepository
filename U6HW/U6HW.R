library(readxl)
library(ggplot2)
library(cowplot)
library(reshape)
library(ggrepel)
#library(xts)
library(forecast)
library(tseries)
library(lubridate)
#library(devtools)
library(readxl)
library(tfplot)
library(lmtest)
library(FredR)
library(rugarch)
library(fGarch)

#install_github("jcizel/FredR") https://github.com/jcizel/FredR

fred <- FredR('4aee7f876d9a432b93ad887a7535c050')

TotalVehicleSales <- ts(fred$series.observations(series_id = 'TOTALNSA')[,c('date','value')][,-1], 
                        start=c(1976,1), frequency = 12)
AnalyzeSeries(diff(TotalVehicleSales, differences=1))


RetailSales <- ts(fred$series.observations(series_id = 'RETAILSMNSA')[,c('date','value')][,-1], 
                        start=c(1992,1), frequency = 12)
AnalyzeSeries(diff(RetailSales, differences=2))


ChinaCPI <- ts(fred$series.observations(series_id = 'CHNCPIALLQINMEI')[,c('date','value')][,-1], 
                  start=c(1993,1), frequency = 4)
AnalyzeSeries(diff(ChinaCPI, differences=4))

USGDP <- ts(fred$series.observations(series_id = 'GDPCA')[,c('date','value')][,-1], 
               start=c(1929), frequency = 1)
AnalyzeSeries(diff(USGDP, differences=7))

USGDPpc <- ts(fred$series.observations(series_id = 'A191RL1A225NBEA')[,c('date','value')][,-1], 
            start=c(1930), frequency = 1)
AnalyzeSeries(diff(USGDPpc, differences=1))

####
hmwk_week_6_2 <- read_csv("hmwk week 6_2.dat", 
                          col_types = cols(R = col_number()))
TS=ts(hmwk_week_6_2)
AnalyzeSeries(TS)


g=garchFit(formula=~garch(2,2), data=TS)


AICdf=data.frame()
for (p in seq(1,9)){
  aicRow=c()
  for (q in seq(1,9)){
    m=garch(TS, order=c(p,q))
    aic=AIC(m)
    #aic=AIC(m,  k=log(length(TS)))
    aicRow=c(aicRow, aic)
  }
  AICdf=rbind(AICdf,aicRow)
}
AICdf

AICdf=data.frame()
for (p in seq(2,9)){
  aicRow=c()
  for (q in seq(0,0)){
    st=paste('garchFit(~garch(',p, ',',q,'),TS)')
    m=eval(parse(text=st))
    aic=(m@fit)$ics[1]
    #aic=AIC(m,  k=log(length(TS)))
    aicRow=c(aicRow, aic)
  }
  AICdf=rbind(AICdf,aicRow)
}
AICdf

m=garch(TS, order=c(0,1))
AIC(m)
AIC(m, k=log(length(m)))

gspec.ru <- ugarchspec(mean.model=list(
  armaOrder=c(0,0)), distribution="std")
gfit.ru <- ugarchfit(gspec.ru, TS)
AIC(gfit.ru)
summary(gfit.ru)


fit = garchFit(~garch(1,2), TS) 
(fit@fit)$ics[3]

arma.sim<-arima.sim(model=list(ar=c(.9,-.6, .5),ma=c(-.7), order=c(3, 2, 1)),n=100)
plot(arma.sim )
adf.test(diff(arma.sim, differences = 2))
dif2=diff(arma.sim, differences = 2)
md=Arima(dif2,order=c(0,0,1))
AnalyzeModel(md)
md2=Arima()
