install.packages("Quandl")
library(Quandl)
## part a 
library(Quandl)
ngd <- Quandl("EIA/AEO_2016_REF_NO_CPP_PRCE_NA_COMM_NA_NG_NA_SATL_Y13DLRPMCF_A", api_key="otkvu4KBWBAykqK83Zxi", type="ts")
str(ngd)
head(ngd)
## part b 
library(ggplot2)
library(forecast)

autoplot(ngd)

ndiffs(ngd)
ggtsdisplay(diff(ngd))
##part c

ngd.ar <- auto.arima(ngd)
summary(ngd.ar)
checkresiduals(ngd.ar)

##partd

ngd110 <- Arima(ngd, order = c(2, 1, 1))
checkresiduals(ngd110)

f.ngd110 <- forecast(ngd110, h = 4)
autoplot(f.ngd110)

##part e
ngd.ets <- ets(ngd)
summary(ngd.ets)
autoplot(ngd.ets)

## part f
checkresiduals(ngd.ets)

##part g 
f.ngd.ets <- forecast(ngd.ets, h = 4)
autoplot(f.ngd.ets)

##part h 
f.ngd.ets <- forecast(ngd.ets, h = 4)
autoplot(f.ngd.ets)






