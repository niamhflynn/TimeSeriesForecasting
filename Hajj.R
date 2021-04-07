install.packages("fma")
require(fma)
library("forecast")

HajjCSV = read.csv("C:/Users/Niamh/OneDrive/Documents/Forecasting/Hajj.csv", header=TRUE)
Hajj= ts(HajjCSV[,2], frequency = 12, start = c(2008,02), end = c(2018,01))

#VISUALIZAION
tsdisplay(Hajj)
seasonplot(Hajj)
plot(decompose(Hajj))
decompose(Hajj)
hist(decompose(Hajj)$random)

#HOLT WINTERS
#SES HOLT WINTERS
HoltWinters(Hajj, beta = F, gamma = F)
SES_predict = predict(HoltWinters(Hajj, beta = F, gamma = F), n.ahead = 24)
ts.plot(Hajj, SES_predict)

#DES HOLT WINTERs
HoltWinters(Hajj, gamma = F)
DES_predict = predict(HoltWinters(Hajj, beta = F, gamma = F), n.ahead = 24)
ts.plot(Hajj, DES_predict)
#both are obviously unsuitable, as cannot handle trend or seasonality

#SWH+
HoltWinters(Hajj)
SWHADD_predict = predict(HoltWinters(Hajj),n.ahead = 24)
ts.plot(Hajj, SWHADD_predict)

#SHWx
HoltWinters(Hajj, seasonal = "multiplicative")
SWHMULT_predict = predict(HoltWinters(Hajj, seasonal = "multiplicative"))
ts.plot(Hajj, SWHMULT_predict)

#WHICH FITS BETTER?
HoltWinters(Hajj)$SSE
HoltWinters(Hajj, seasonal = "multiplicative")$SSE
#ans: ADDITIVE SHW

HajjForecast = forecast(HoltWinters(Hajj))
plot.ts(HajjForecast$residuals)
hist(HajjForecast$residuals)
plot(HoltWinters(Hajj))

#ARIMA
auto.arima(Hajj)
tsdisplay(auto.arima(Hajj)$residuals)
plot(aggregate(Hajj, FUN = var))
#residuals get bigger with time, use transformation

Hajj_SquaredSeries= sqrt(Hajj)
Hajj_cubedSeries= (Hajj)^1/3
Hajj_logSeries =log(Hajj)
Hajj_negInverseSeries = (Hajj)^-1*1

plot(aggregate(Hajj_SquaredSeries, FUN = var))
plot(aggregate(Hajj_cubedSeries, FUN = var))
plot(aggregate(Hajj_logSeries, FUN = var))
plot(aggregate(Hajj_negInverseSeries, FUN = var))

#Neg Inverse smallest residual Variance
#Therefore use neg inverse in arima model
arima1 = arima(Hajj_negInverseSeries, order = c(0,0,0), seasonal = list(order = c(0,0,0), period =12))
tsdisplay(arima1$residuals, main = "Arima 1: (0,0,0)(0,0,0)[12]")

#apply non seasonal differencing to tackle trend
arima2 = arima(Hajj_negInverseSeries, order = c(0,1,0), seasonal = list(order = c(0,0,0), period =12))
tsdisplay(arima2$residuals, main = "Arima 2: (0,1,0)(0,0,0)[12]")


#autocorrelation at 12,24,36 is positive, adding SAR term to model, DO NOT MIX SAR AND SMA
arima3 = arima(Hajj_negInverseSeries, order = c(0,1,0), seasonal = list(order = c(1,0,0), period =12))
tsdisplay(arima3$residuals, main = "Arima 3: (0,1,0)(1,0,0)[12]")

#still strong ACF 12,24,36- increase to SAR(2)

#autocorrelation at 12,24,36 is positive, adding SAR term to model, DO NOT MIX SAR AND SMA
#will see changes in seasonality, if needs be will come back to differencing, dont want to over difference
arima4 = arima(Hajj_negInverseSeries, order = c(0,1,0), seasonal = list(order = c(2,0,0), period =12))
tsdisplay(arima4$residuals, main = "Arima 4: (0,1,0)(2,0,0)[12]")
arima4$aic

#still peak at 12, 36- no peak at 24. Think sar(2) is satisfactory but test SAR(3)

#autocorrelation at 12,24,36 is positive, adding SAR term to model, DO NOT MIX SAR AND SMA
#will see changes in seasonality, if needs be will come back to differencing, dont want to over difference
TESTSAR3 = arima(Hajj_negInverseSeries, order = c(0,1,0), seasonal = list(order = c(2,0,0), period =12))
tsdisplay(TESTSAR3$residuals, main = "Arima 3: (0,1,0)(2,0,0)[12]")
TESTSAR3$aic
#disimproves AIC, SAR(2) fits better

#ACF has peak at 12, then zero- indicative of MA model
arima5 = arima(Hajj_negInverseSeries, order = c(0,1,1), seasonal = list(order = c(2,0,0), period =12))
tsdisplay(arima5$residuals, main = "Arima 5: (0,1,1)(2,0,0)[12]")

#still slight peak at 12,24 - MA(2) model
arima6 = arima(Hajj_negInverseSeries, order = c(0,1,2), seasonal = list(order = c(2,0,0), period =12))
tsdisplay(arima6$residuals, main = "Arima 6: (0,1,2)(2,0,0)[12]")
arima6$aic

#This is my final arima model, will now look at autoarima 
auto = auto.arima(Hajj_negInverseSeries)
plot(auto.arima(Hajj_negInverseSeries))

BIC(arima6)
arima6$aic
auto

#exmaining fit of arima models 
hist(arima6$residuals)
arima6Forecast = forecast(arima6)
plot(arima6Forecast)
BIC(arima6)
arima6$aic
auto$aic 
BIC(auto)

predictedVal = arima6Forecast$mean*((arima6Forecast$mean^-1)^2)
plot(predictedVal)
plot(forecast(arima6, h = 24))
