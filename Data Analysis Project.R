library("TSA")
library("tseries")

data = read.csv("C:\\Users\\Don Bosco\\Documents\\MATH 62.2 R Codes\\accidents.csv", header = TRUE)

head(data)

accidents = data[,"Accident_Count"] 

head(accidents)

accidents = ts(accidents)

accidents.plot = plot(accidents)

#Test for stationarity
#H_0: Data is NOT stationary (so we want to reject this)
adf.test(accidents) #not stationary

# perform pre-processing
accidents.ln = log(accidents) # take log transformation
accidents.ln.d = diff(accidents.ln) # take first difference of log

accidents.ln.d = plot(accidents.ln.d)

adf.test(accidents.ln.d) #not stationary

Box.test(accidents, type = "Ljung") # p-value = 1.232e-14 so slay

Box.test(accidents.ln.d, type = "Ljung") # p-value = 0.001474 so slay

#ACF
# Compute for ACF
# The largest values that will exceed the broken lines (the interval) will be the basis for what h we will use
acf(accidents.ln.d) #MA(2) or MA(3)

# Compute for PACF
acf(accidents.ln.d, type="partial") #AR(1) or AR(4)

arima(accidents.ln.d, order=c(1,0,0)) # test AR(1)
arima(accidents.ln.d, order=c(4,0,0)) # test AR(4)

arima(accidents.ln.d, order=c(0,0,2)) # test MA(2)
arima(accidents.ln.d, order=c(0,0,3)) # test MA(3)

arima(accidents.ln.d, order=c(1,0,2)) # test ARMA(1,2)
arima(accidents.ln.d, order=c(1,0,3)) # test ARMA(1,3)
arima(accidents.ln.d, order=c(4,0,2)) # test ARMA(4,2)
arima(accidents.ln.d, order=c(4,0,3)) # test ARMA(4,3)

#We choose ARMA(4,2) as it has the lowest AIC

#install.packages('forecast')
library("forecast")

# from previous result, use ARMA (4, 2)
arma42 = arima(accidents.ln.d, order = c(4, 0, 2))
arma42$coef

# forecast 10-step ahead
arma42.p = predict(arma42, n.ahead =10)

arma42.p$pred #forecast values

arma42.2 = Arima(accidents.ln.d, order=c(4,0,2))
arma42.2
arma10 = Arima(accidents.ln.d, order=c(1,0, 0))
arma10
arma40 = Arima(accidents.ln.d, order=c(4,0, 0))
arma40
arma02 = Arima(accidents.ln.d, order=c(0,0, 2))
arma02
arma12 = Arima(accidents.ln.d, order=c(1,0, 2))
arma12


arma42.2$coef
arma42.p2 = forecast(arma42.2, h=10)
arma42.p2$mean

arma42.2

#residual diagnostics
tsdiag(arma42)
jarque.bera.test(residuals(arma42))

accidents.forecast = c(accidents.ln.d, arma42.p2$mean)
accidents.forecast = ts(accidents.forecast)
plot(accidents.forecast)

