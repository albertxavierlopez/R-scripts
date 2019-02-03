######  Time series analysis ######
####################################

# Prediction with ARIMA(p,d,q) models: the goal is to discover the best parameters (p,d,q) to fit the ARIMA model.
# These examples are made with simulated time series WITHOUT SEASONALITY !!! 
# For exact explanations on how to identify (p,d,q) -> https://people.duke.edu/~rnau/411arim.htm
#                                                   -> http://people.duke.edu/~rnau/Slides_on_ARIMA_models--Robert_Nau.pdf



# Let's see a simualted AR(1) time series:

data<-arima.sim(model=list(ar=.7),n=120)           # simulate a time series AR(1) of 120 observations
ts.plot(data,col=4)                                # plot it just to see it

    # How can we know the ARIMA(p,q,r) type? By analysing the ACF and PACF.
    library(TSA)
    par(mfrow=c(1,1))
    acf(data)                                      # here we can see the correlation in the second period, so AR(1)
    pacf(data)

fit<-arima(data,c(1,0,0))                          # fit an ARIMA model with form AR(1)
ts.plot(data,fit$residuals, col=c(1,2))            # the ARIMA(1,0,0) over the simulated sime series and we can see we almost nailed.

predictions<-predict(fit,5)                        # prediction for the next 5 periods
ts.plot(data,predictions$pred, col=c(1,2))         # prediciton plot

    
    
# Now let's do the same with AR(2)
    
data2<-arima.sim(model=list(ar=c(.7,-0.3)),n=120)
ts.plot(data2,col=4)
    
    # How can we know the ARIMA(p,q,r) type? By analysing the ACF and PACF.
    par(mfrow=c(1,1))
    acf(data2)                                     # here we can see the correlation in the second period, so AR(2)
    pacf(data2)


fit2<-arima(data2,c(2,0,0))                        # fit an ARIMA model with form AR(1)
ts.plot(data2,fit2$residuals, col=c(1,2))

predictions2<-predict(fit2,5)                      # prediction for the next 5 periods
ts.plot(data2,predictions2$pred, col=c(1,2)) 

    
# Now let's do the same with MA(1)
    
data3<-arima.sim(model=list(ma=.5),n=120)
ts.plot(data3,col=4)

    # How can we know the ARIMA(p,q,r) type? By analysing the ACF and PACF.
    par(mfrow=c(1,1))
    acf(data3)                                     # see the tips from the website
    pacf(data3)


fit3<-arima(data3,c(0,0,1))                        # fit an ARIMA model with form MA(1)
ts.plot(data3,fit3$residuals, col=c(1,2))          # residuals from the model should match the time series

predictions3<-predict(fit3,5)                      # prediction for the next 5 periods
ts.plot(data3,predictions3$pred, col=c(1,2)) 



# Example with real data from Car accidents between 1973-1979.

accidents<-ts(read.table("accidents.dat"),start=1973,frequency=12)
ts.plot(accidents)

# Regular differences to eliminate the seasonality/tendency
dserie<-diff(accidents)
par(mfrow=c(1,1))
plot(dserie,col=4)

par(mfrow=c(2,1))
acf(dserie,lag=100)
pacf(dserie,lag=100)

# Seasonality diferences  !!!!!!!!!!! (there was some tendency yet)

ddserie<-diff(dserie,lag=12)
par(mfrow=c(1,1))
plot(ddserie,col=4)

par(mfrow=c(2,1))
acf(ddserie,lag=100)
pacf(ddserie,lag=100)

    # How can we know the ARIMA(p,q,r) type? By analysing the ACF and PACF.
    par(mfrow=c(1,1))
    acf(accidents)                                     # see the tips from the website
    pacf(accidents)
    
fit4<-arima(ddserie,c(12,0,0))
ts.plot(ddserie, fit4$residuals, col=c(1,2))

    # Analysing residuals => ACF and PACF have to be clean (no spikes outside) !!! Only from (12,0,0) and above
    par(mfrow=c(2,1))
    acf(fit4$residuals,lag=100)
    pacf(fit4$residuals,lag=100)

par(mfrow=c(1,1))    
predicitons4<-predict(fit4,5)
ts.plot(ddserie, predicitons4$pred, col=c(4,2))
predicitons4

accidents

library(forecast)  
plot(forecast(Arima(y = accidents, order = c(11,0,0))))   # To undo diferences and get the forecast
forecast(Arima(y = accidents, order = c(3,0,0)))


## Spanish unemployment !!!!!!!!!!!!!!!

paro<-read.csv(".../LRHUTTTTESM156S.csv", header = TRUE)
ts.plot(paro$LRHUTTTTESM156S)

paro<-ts(paro$LRHUTTTTESM156S, start = c(1986, 4), frequency=12)
ts.plot(paro)

dparo<-diff(paro)
ts.plot(dparo)

par(mfrow=c(2,1))
acf(dparo,lag=100)
pacf(dparo,lag=100)

ddparo<-diff(dparo, lag=1)
ts.plot(ddparo)

fit5<-arima(ddparo,c(2,1,1), seasonal = list(order=c(0,0,1)))
ts.plot(ddparo, fit5$residuals, col=c(1,2))

  # Analysing residuals => ACF and PACF have to be clean (no spikes outside) smallest AIC !!! 
  AIC(fit5)
  par(mfrow=c(2,1))
  acf(fit5$residuals,lag=100)
  pacf(fit5$residuals,lag=100)

par(mfrow=c(1,1))    
predicitons5<-predict(fit5,12)
ts.plot(ddparo, predicitons5$pred, col=c(4,2))

library(forecast)  
plot(forecast(Arima(y = paro, order = c(1,1,1),seasonal = list(order=c(0,0,1)))))   # To undo diferences and get the forecast
forecast(Arima(y = paro, order = c(2,1,1)))  

#plot through ggplot
library(ggplot2)
paro %>%
  Arima(order=c(1,1,1), seasonal = c(0,0,1)) %>%
  forecast(h=18) %>%
  autoplot  
