##### Comparing auto.arima() and ets() on seasonal dat #####

library(tidyverse)
library(fpp2)

##### because the series is relatively long, we can afford to use a training and a test set rather than 
##### time series cross-validatio



# Consider the qcement data beginning in 1988
cement <- window(qcement, start=1988)
# Use 20 years of the data as the training set
train <- window(cement, end=c(2007,4))

##### ARIMA model selected and estimated by auto.arima()
(fit.arima <- auto.arima(train))
checkresiduals(fit.arima)

##### ETS model selected and estimated by ets()
(fit.ets <- ets(train))
checkresiduals(fit.ets)


###### below evaluates the forecasting performance of the two competing models over the test set - in this case 
##### the ETS model seems to be the slightly more accurate model based on the test set RMSE, MAPE and MASE
# generate forecasts and compare accuracy over the test set
a1 <- fit.arima %>% forecast(h = 4*(2013-2007)+1) %>%
  accuracy(qcement)
a1[,c("RMSE","MAE","MAPE","MASE")]

a2 <- fit.ets %>% forecast(h = 4*(2013-2007)+1) %>%
  accuracy(qcement)
a2[,c("RMSE","MAE","MAPE","MASE")]
##### notice that the ARIMA model fits the training data slightly better than the ETS model, but that the ETS model 
##### provides more accurate forecasts on the test set - a good fit to training data is never an indication that the 
##### model will forecast well

# Generate forecasts from an ETS model
cement %>% ets() %>% forecast(h=12) %>% autoplot()
