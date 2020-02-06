##### Forecasting on training and test sets #####

library(tidyverse)
library(fpp2)

##### multi-step forecasts on training data

##### normally we define fitted values to be one-step forecasts on the training set, but a similar idea can be used for 
##### multi-step forecasts - we will illustrate the method using an ARIMA(2,1,1)(0,1,2)12 model for the Australian eating-out 
##### expenditure - the last five years are used for a test set

training <- subset(auscafe, end=length(auscafe)-61)
test <- subset(auscafe, start=length(auscafe)-60)
cafe.train <- Arima(training, order=c(2,1,1),
                    seasonal=c(0,1,2), lambda=0)
cafe.train %>%
  forecast(h=60) %>%
  autoplot() + autolayer(test)

##### the fitted() function has an h argument to allow for h-step “fitted values” on the training set - below is a plot 
##### of 12-step (one year) forecasts on the training set - because the model involves both seasonal (lag 12) and 
##### first (lag 1) differencing, it is not possible to compute these forecasts for the first few observations
autoplot(training, series="Training data") +
  autolayer(fitted(cafe.train, h=12),
            series="12-step fitted values")

##### it is common practice to fit a model using training data, and then to evaluate its performance on a test data set - the 
##### way this is usually done means the comparisons on the test data use different forecast horizons - in the above example, 
##### we have used the last sixty observations for the test data, and estimated our forecasting model on the training data - 
##### then the forecast errors will be for 1-step, 2-steps, …, 60-steps ahead - the forecast variance usually increases with 
##### the forecast horizon, so if we are simply averaging the absolute or squared errors from the test set, we are combining 
##### results with different variances

##### one solution to this issue is to obtain 1-step errors on the test data - that is, we still use the training data to 
##### estimate any parameters, but when we compute forecasts on the test data, we use all of the data preceding each 
##### observation (both training and test data). So our training data are for times 1,2,…,T−60 - we estimate the model on 
##### these data, but then compute ^yT−60+h|T−61+h, for h=1,…,T−1 - because the test data are not used to estimate the 
##### parameters, this still gives us a “fair” forecast - for the ets(), Arima(), tbats() and nnetar() functions, these 
##### calculations are easily carried out using the model argument

# using the same ARIMA model used above, we now apply the model to the test data
cafe.test <- Arima(test, model=cafe.train)
accuracy(cafe.test)
##### note that Arima() does not re-estimate in this case - instead, the model obtained previously (and stored as cafe.train) 
##### is applied to the test data. Because the model was not re-estimated, the “residuals” obtained here are actually one-step 
##### forecast errors - consequently, the results produced from the accuracy() command are actually on the test set (despite 
##### the output saying “Training set”)

