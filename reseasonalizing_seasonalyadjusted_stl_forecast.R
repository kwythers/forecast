##### Forecasting using stl objects #####

##### forecasts of STL objects are obtained by applying a non-seasonal forecasting method to the seasonally adjusted 
##### data and re-seasonalizing using the last year of the seasonal component

library(tidyver)
library(forecast)

##### stlm takes a time series y, applies an STL decomposition, and models the seasonally adjusted data using the model 
##### passed as modelfunction or specified using method - it returns an object that includes the original STL decomposition 
##### and a time series model fitted to the seasonally adjusted data - this object can be passed to the forecast.stlm for 
##### forecasting

##### forecast.stlm forecasts the seasonally adjusted data, then re-seasonalizes the results by adding back the last year 
##### of the estimated seasonal component
tsmod <- stlm(USAccDeaths, modelfunction=ar)
plot(forecast(tsmod, h=36))

##### forecast.stl is similar to stlf except that it takes the STL decomposition as the first argument, 
##### instead of the time series
decomp <- stl(USAccDeaths, s.window="periodic")
plot(forecast(decomp))

##### stlf combines stlm and forecast.stlm - it takes a ts argument, applies an STL decomposition, models the seasonally 
##### adjusted data, reseasonalizes, and returns the forecasts - however, it allows more general forecasting methods to be 
##### specified via forecastfunction
plot(stlf(AirPassengers, lambda=0))
