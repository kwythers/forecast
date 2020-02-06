##### Seasonal ARIMA models #####

library(tidyverse)
library(fpp2)

##### european quarterly retail trade 
# plot the data
autoplot(euretail) + ylab("Retail index") + xlab("Year")
##### the data are clearly non-stationary, with some seasonality 

# so first take a seasonal difference
euretail %>% diff(lag=4) %>% ggtsdisplay()
# double differenced European retail trade index
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()

##### find an appropriate ARIMA model based on the ACF and PAC
##### the significant spike at lag 1 in the ACF suggests a non-seasonal MA(1) component, and the significant spike at 
##### lag 4 in the ACF suggests a seasonal MA(1) component - consequently, we begin with an ARIMA(0,1,1)(0,1,1)4 model, 
##### indicating a first and seasonal difference, and non-seasonal and seasonal MA(1) components
euretail %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
##### by analogous logic applied to the PACF, we could also have started with an ARIMA(1,1,0)(1,1,0)4 model
euretail %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
##### both the ACF and PACF show significant spikes at lag 2, and almost significant spikes at lag 3
##### smallest ACF - ARIMA(0,1,3)(0,1,1)4 model
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)


##### now we have a seasonal ARIMA model that passes the required checks and is ready for forecasting
fit3 %>% forecast(h=12) %>% autoplot()

##### in this case, auto.arima(), would have given the same result
auto.arima(euretail)

##### more difficult - forecast monthly corticosteroid drug sales in australia
# plot the data
lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales"=lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("")

# seasonally differenced corticosteroid drug sales in australia
lh02 %>% diff(lag=12) %>%
  ggtsdisplay(xlab="Year",
              main="Seasonally differenced H02 scripts")
##### the plots of the seasonally differenced data, there are spikes in the PACF at lags 12 and 24, but nothing at 
##### seasonal lags in the ACF - this may be suggestive of a seasonal AR(2) term - in the non-seasonal lags, there are 
##### three significant spikes in the PACF, suggesting a possible AR(3) term - the pattern in the ACF is not indicative 
##### of any simple model

##### all the spikes are now within the significance limits, so the residuals appear to be white noise - the Ljung-Box test 
##### also shows that the residuals have no remaining autocorrelations
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)
# forecasts from the ARIMA(3,0,1)(0,1,2)12 model applied to the H02 monthly script sales data
h02 %>%
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")

