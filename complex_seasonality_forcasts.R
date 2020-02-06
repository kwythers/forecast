##### Complex seasonality #####

library(tidyverse)
library(fpp2)

p1 <- autoplot(calls) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(breaks=seq(1,33,by=2))
p2 <- autoplot(window(calls, end=4)) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(minor_breaks = seq(1,4,by=0.2))
gridExtra::grid.arrange(p1,p2)

# multiple STL for the call volume data
calls %>% mstl() %>%
  autoplot() + xlab("Week")
##### two seasonal patterns shown, one for the time of day (the third panel), and one for the time of week (the fourth panel)

##### the decomposition can also be used in forecasting, with each of the seasonal components forecast using a seasonal 
##### naïve method, and the seasonally adjusted data forecasting using ETS (or some other user-specified method) - the stlf() 
##### function will do this automatically
calls %>%  stlf() %>%
  autoplot() + xlab("Week")


##### forecasts from a dynamic harmonic regression applied to the call volume data
fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
                  xreg=fourier(calls, K=c(10,10)))
fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")
##### a large model, containing 40 parameters: 4 ARMA coefficients, 20 Fourier coefficients for frequency 169, 
##### and 16 Fourier coefficients for frequency 845

##### TBATS models
##### as with any automated modelling framework, there may be cases where it gives poor results, but it can be a useful approach 
##### in some circumstances
##### TBATS model differs from dynamic harmonic regression in that the seasonality is allowed to change slowly over time
calls %>%
  subset(start=length(calls)-2000) %>%
  tbats() -> fit2
fc2 <- forecast(fit2, h=2*169)
autoplot(fc2, include=5*169) +
  ylab("Call volume") + xlab("Weeks")
##### the prediction intervals appear to be much too wide – something that seems to happen quite often with 
##### TBATS models unfortunately
##### TBATS models do not allow for covariates, although they can be included in dynamic harmonic regression models

##### Complex seasonality with covariates #####
# half-hourly electricity demand and corresponding temperatures in 2014, Victoria, Australia
autoplot(elecdemand[,c("Demand","Temperature")],
         facet=TRUE) +
  scale_x_continuous(minor_breaks=NULL,
                     breaks=2014+
                       cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))/365,
                     labels=month.abb) +
  xlab("Time") + ylab("")
# half-hourly electricity demand for Victoria, plotted against temperatures for the same times in Melbourne, 
# the largest city in Victoria
elecdemand %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) + geom_point() +
  xlab("Temperature (degrees Celsius)") +
  ylab("Demand (GW)")

# fit a regression model with a piecewise linear function of temperature (containing a knot at 18 degrees), 
# and harmonic regression terms to allow for the daily seasonal pattern
cooling <- pmax(elecdemand[,"Temperature"], 18)
fit <- auto.arima(elecdemand[,"Demand"],
                  xreg = cbind(fourier(elecdemand, c(10,10,0)),
                               heating=elecdemand[,"Temperature"],
                               cooling=cooling))
##### forecasting with such models is difficult because we require future values of the predictor variables - future values 
##### of the Fourier terms are easy to compute, but future temperatures are, of course, unknown - if we are only interested 
##### in forecasting up to a week ahead, we could use temperature forecasts obtain from a meteorological model - alternatively, 
##### we could use scenario forecasting and plug in possible temperature patterns - here I have used a repeat of the last 
##### two days of temperatures to generate future possible demand values
# forecasts from a dynamic harmonic regression model applied to half-hourly electricity demand data
temps <- subset(elecdemand[,"Temperature"],
                start=NROW(elecdemand)-2*48+1)
fc <- forecast(fit,
               xreg=cbind(fourier(temps, c(10,10,0)),
                          heating=temps, cooling=pmax(temps,18)))
autoplot(fc, include=14*48)
##### the short-term forecasts look reasonable, this is a crude model for a complicated process - the residuals demonstrate that 
##### there is a lot of information that has not been captured with this model
checkresiduals(fc)
