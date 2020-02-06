##### dynamic forcsting with ARIMA models #####

library(tidyverse)
library(fpp2)

# residuals diagnostics for a dynamic regression model for daily electricity demand with workday and quadratic 
# temperature effects
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)
##### the model has some significant autocorrelation in the residuals, which means the prediction intervals may not 
##### provide accurate coverage - also, the histogram of the residuals shows one positive outlier, which will also 
##### affect the coverage of the prediction intervals

##### using the estimated model we forecast 14 days ahead starting from Thursday 1 January 2015 (a non-work-day being a 
##### public holiday for New Years Day) - in this case, we could obtain weather forecasts from the weather bureau for 
##### the next 14 days - for the sake of illustration, we will use scenario based forecasting where we set the temperature 
##### for the next 14 days to a constant 26 degrees
fcast <- forecast(fit,
                  xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14), 
                               Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
# forecasts from the dynamic regression model for daily electricity demand
autoplot(fcast) + ylab("Electricity demand (GW)")
