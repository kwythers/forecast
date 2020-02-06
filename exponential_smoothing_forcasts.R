##### Exponential Smoothing #####

library(tidyverse)
library(fpp2)


##### no seasonality
# annual sheep livestock numbers in Asia (in million head) 
autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

# use time series cross-validation to compare the one-step forecast accuracy of the three methods.
e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)
# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

fc <- holt(livestock, damped=TRUE)
# estimated parameters:
fc[["model"]]

# forecasting livestock, sheep in Asia: comparing forecasting performance of non-seasonal method
autoplot(fc) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

##### with seasonality #####
##### Holt-Winters’ seasonal method
# forecasting international visitor nights in Australia using the Holt-Winters method with both additive and 
# multiplicative seasonality
aust <- window(austourists,start = 2005)
fit1 <- hw(aust,seasonal = "additive")
fit2 <- hw(aust,seasonal = "multiplicative")
autoplot(aust) +
  autolayer(fit1, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fit2, series = "HW multiplicative forecasts",
            PI = FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title = "Forecast"))

##### Holt-Winters method with daily data #####
##### the Holt-Winters method can also be used for daily type of data, where the seasonal period is m=7, and the 
##### appropriate unit of time for h is in days - here, we generate daily forecasts for the last five weeks for the 
##### hyndsight data, which contains the daily pageviews on the Hyndsight blog for one year starting April 30, 2014
fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
         damped = TRUE, seasonal = "multiplicative", h = 35)
autoplot(hyndsight) +
  autolayer(fc, series="HW multi damped", PI = FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))

