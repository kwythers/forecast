##### quick linear forcast #####

library(tidyverse)
library(fpp)

ausbeer



beer2 <- window(ausbeer, start = 1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres") # dark blue 80%, light blue 95%



