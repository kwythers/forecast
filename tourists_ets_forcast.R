##### Forcasts with ets() #####
##### International tourist visitor nights in Australia

library(tidyverse)
library(fpp2)

aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)

# components of ets(M,A,M) 
# graphical representation of the estimated states over time
autoplot(fit)

# residuals and one-step forecast errors from the ETS(M,A,M) model
cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

# to obtain forecasts from an ETS model, we use the forecast() function
fit %>% forecast(h=8) %>%
  autoplot() +
  ylab("International visitor night in Australia (millions)")
