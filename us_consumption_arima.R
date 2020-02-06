##### Non-seasonal ARIMA models #####

library(tidyverse)
library(fpp2)

# quarterly percentage change in US consumption expenditure
autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")

# the following R code selects a model automatically
fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE)

# forecasts of quarterly percentage changes in US consumption expenditure
fit %>% forecast(h=10) %>% autoplot(include=80)

# ACF and PCAF of quarterly percentage change in US consumption
ggAcf(uschange[,"Consumption"])
ggPacf(uschange[,"Consumption"])
##### the pattern in the first three spikes is what we would expect from an ARIMA(3,0,0), as the PACF tends 
##### to decrease - so in this case, the ACF and PACF lead us to think an ARIMA(3,0,0) model might be appropriate

# fit the 3,0,0 model
(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))
##### this model is actually slightly better than the model identified by auto.arima() with an AICc value of 340.67 
##### compared to 342.08
##### The auto.arima() function did not find this model because it does not consider all possible models in its search
# you can make auto.arima work harder by using the arguments stepwise=FALSE and approximation=FALSE
(fit3 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                    stepwise=FALSE, approximation=FALSE))
##### also use the argument seasonal=FALSE to prevent it searching for seasonal ARIMA models



