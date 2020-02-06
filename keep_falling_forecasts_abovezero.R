##### Ensuring forecasts stay within limits #####

##### positive forecasts
##### to impose a positivity constraint, simply work on the log scale, by specifying the Box-Cox parameter λ=0 - for example, 
##### consider the real price of a dozen eggs (1900-1993; in cents)

library(tidyverse)
library(fpp2)

# forecasts for the price of a dozen eggs, constrained to be positive using a Box-Cox transformation
eggs %>%
  ets(model = "AAN", damped = FALSE, lambda = 0) %>%
  forecast(h = 50, biasadj = TRUE) %>%
  autoplot()

##### Forecasts constrained to an interval
##### handle data constrained to an interval, imagine that the egg prices were constrained to lie within a=50 
##### and b=400 - then we can transform the data using a scaled logit transform which maps (a,b) to the whole real line
# this is not a built-in transformation, so we will need to do more work
# Bounds
a <- 50
b <- 400
# Transform data and fit model
fit <- log((eggs-a)/(b-eggs)) %>%
  ets(model="AAN", damped=FALSE)
fc <- forecast(fit, h=50)
# Back-transform forecasts
fc[["mean"]] <- (b-a)*exp(fc[["mean"]]) /
  (1+exp(fc[["mean"]])) + a
fc[["lower"]] <- (b-a)*exp(fc[["lower"]]) /
  (1+exp(fc[["lower"]])) + a
fc[["upper"]] <- (b-a)*exp(fc[["upper"]]) /
  (1+exp(fc[["upper"]])) + a
fc[["x"]] <- eggs
# plot result on original scale
autoplot(fc)
##### no bias-adjustment has been used here, so the forecasts are the medians of the future distributions - the prediction 
##### intervals from these transformations have the same coverage probability as on the transformed scale, because quantiles 
##### are preserved under monotonically increasing transformations

##### the prediction intervals lie above 50 due to the transformation - as a result of this artificial (and unrealistic) 
##### constraint, the forecast distributions have become extremely skewed
