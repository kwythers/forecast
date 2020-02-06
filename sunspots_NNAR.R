##### Sunspots and forecasts from an NNAR #####

library(tidyverse)

# forecasts from a neural network with ten lagged inputs and one hidden layer containing six neurons
fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30))

# here is a simulation of 9 possible future sample paths for the sunspot data - each sample path covers the 
# next 30 years after the observed data
sim <- ts(matrix(0, nrow=30L, ncol=9L),
          start=end(sunspotarea)[1L]+1L)
for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=30L)
# future sample paths for the annual sunspot data
autoplot(sunspotarea) + autolayer(sim)

##### because it is a little slow, PI=FALSE is the default, so prediction intervals are not computed unless requested - the 
##### npaths argument in forecast() controls how many simulations are done (default 1000) - by default, the errors are drawn 
##### from a normal distribution. The bootstrap argument allows the errors to be “bootstrapped”
fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)
