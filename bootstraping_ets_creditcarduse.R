##### Bootstrapping time series #####

library(tidyverse)
library(fpp2)


##### because there may be autocorrelation present in an STL remainder series, we cannot simply use the re-draw procedure 
##### instead, we use a “blocked bootstrap”, where contiguous sections of the time series are selected at random and joined 
##### together
bootseries <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>% ts(start=2000, frequency=12)
autoplot(debitcards) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(debitcards, colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")
##### ten bootstrapped versions of monthly expenditure on retail debit cards in iceland

# use bootstrapped time series to go some way towards overcoming the "too narrow" predictiion interval problem
nsim <- 1000L
sim <- bld.mbb.bootstrap(debitcards, nsim)

# fit an ETS model and simulate one sample path from that model
h <- 36L
future <- matrix(0, nrow=nsim, ncol=h)
for(i in seq(nsim))
  future[i,] <- simulate(ets(sim[[i]]), nsim=h)

# take the means and quantiles of these simulated sample paths to form point forecasts and prediction intervals
start <- tsp(debitcards)[2]+1/12
simfc <- structure(list(
  mean = ts(colMeans(future), start=start, frequency=12),
  lower = ts(apply(future, 2, quantile, prob=0.025),
             start=start, frequency=12),
  upper = ts(apply(future, 2, quantile, prob=0.975),
             start=start, frequency=12),
  level=95),
  class="forecast")

# these prediction intervals will be larger than those obtained from an ETS model applied directly to the original data
etsfc <- forecast(ets(debitcards), h=h, level=95)
autoplot(debitcards) +
  ggtitle("Monthly retail debit card usage in Iceland") +
  xlab("Year") + ylab("million ISK") +
  autolayer(simfc, series="Simulated") +
  autolayer(etsfc, series="ETS")
