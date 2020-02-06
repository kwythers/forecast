##### Forecasting with decomposition #####
library(tidyverse)
library(fpp2)

# naïve forecasts of the seasonally adjusted data obtained from an STL decomposition of the electrical equipment orders data
fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")


# forecasts of the electrical equipment orders data based on a naïve forecast of the seasonally adjusted data and a 
# seasonal naïve forecast of the seasonal component, after an STL decomposition of the data
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

# a short-cut approach is to use the stlf() function - the following code will decompose the time series using STL, 
# forecast the seasonally adjusted series, and return the reseasonalised forecasts
fcast <- stlf(elecequip, method='naive')
autoplot(fcast)
