##### Backcasting #####

library(tidyverse)
library(fpp2)

# function to reverse the ts
reverse_ts <- function(y)
{
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}
# Function to reverse a forecast
reverse_forecast <- function(object)
{
  h <- length(object[["mean"]])
  f <- frequency(object[["mean"]])
  object[["x"]] <- reverse_ts(object[["x"]])
  object[["mean"]] <- ts(rev(object[["mean"]]),
                         end=tsp(object[["x"]])[1L]-1/f, frequency=f)
  object[["lower"]] <- object[["lower"]][h:1L,]
  object[["upper"]] <- object[["upper"]][h:1L,]
  return(object)
}

# backcast example
euretail %>%
  reverse_ts() %>%
  auto.arima() %>%
  forecast() %>%
  reverse_forecast() -> bc
# backcasts for quarterly retail trade in the Euro area using an ARIMA model
autoplot(bc) +
  ggtitle(paste("Backcasts from",bc[["method"]]))
