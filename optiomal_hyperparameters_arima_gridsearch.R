##### Searching for the optimal hyper-parameters of an ARIMA model in parallel: the tidy gridsearch approach #####
##### predict future monthly total passengers flying from LuxAirport

# load libraries
library(tidyverse)
library(forecast)
library(lubridate)
library(furrr)
library(tsibble)
library(brotools)

ihs <- function(x){
  log(x + sqrt(x**2 + 1))
}

# load the data
# Parsed with column specification:
# cols(
#   destination = col_character(),
#   date = col_date(format = ""),
#   passengers = col_double()
# )
avia_clean_monthly <- read_csv("https://raw.githubusercontent.com/b-rodrigues/avia_par_lu/master/avia_clean_monthy.csv")


##### Setup #####

# split the data into a training set and into a testing set
avia_clean_train <- avia_clean_monthly %>%
  select(date, passengers) %>%
  filter(year(date) < 2015) %>%
  group_by(date) %>%
  summarise(total_passengers = sum(passengers)) %>%
  pull(total_passengers) %>%
  ts(., frequency = 12, start = c(2005, 1))

avia_clean_test <- avia_clean_monthly %>%
  select(date, passengers) %>%
  filter(year(date) >= 2015) %>%
  group_by(date) %>%
  summarise(total_passengers = sum(passengers)) %>%
  pull(total_passengers) %>%
  ts(., frequency = 12, start = c(2015, 1))

logged_train_data <- ihs(avia_clean_train)

logged_test_data <- ihs(avia_clean_test)

# define a helper function
# takes a forecast object as argument, and returns a nice tibble.
to_tibble <- function(forecast_object){
  point_estimate <- forecast_object$mean %>%
    as_tsibble() %>%
    rename(point_estimate = value,
           date = index)
  
  upper <- forecast_object$upper %>%
    as_tsibble() %>%
    spread(key, value) %>%
    rename(date = index,
           upper80 = `80%`,
           upper95 = `95%`)
  
  lower <- forecast_object$lower %>%
    as_tsibble() %>%
    spread(key, value) %>%
    rename(date = index,
           lower80 = `80%`,
           lower95 = `95%`)
  
  reduce(list(point_estimate, upper, lower), full_join)
}

# look at the arima() function:
# ARIMA Modelling of Time Series
# Description
# fit an ARIMA model to a univariate time series.
# 
# Usage
# 
# arima(x, order = c(0L, 0L, 0L),
#       seasonal = list(order = c(0L, 0L, 0L), period = NA),
#       xreg = NULL, include.mean = TRUE,
#       transform.pars = TRUE,
#       fixed = NULL, init = NULL,
#       method = c("CSS-ML", "ML", "CSS"), n.cond,
#       SSinit = c("Gardner1980", "Rossignol2011"),
#       optim.method = "BFGS",
#       optim.control = list(), kappa = 1e6)
# 
# the user is supposed to enter the hyper-parameters as two lists, one called order for p, d, q and one called seasonal 
# for P, D, Q, S. So what we need is to define these lists
order_list <- list("p" = seq(0, 3),
                   "d" = seq(0, 2),
                   "q" = seq(0, 3)) %>% 
  cross() %>% 
  map(lift(c))
# start with order_list. This list has 3 elements, “p”, “d” and “q”. Each element is a sequence from 0 to 3 
# (2 in the case of “d”). When I pass this list to purrr::cross() I get the product set of the starting list, 
# so in this case a list of 4*3*4 = 48 elements. However, this list looks pretty bad:
list("p" = seq(0, 3), 
     "d" = seq(0, 2), 
     "q" = seq(0, 3)) %>% 
  cross() %>% 
  head(3)
# I want something like this instead:
# 
# [[1]]
# p d q 
# 0 0 0 
# 
# [[2]]
# p d q 
# 1 0 0 
# 
# [[3]]
# p d q 
# 2 0 0 

# this is possible with the last line, map(lift(c)) - there’s a lot going on in this very small line of code - first of all, 
# there’s map() - map() iterates over lists, and applies a function, in this case lift(c) - purrr::lift() is a very 
# interesting function that lifts the domain of definition of a function from one type of input to another - the function 
# whose input I am lifting is c(). So now, c() can take a list instead of a vector - compare the following
# the usual
c("a", "b")
# nothing happens
c(list("a", "b"))
# magic happens
lift(c)(list("a", "b"))
# so order_list is exactly what I want
head(order_list)

# do the same for season_list:
season_list <- list("P" = seq(0, 3),
                    "D" = seq(0, 2),
                    "Q" = seq(0, 3),
                    "period" = 12)  %>%
  cross() %>%
  map(lift(c))

# coerce these two lists of vectors to tibbles:
orderdf <- tibble("order" = order_list)
seasondf <- tibble("season" = season_list)

# create the grid of hyper-parameters:
hyper_parameters_df <- crossing(orderdf, seasondf)
nrows <- nrow(hyper_parameters_df)
head(hyper_parameters_df)

# the hyper_parameters_df data frame has 2304 rows, meaning, I will now estimate 2304 models, and will do so in 
# parallel - let’s just take a quick look at the internals of hyper_parameters_df:
glimpse(hyper_parameters_df)
##### in the order column, the vector 0, 0, 0 is repeated as many times as there are combinations of P, D, Q, S 
# for season - same for all the other vectors of the order column

##### Training the models ##### 
# because training these models might take some time, use the  {furrr} package by Davis Vaughan to train the arima() 
# function in parallel. For this, first define 8 workers
plan(multiprocess, workers = 8)

# run the code
tic <- Sys.time()
models_df <- hyper_parameters_df %>%
  mutate(models = future_map2(.x = order,
                              .y = season,
                              ~possibly(arima, otherwise = NULL)(x = logged_train_data,
                                                                 order = .x, seasonal = .y)))
running_time <- Sys.time() - tic
##### I use future_map2(), which is just like map2() but running in parallel - add a new column to the data called models, 
##### which will contain the models trained over all the different combinations of order and season. The models are trained 
##### on the logged_train_data

# training the 2304 models took 18 minutes, which is plenty of time to browse the latest memes, but still quick enough 
# that it justifies the whole approach. Let’s take a look at the models_df object:
head(models_df)
##### as you can see, the models column contains all the trained models - the model on the first row, was trained with the 
##### hyperparameters of row 1, and so on - but, the work is not over! we now need to find the best model - first, add a 
##### new column to the tibble, which contains the forecast

# from the forecast, I extract the point estimate and mutate true values and the computed the RMSE, 
# as as columns to the original data:
models_df <- models_df %>%
  mutate(forecast = map(models, ~possibly(forecast, otherwise = NULL)(., h = 39))) %>%
  mutate(point_forecast = map(forecast, ~.$`mean`)) %>%
  mutate(true_value = rerun(nrows, logged_test_data)) %>%
  mutate(rmse = map2_dbl(point_forecast, true_value,
                         ~sqrt(mean((.x - .y) ** 2))))

head(models_df)

# select the best performing model - as the model with minimum RMSE
best_model <- models_df %>%
  filter(rmse == min(rmse, na.rm = TRUE))
# best model
(best_model_forecast <- to_tibble(best_model$forecast[[1]]))

# plot 
avia_clean_monthly %>%
  group_by(date) %>%
  summarise(total = sum(passengers)) %>%
  mutate(total_ihs = ihs(total)) %>%
  ggplot() +
  ggtitle("Logged data") +
  geom_line(aes(y = total_ihs, x = date), colour = "#82518c") +
  scale_x_date(date_breaks = "1 year", date_labels = "%m-%Y") +
  geom_ribbon(data = best_model_forecast, aes(x = date, ymin = lower95, ymax = upper95), 
              fill = "#666018", alpha = 0.2) +
  geom_line(data = best_model_forecast, aes(x = date, y = point_estimate), linetype = 2, colour = "#8e9d98") +
  theme_minimal()

