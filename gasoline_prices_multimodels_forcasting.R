

library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
 
# get  data from the FRED data base using tidyquant - use tq_get() to retrieve the Gasoline Prices 
# from 1990 through today (2019-09-20
gas_prices_monthly_raw <- tq_get(x    = "GASREGCOVM", get  = "economic.data", from = "1990-01-01", to   = "2016-12-31") 
gas_prices_monthly_raw

# any NAs?
summary(gas_prices_monthly_raw$price)

# use the fill() from the tidyr package to help deal with these data - first fill down and then 
# fill up to use the previous and then post days prices to fill in the missing data
gas_prices_monthly <- gas_prices_monthly_raw %>%
  fill(price, .direction = "down") %>%
  fill(price, .direction = "up")

# plot the data
gas_prices_monthly %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(color = palette_light()[[1]]) +
  labs(title = "Gasoline Prices, Monthly", x = "", y = "USD") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq()

##### if monthly periodicity is too granular for model fitting, switch periodicity to quarterly using 
##### tq_transmute() from the tidyquant package along with the periodicity aggregation function to.period 
##### from the xts package - convert the date to yearqtr class which is regularized
gas_prices_quarterly <- gas_prices_monthly %>%
  tq_transmute(mutate_fun = to.period, period = "quarters") 
gas_prices_quarterly

# plot quaterly data
gas_prices_quarterly %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(color = palette_light()[[1]], size = 1) +
  labs(title = "Gasoline Prices, Quarterly", x = "", y = "USD") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_tq()

##### multiple model implementation #####

# coerce the data to time series
# build a model list using nested lists
# create the the model data frame
# invoke a function map

# start by coercing the univariate time series with tk_ts()
gas_prices_quarterly_ts <- gas_prices_quarterly %>% 
  tk_ts(select = -date, start = c(1990, 3), freq = 4)
gas_prices_quarterly_ts

# create a nested list using the function names as the first-level keys - pass the model parameters as 
# name-value pairs in the second level
models_list <- list(
  auto.arima = list(
    y = gas_prices_quarterly_ts
  ),
  ets = list(
    y = gas_prices_quarterly_ts,
    damped = TRUE
  ),
  bats = list(
    y = gas_prices_quarterly_ts
  )
)

# convert to a data frame using the function, enframe() that turns lists into tibbles - set the arguments 
# name = "f" and value = "params" - in doing so the model names are the now convieniently located in column “f”
models_tbl <- enframe(models_list, name = "f", value = "params")
models_tbl

# combine mutate() with invoke_map() as follows - now models are fitted using the parameters we defined previously
models_tbl_fit <- models_tbl %>%
  mutate(fit = invoke_map(f, params))
models_tbl_fit

##### inspecting the model fit
##### review the model parameters, accuracy measurements, and the residuals using sw_tidy(), sw_glance(), and sw_augment()

# the tidying function returns the model parameters and estimates - use the combination of mutate and map to 
# iteratively apply the sw_tidy() function as a new column named “tidy” - uunnest and spread to review the terms 
# by model function
models_tbl_fit %>%
  mutate(tidy = map(fit, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = f, value = estimate)

# glance is one of the most powerful tools because it yields the model accuracies enabling direct comparisons 
# between the fit of each model - use the same process for used for tidy, except theres no need to spread to perform 
# the comparison - the ARIMA model has the lowest AIC by far
models_tbl_fit %>%
  mutate(glance = map(fit, sw_glance)) %>%
  unnest(glance, .drop = TRUE)

# augment the models to get the residuals following the same procedure - pipe (%>%) the results right into ggplot() 
# for plotting - the ARIMA model has the largest residuals especially as the model index increases whereas the bats 
# model has relatively low residuals
models_tbl_fit %>%
  mutate(augment = map(fit, sw_augment, rename_index = "date")) %>%
  unnest(augment) %>%
  ggplot(aes(x = date, y = .resid, group = f)) +
  geom_line(color = palette_light()[[2]]) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess") +
  facet_wrap(~ f, nrow = 3) +
  labs(title = "Residuals Plot") +
  theme_tq()

# create the forecast for the models is accomplished by mapping the forecast function - the next six quarters are 
# forecasted withe the argument h = 6
models_tbl_fcast <- models_tbl_fit %>%
  mutate(fcast = map(fit, forecast, h = 6))
models_tbl_fcast

# map sw_sweep, which coerces the forecast into the “tidy” tibble format - set fitted = FALSE to remove the model 
# fitted values from the output - set timetk_idx = TRUE to use dates instead of numeric values for the index
models_tbl_fcast_tidy <- models_tbl_fcast %>%
  mutate(sweep = map(fcast, sw_sweep, fitted = FALSE, timetk_idx = TRUE, rename_index = "date"))
models_tbl_fcast_tidy

# unnest the “sweep” column to get the results of all three model
models_tbl_fcast_tidy %>%
  unnest(sweep)

# plot the forecasts by unnesting the “sweep” column and piping to ggplot()
models_tbl_fcast_tidy %>%
  unnest(sweep) %>%
  ggplot(aes(x = date, y = price, color = key, group = f)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  facet_wrap(~f, nrow = 3) +
  labs(title = "Gasoline Price Forecasts",
       subtitle = "Forecasting multiple models with sweep: ARIMA, BATS, ETS",
       x = "", y = "Price") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_tq() +
  scale_color_tq()


