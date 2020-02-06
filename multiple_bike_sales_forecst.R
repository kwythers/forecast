# one of the most powerful benefits of sweep is that it helps forecasting at scale within the “tidyverse”.
# this code gives an example of applying a model to groups of time series

# load packages
library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)

# bike sales
# 
# use the bike sales data set, bike_sales, provided with the sweep package for this tutorial - the bike_sales data set 
# is a fictional daily order history that spans 2011 through 2015. It simulates a sales database that is typical of a 
# business - the customers are the “bike shops” and the products are the “models”.
bike_sales

# analyse the monthly sales trends for the bicycle manufacturer - transform the data set by aggregating by month
bike_sales_monthly <- bike_sales %>%
  mutate(month = month(order.date, label = TRUE),
         year  = year(order.date)) %>%
  group_by(year, month) %>%
  summarise(total.qty = sum(quantity)) 
bike_sales_monthly

# visualize active months for all years with a month plot using ggplot2
bike_sales_monthly %>%
  ggplot(aes(x = month, y = total.qty, group = year)) +
  geom_area(aes(fill = year), position = "stack") +
  labs(title = "Quantity Sold: Month Plot", x = "", y = "Sales",
       subtitle = "March through July tend to be most active") +
  scale_y_continuous() +
  theme_tq()

##### Performing Forecasts on Groups
# get the data organized into groups by month of the year - create a new “order.month” date using zoo::as.yearmon() 
# that captures the year and month information from the “order.date” and then passing this to lubridate::as_date() 
# to convert to date format

monthly_qty_by_cat2 <- bike_sales %>%
  mutate(order.month = as_date(as.yearmon(order.date))) %>%
  group_by(category.secondary, order.month) %>%
  summarise(total.qty = sum(quantity))
monthly_qty_by_cat2

# use the nest() function from the tidyr package to consolidate each time series by group - the newly created 
# list-column, “data.tbl”, contains the “order.month” and “total.qty” columns by group from the previous step - 
# the nest() function just bundles the data together which is very useful for iterative functional programming
monthly_qty_by_cat2_nest <- monthly_qty_by_cat2 %>%
  group_by(category.secondary) %>%
  nest()
monthly_qty_by_cat2_nest

# Forecasting Workflow
# 
# The forecasting workflow involves a few basic steps:
#   
# 1: coerce to a ts object class.
# 2: apply a model (or set of models)
# 3: forecast the models (similar to predict)
# 4: tidy the forecast

# 1: coerce to a ts object class
# map the tk_ts() function into a new column “data.ts” - the procedure is performed using the combination 
# of dplyr::mutate() and purrr::map(), which works really well for the data science workflow where analyses are built 
# progressively. As a result, this combination will be used in many of the subsequent steps in this vignette as we 
# build the analysis.
# 
# mutate and map
# The mutate() function adds a column, and the map() function maps the contents of a list-column (.x) to a function (.f). 
# in this case, .x = data.tbl and .f = tk_ts. The arguments select = -order.month, start = 2011, and freq = 12 are passed 
# to the ... parameters in map, which are passed through to the function. The select statement is used to drop the 
# “order.month” from the final output so we don’t get a bunch of warning messages. We specify start = 2011 and freq = 12 
# to return a monthly frequency.
monthly_qty_by_cat2_ts <- monthly_qty_by_cat2_nest %>%
  mutate(data.ts = map(.x = data, 
                       .f = tk_ts, 
                       select = -order.month, 
                       start = 2011,
                       freq = 12))
monthly_qty_by_cat2_ts

# modeling a time series
# map the exponential smoothing ETS (Error, Trend, Seasonal) model function, ets, from the forecast package 
# use the combination of mutate to add a column and map to interatively apply a function rowwise to a list-column - in 
# this instance, the function to map the ets function and the list-column is “data.ts” - rename the resultant column 
# “fit.ets” indicating an ETS model was fit to the time series data
monthly_qty_by_cat2_fit <- monthly_qty_by_cat2_ts %>%
  mutate(fit.ets = map(data.ts, ets))
monthly_qty_by_cat2_fit

# now do some model inspection with the sweep tidiers

# sw_tidy
# 
# get the model parameters for each nested list, combine sw_tidy within the mutate and map combo - the only 
# real difference is now unnest the generated column (named “tidy”) - because it’s easier to compare the model 
# parameters side by side, we add one additional call to spread() from the tidyr package.
monthly_qty_by_cat2_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = category.secondary, value = estimate)

# sw_glance
# 
# view the model accuracies also by mapping sw_glance within the mutate and map combo
monthly_qty_by_cat2_fit %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance)

# sw_augment
# 
# The augmented fitted and residual values can be achieved in much the same manner. This returns nine groups data. 
# Note that we pass timetk_idx = TRUE to return the date format times as opposed to the regular (yearmon or numeric) 
# time series.
augment_fit_ets <- monthly_qty_by_cat2_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment, .drop = TRUE)
augment_fit_ets

# plot the residuals for the nine categories - unfortunately we do see some very high residuals (especially 
# with “Fat Bike”) - this is often the case with realworld data.
augment_fit_ets %>%
  ggplot(aes(x = date, y = .resid, group = category.secondary)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ category.secondary, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

# create decompositions using the same procedure with sw_tidy_decomp() and the mutate and map combo.
monthly_qty_by_cat2_fit %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

# forecasting the model
# 
# forecast the multiple models again using a very similar approach with the forecast function - we want 
# a 12 month forecast so we add the argument for the h = 12 (refer to ?forecast for all of the parameters you can add, 
# there’s quite a few).
monthly_qty_by_cat2_fcast <- monthly_qty_by_cat2_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 12))
monthly_qty_by_cat2_fcast

# apply sw_sweep to get the forecast in a nice “tidy” data frame. We use the argument fitted = FALSE to 
# remove the fitted values from the forecast (leave off if fitted values are desired) - set timetk_idx = TRUE to use 
# dates instead of numeric values for the index - use unnest() to drop the left over list-columns and return an 
# unnested data frame
monthly_qty_by_cat2_fcast_tidy <- monthly_qty_by_cat2_fcast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)
monthly_qty_by_cat2_fcast_tidy

# plot the forcast for each group
monthly_qty_by_cat2_fcast_tidy %>%
  ggplot(aes(x = index, y = total.qty, color = key, group = category.secondary)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Forecasts",
       x = "", y = "Units") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ category.secondary, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
