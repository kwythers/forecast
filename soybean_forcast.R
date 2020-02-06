##### use 'broom' for time series forcating #####

# load libraries 
require(tidyverse)
require(tidyquant)
require(timetk)
require(sweep)
require(forecast)

##### get some data with tidyquant from the FRED data base (source: us census data):
##### sales - alcoholic beverages (S4248SM144NCEN) 
##### price - global soybean (PSOYBUSDQ)

# alcohol_sales_tbl <- tq_get("S4248SM144NCEN", 
#                             get  = "economic.data", 
#                             from = "2007-01-01",
#                             to   = "2016-12-31")
# alcohol_sales_tbl

soybean_price <- tq_get("PSOYBUSDQ",
                            get  = "economic.data",
                            from = "2008-01-01",
                            to   = "2018-12-31")
soybean_price

# plot the data
soybean_price %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_smooth(method = "loess") +
  labs(title = "Global Soybean Price: Quarterly", x = "", y = "US Dollars per Metric Ton") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

##### workflow:
##### coerce to a ts object class.
##### apply a model (or set of models)
##### forecast the models (similar to predict)
##### use sw_sweep() to tidy the forecast

soybean_price_ts <- tk_ts(soybean_price, start = 2008, freq = 4, silent = TRUE)
soybean_price_ts

# verify that our ts has a time index with has_timetk_idx() from the timetk package
has_timetk_idx(soybean_price_ts)

# take the time series object and applly a model - use the ets() function from the forecast package 
# to get an exponential smoothing ETS (Error, Trend, Seasonal) model
fit_ets <- soybean_price_ts %>%
  ets()

# sw_tidy() returns the model parameters
sw_tidy(fit_ets)

# sw_glance() returns the model quality parameters
sw_glance(fit_ets)

# sw_augment() returns the actual, fitted and residual values
augment_fit_ets <- sw_augment(fit_ets)
augment_fit_ets

##### review the residuals to determine if their are any underlying patterns left 
##### note that the index is class yearmon, which is a regularized date format
augment_fit_ets %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_x_yearmon(n = 10) +
  labs(title = "Global Soybean Price: ETS Residuals", x = "") + 
  theme_tq()

# sw_tidy_decomp() returns the decomposition of the ETS model
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets 


##### review the decomposition using ggplot2 - use the gather() function from the tidyr package to 
##### reshape the data into a long format data frame with column names “key” and “value” indicating 
##### all columns except for index are to be reshaped - mutate the “key” column with mutate() to a factor 
##### which preserves the order of the keys so “observed” comes first when plotting
decomp_fit_ets %>%
  gather(key = key, value = value, -index) %>%
  mutate(key = forcats::as_factor(key)) %>%
  ggplot(aes(x = index, y = value, group = key)) +
  geom_line(color = palette_light()[[2]]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  facet_wrap(~ key, scales = "free_y") +
  scale_x_yearmon(n = 10) +
  labs(title = "Global Soybean Price: ETS Decomposition", x = "") + 
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### forecast the ETS model using the forecast() function - the returned forecast object isn’t 
##### in a “tidy” format (i.e. data frame) - this is where the sw_sweep() function helps
fcast_ets <- fit_ets %>%
  forecast(h = 12)

# use the sw_sweep() function to coerce a forecast into a “tidy” data frame
sw_sweep(fcast_ets, fitted = TRUE)

# visualize the forecast with ggplot2 - use a combination of geom_line() and geom_ribbon() - the fitted 
# values are toggled off by default to reduce the complexity of the plot, but these can be added later - note 
# that because we are using a regular time index of the yearmon class, we need to add scale_x_yearmon()
sw_sweep(fcast_ets) %>%
  ggplot(aes(x = index, y = price, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Global Soybean Price, ETS Model Forecast", x = "", y = "US Dollars per Metric Ton",
       subtitle = "Regular Time Index") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_yearmon(n = 12, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()
##### ts object was created with the tk_ts() function, it contained a timetk index that was carried with 
##### it throughout the forecasting workflow

##### we can use the timetk_idx argument, which maps the original irregular index (dates) and a generated 
##### future index to the regularized time series (yearmon) - this results in the ability to return an index 
##### of date and datetime, which is not currently possible with the forecast objects - note that the index is 
##### returned as date class
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  head()
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  tail()

# same plot with dates in the x-axis now
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = price, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Global Soybean Price, ETS Model Forecast", x = "", y = "US Dollars per Metric Ton", 
       subtitle = "Irregular Time Index") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()


