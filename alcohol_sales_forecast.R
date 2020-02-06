# The sweep package extends the broom tools (tidy, glance, and augment) for performing forecasts and time series 
# analysis in the “tidyverse”. The package is geared towards the workflow required to perform forecasts using 
# Rob Hyndman’s forecast package, and contains the following elements:
#   
# model tidiers: sw_tidy, sw_glance, sw_augment, sw_tidy_decomp functions extend tidy, glance, and augment from 
# the broom package specifically for models (ets(), Arima(), bats(), etc) used for forecasting.
# 
# forecast tidier: sw_sweep converts a forecast object to a tibble that can be easily manipulated in the “tidyverse”.
# 
# To illustrate, let’s take a basic forecasting workflow starting from data collected in a tibble format and then 
# performing a forecast to achieve the end result in tibble format.

# load libriaries
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)

# Forecasting Sales of Beer, Wine, and Distilled Alcohol Beverages
# 
# We’ll use the tidyquant package to get the US alcohol sales, which comes from the FRED data base 
# (the origin is the US Bureau of the Census, one of the 80+ data sources FRED connects to). The FRED code 
# is “S4248SM144NCEN” and the data set can be found here.
alcohol_sales_tbl <- tq_get("S4248SM144NCEN", 
                            get  = "economic.data", 
                            from = "2007-01-01",
                            to   = "2016-12-31")
alcohol_sales_tbl

# We can quickly visualize using the ggplot2 package. We can see that there appears to be some seasonality and an 
# upward trend.
alcohol_sales_tbl %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_smooth(method = "loess") +
  labs(title = "US Alcohol Sales: Monthly", x = "", y = "Millions") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

# Forecasting Workflow

# The forecasting workflow involves a few basic steps:
#   
# Step 1: Coerce to a ts object class.
# Step 2: Apply a model (or set of models)
# Step 3: Forecast the models (similar to predict)
# Step 4: Use sw_sweep() to tidy the forecast.
# Note that we purposely omit other steps such as testing the series for stationarity (Box.test(type = "Ljung")) and analysis of autocorrelations (Acf, Pacf) for brevity purposes. We recommend the analyst to follow the forecasting workflow in “Forecasting: principles and practice”

# Step 1: Coerce to a ts object class
# 
# The forecast package uses the ts data structure, which is quite a bit different than tibbles that we are currently using. 
# Fortunately, it’s easy to get to the correct structure with tk_ts() from the timetk package. The start and freq variables 
# are required for the regularized time series (ts) class, and these specify how to treat the time series. For monthly, 
# the frequency should be specified as 12. This results in a nice calendar view. The silent = TRUE tells the tk_ts() function 
# to skip the warning notifying us that the “date” column is being dropped. Non-numeric columns must be dropped for ts 
# class, which is matrix based and a homogeneous data class.
alcohol_sales_ts <- tk_ts(alcohol_sales_tbl, start = 2007, freq = 12, silent = TRUE)
alcohol_sales_ts

# A significant benefit is that the resulting ts object maintains a “timetk index”, which will help with forecasting 
# dates later. We can verify this using has_timetk_idx() from the timetk package.

has_timetk_idx(alcohol_sales_ts)
# Now that a time series has been coerced, let’s proceed with modeling.

# Step 2: Modeling a time series
# 
# The modeling workflow takes a time series object and applies a model. Nothing new here: we’ll simply use the 
# ets() function from the forecast package to get an Exponential Smoothing ETS (Error, Trend, Seasonal) model.
fit_ets <- alcohol_sales_ts %>% 
  ets()
# Where sweep can help is in the evaluation of a model. Expanding on the broom package there are four functions:
#   
# sw_tidy(): Returns a tibble of model parameters
# sw_glance(): Returns the model accuracy measurements
# sw_augment(): Returns the fitted and residuals of the model
# sw_tidy_decomp(): Returns a tidy decomposition from a model
# The guide below shows which model object compatibility with sweep tidier functions.
# 
# Function Compatibility
# Object	sw_tidy()	sw_glance()	sw_augment()	sw_tidy_decomp()	sw_sweep()
# ar					
# arima	  X	        X	          X		
# Arima	  X	        X	          X		
# ets	    X	        X	          X	            X	
# baggedETS					
# bats	  X	        X	          X	            X	
# tbats	  X	        X	          X	            X	
# nnetar	X	        X	          X		
# stl				                                  X	
# HoltWinters	X	    X	          X	            X	
# StructTS	X	      X	          X	            X	
# tslm	  X	        X	          X		
# decompose				                            X	
# adf.test	X	      X			
# Box.test	X	      X			
# kpss.test	X	X			
# forecast					                                            X
# Going through the tidiers, we can get useful model information.

# sw_tidy
# 
# sw_tidy() returns the model parameters.
sw_tidy(fit_ets)

# sw_glance
# 
# sw_glance() returns the model quality parameters.
sw_glance(fit_ets)

# sw_augment
# 
# sw_augment() returns the actual, fitted and residual values.
augment_fit_ets <- sw_augment(fit_ets)
augment_fit_ets

# We can review the residuals to determine if their are any underlying patterns left. Note that the index is class 
# yearmon, which is a regularized date format.

augment_fit_ets %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_x_yearmon(n = 10) +
  labs(title = "US Alcohol Sales: ETS Residuals", x = "") + 
  theme_tq()

# sw_tidy_decomp
# 
# sw_tidy_decomp() returns the decomposition of the ETS model.
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets

# We can review the decomposition using ggplot2 as well. The data will need to be manipulated slightly for the 
# facet visualization. The gather() function from the tidyr package is used to reshape the data into a long format 
# data frame with column names “key” and “value” indicating all columns except for index are to be reshaped. 
# The “key” column is then mutated using mutate() to a factor which preserves the order of the keys so “observed” 
# comes first when plotting.
decomp_fit_ets %>%
  gather(key = key, value = value, -index) %>%
  mutate(key = forcats::as_factor(key)) %>%
  ggplot(aes(x = index, y = value, group = key)) +
  geom_line(color = palette_light()[[2]]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  facet_wrap(~ key, scales = "free_y") +
  scale_x_yearmon(n = 10) +
  labs(title = "US Alcohol Sales: ETS Decomposition", x = "") + 
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 3: Forecasting the model
# 
# Next we forecast the ETS model using the forecast() function. The returned forecast object isn’t in a “tidy” 
# format (i.e. data frame). This is where the sw_sweep() function helps.
fcast_ets <- fit_ets %>%
  forecast(h = 12)

# Step 4: Tidy the forecast object
# 
# We’ll use the sw_sweep() function to coerce a forecast into a “tidy” data frame. The sw_sweep() function then 
# coerces the forecast object into a tibble that can be sent to ggplot for visualization. Let’s inspect the result.
sw_sweep(fcast_ets, fitted = TRUE)

# The tibble returned contains “index”, “key” and “value” (or in this case “price”) columns in a long or “tidy” 
# format that is ideal for visualization with ggplot2. The “index” is in a regularized format (in this case yearmon) 
# because the forecast package uses ts objects. We’ll see how we can get back to the original irregularized format 
# (in this case date) later. The “key” and “price” columns contains three groups of key-value pairs:
#   
# actual: the actual values from the original data
# fitted: the model values returned from the ets() function (excluded by default)
# forecast: the predicted values from the forecast() function
# 
# The sw_sweep() function contains an argument fitted = FALSE by default meaning that the model “fitted” values are 
# not returned. We can toggle this on if desired. The remaining columns are the forecast confidence intervals 
# (typically 80 and 95, but this can be changed with forecast(level = c(80, 95))). These columns are setup in a wide 
# format to enable using the geom_ribbon().

# Let’s visualize the forecast with ggplot2. We’ll use a combination of geom_line() and geom_ribbon(). The fitted values 
# are toggled off by default to reduce the complexity of the plot, but these can be added if desired. Note that because 
# we are using a regular time index of the yearmon class, we need to add scale_x_yearmon().
sw_sweep(fcast_ets) %>%
  ggplot(aes(x = index, y = price, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "US Alcohol Sales, ETS Model Forecast", x = "", y = "Millions",
       subtitle = "Regular Time Index") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_yearmon(n = 12, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()

# Because the ts object was created with the tk_ts() function, it contained a timetk index that was carried with it 
# throughout the forecasting workflow. As a result, we can use the timetk_idx argument, which maps the original irregular 
# index (dates) and a generated future index to the regularized time series (yearmon). This results in the ability 
# to return an index of date and datetime, which is not currently possible with the forecast objects. Notice that the 
# index is returned as date class.
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  head()
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  tail()

# We can build the same plot with dates in the x-axis now.
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = price, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "US Alcohol Sales, ETS Model Forecast", x = "", y = "Millions", 
       subtitle = "Irregular Time Index") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()

# In this example, there is not much benefit to returning an irregular time series. However, when working with frequencies 
# below monthly, the ability to return irregular index values becomes more apparent.


alcohol_sales_tsalcohol_sales_ts