####################################################################################################
#
#                               Forcasting passengers at YYZ
#                               ARIMA vs...
#                               Kirk R Wythers 2019.03.15 
#
####################################################################################################

# load libraries
library(tidyverse)
library(broom)
library(sweep)
library(forecast)
library(tidyquant)
library(timetk)
library(zoo)
library(tibbletime)
library(lubridate)
library(ggrepel)


# load the data
passengers <- read_csv("R_data/yyz_passengers.csv")
str(passengers)

# convert to dates
passengers <- passengers %>% 
  mutate(date = mdy(date))

passengers

# We can quickly visualize using the ggplot2 package - there appears to be some seasonality and an 
# upward trend.
passengers %>%
  ggplot(aes(x = date, y = passengers)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_smooth(method = "loess") +
  labs(title = "Monthly totals", x = "", y = "Passengers") +
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()

passengers_ts <- passengers %>%
  tk_ts(start = 2000, freq = 12, silent = TRUE)
# passengers_ts <- ts(passengers, start = 2000, frequency = 12)

passengers_ts

# split into training and testing data sets
train <- window(passengers_ts, end = c(2014, 8))
test <- window(passengers_ts, start = c(2014, 9))

# fit training data with forcast::auto.arima
arima.fit <- forecast::auto.arima(train, d = 1, D = 1,
                                  stepwise = FALSE,
                                  approximation = FALSE)
arima.fit

# fit training data with forcast::ets
ets.fit <- forecast::ets(train, model = "ZZZ")
ets.fit

##### fitting many models #####
# fit a wide variety of both ETS and ARIMA models and compare their out-of-sample performance
# 1 - create columns for training data, modelling functions, and any other model parameters
# 2 - wrap these training data and model parameters into a single param column
# 3 - use purrr::invoke_map() to apply the modelling function to these model parameters
# 4 - use purrr::map() and friends (or broom::glance()) to extract goodness of fit measures
ets.params <- tidyr::crossing(
  error = c("A", "M"), trend = c("N", "A", "M"),
  seasonal = c("N", "A", "M"), damped = c(TRUE, FALSE)
  ) %>%
  # Drop combinations with a damped non-trend.
  mutate(drop = ifelse(trend == "N" & damped, TRUE, FALSE)) %>%
  filter(!drop) %>%
  # Create labels for the models out of these parameters.
  mutate(kind = "ETS",
         desc = paste0("(", error, ",", trend,
                       ifelse(damped, "d", ""),
                       ",", seasonal, ")"),
         model = paste0(error, trend, seasonal)) %>%
  # Drop nonsensical models (these are flagged by `ets` anyway).
  filter(!model %in% c("MMA", "AMN", "AMA", "AMM",
                       "ANM", "AAM")) %>%
  select(kind, desc, model, damped)

ets.params

# create list columns containing the training data and the function to compute the model (ets in this case) - wrap up the 
# parameters into a param list column by using the curious but useful transpose() function
ets.models <- ets.params %>%
  # Add in the training set and the modelling function.
  mutate(fn = replicate(forecast::ets, n = n()),
         train = replicate(list(train), n = n())) %>%
  # Create a "param" column to pass to `fn`.
  mutate(params = purrr::transpose(list(
    "y" = train, "model" = model, "damped" = damped
  ))) %>%
  select(kind, desc, train, fn, params)

ets.models

# invoke_map() to fit all of these models - extracted the AICc values of each for comparison
ets.fits <- ets.models %>%
  # Fit the models, and extract the AICc.
  mutate(fit = purrr::invoke_map(fn, params),
         aicc = purrr::map_dbl(fit, "aicc")) %>%
  arrange(aicc)

ets.fits
##### Note that the best-performing model here — ETS(A,N,A) — is the same one arrived at automatically above

#####  same thing with the ARIMA models ##### 
# generate a list of model parameters as a data frame, collect these into a param list-column, and then fit the 
# function using invoke_map() - unlike ETS, there are an infinite number of possible ARIMA models - however, we can 
# guess from above that good models will have low order
arima.params <- tidyr::crossing(ar = 0:2, d = 1:2, ma = 0:2,
                                sar = 0:1, sma = 0:1) %>%
  # Create a label for the model.
  mutate(kind = "ARIMA",
         desc = paste0("(", ar, ",", d, ",", ma, ")(",
                       sar, ",1,", sma, ")")) %>%
  group_by(kind, desc) %>%
  # This has to be done rowwise to avoid the flattening
  # behaviour of c().
  do(order = c(.$ar, .$d, .$ma),
     seasonal = c(.$sar, 1, .$sma)) %>%
  ungroup()

arima.models <- arima.params %>%
  # Add in the training set and the modelling function.
  mutate(fn = replicate(forecast::Arima, n = n()),
         train = replicate(list(train), n = n())) %>%
  # Create a "param" column to pass to `fn`.
  mutate(params = purrr::transpose(list(
    "y" = train, "order" = order, "seasonal" = seasonal
  ))) %>%
  select(kind, desc, train, fn, params)

arima.fits <- arima.models %>%
  # Fit the models, and extract the AICc.
  mutate(fit = purrr::invoke_map(fn, params),
         aicc = purrr::map_dbl(fit, "aicc")) %>%
  arrange(aicc)

arima.fits
##### since this data frame has the same form as the one used for the ETS models, we can concatenate them into a master 
##### list of models - the forecast() function is its namesake package’s universal forecasting function - we can use it to 
##### add a column of the two-year forecast for all of these models
yyz.forecasts <- dplyr::bind_rows(ets.fits, arima.fits) %>%
  mutate(forecast = map(fit, forecast::forecast, h = 24)) %>%
  arrange(aicc)

yyz.forecasts

##### Measuring Forecast Accuracy #####
# the AICc is not a direct measure of forecast accuracy - traditional root mean squared error (RMSE) and mean absolute 
# error (MAE) are both options, although the author of the forecast package recommends mean absolute scaled error (MASE) - 
# the package provides the accuracy() function to compute all of these measures (and a few others) for any forecast object, 
# but unfortunately it returns a matrix with meaningful row names, which would be hard to wrap up inside a list column - what 
# we need is an API that returns a data frame.

# the broom package provides a standard interface for “tidying” model summaries using the S3 glance() method - at the time of 
# writing, the package doesn’t contain an implementation for forecast objects, but it isn’t too hard to write one using the 
# output of forecast::accuracy()
glance.forecast <- function(x, newdata = NULL, ...) {
  res <- if (is.null(newdata)) {
    acc <- forecast::accuracy(x)
    tibble::as_tibble(t(acc[1,]))
  } else {
    acc <- forecast::accuracy(x, newdata)
    tibble::as_tibble(t(acc[2,]))
  }
  # Format the names of the measures to suit broom::glance().
  names(res) <- tolower(names(res))
  if (length(names(res)) > 7) names(res)[8] <- "thielsu"
  res
}

# now, combine with tidyr::unnest() to get columns of RMSE, MAE, MASE, and so on using map()
acc <- yyz.forecasts %>%
  mutate(glance = map(forecast, broom::glance)) %>%
  unnest(glance) %>%
  arrange(rmse, mae, mase)

select(acc, kind, desc, rmse, mae, mase)

##### measuring out-of-sample performance using a test set #####
# pass the test data left aside at the start of the post in the same fashion as above, and lay out the in-sample and 
# out-of-sample measures side by side
perf <- yyz.forecasts %>%
  mutate(test = replicate(list(test), n = n()),
         glance = map2(forecast, test, broom::glance)) %>%
  tidyr::unnest(glance) %>%
  # Pull out the out-of-sample RMSE, MAE, and MASE.
  select(kind, desc, oos.rmse = rmse,
         oos.mae = mae, oos.mase = mase) %>%
  # Join the in-sample RMSE, MAE, and MASE columns.
  inner_join(acc, by = c("kind", "desc")) %>%
  rename(is.rmse = rmse, is.mae = mae, is.mase = mase) %>%
  arrange(oos.rmse, is.rmse) %>%
  select(kind, desc, is.rmse, oos.rmse, is.mae, oos.mae,
         is.mase, oos.mase)

# Find the top four models by in-sample and out-of-sample MASE.
top.perf <- perf %>%
  mutate(top.is = rank(is.mase), top.oos = rank(oos.mase)) %>%
  filter(top.is <= 4 | top.oos <= 4)

# Graph in-sample vs. out-of-sample MASE for the top 50% of each.
perf %>%
  filter(oos.mase < quantile(oos.mase, 0.5) |
           is.mase < quantile(is.mase, 0.5)) %>%
  ggplot(aes(y = oos.mase, x = is.mase)) +
  geom_point(aes(colour = kind)) +
  # Vertical/horizontal lines at the minimum MASE for each sample.
  geom_vline(aes(xintercept = min(is.mase)),
             linetype = 5, colour = "gray50") +
  geom_hline(aes(yintercept = min(oos.mase)),
             linetype = 5, colour = "gray50") +
  # Label the top models.
  ggrepel::geom_label_repel(aes(label = paste(kind, desc)),
                            size = 2.5, segment.colour = "gray50",
                            data = top.perf) +
  labs(x = "In-Sample MASE", y = "Out-of-Sample MASE",
       colour = "Family")

##### forecasting #####
passengers.fc <- passengers_ts %>%
  Arima(order = c(2, 1, 2), seasonal = c(1, 1, 0)) %>%
  forecast(h = 24, level = c(50, 95))

# plot the forcast
autoplot(passengers.fc) +
  geom_path(aes(y = as.vector(passengers.fc$fitted),
                x = time(passengers.fc$fitted)),
            linetype = 1, alpha = 0.5, colour = "red")
##### the model fits the data quite well, aside from some trouble around 2003 - the forecast output also gives us a nice 
##### story to tell: monthly passengers at YYZ looks likely to break 5 million for the first time in summer 2017

##### wrap up the output of autoplot() in a pretty theme, add some bells and whistles, and publish a nice little graphic 
##### on this data - the code below generates the plot at the top of this post
millions <- function(x) paste0(format(x / 1e6, digits = 2), "m")

autoplot(passengers.fc, shadecols = c("#cccccc", "#eeeeee")) +
  geom_segment(aes(y = 5e6, yend = 5e6, x = 2016.5, xend = 2019),
               size = 0.5, colour = "#888888", linetype = 5) +
  scale_x_continuous(breaks = seq(2000, 2018, by = 2), expand = c(0, 1)) +
  scale_y_continuous(labels = millions) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(colour = "#333333", size = 0.5),
        axis.ticks.length = grid::unit(2, units = "pt"),
        text = element_text(family = "Arial Narrow", size = 8),
        plot.title = element_text(size = 12, face = "bold")) +
  labs(title = paste("Monthly Passengers at Pearson International Airport",
                     "to Top 5 Million in 2017"),
       subtitle = paste("Incoming and outgoing passengers in millions,",
                        "with forecast data in blue."),
       caption = paste("Kirk R. Wythers \n",
                       "Data: Toronto Open Data (2019)\n"
                       ),
       x = NULL, y = NULL)
