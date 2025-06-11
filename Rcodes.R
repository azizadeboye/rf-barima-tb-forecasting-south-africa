knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  
  out.width='100%',
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  
  message = FALSE,
  warning = FALSE
)

## Load Required Libraries
library(tidyverse)
library(lubridate)
library(modeltime)
library(bayesmodels)
library(timetk)
library(readxl)
library(recipes)
library(parsnip)
library(rsample)

# Load Data
paym <- read_excel("data/df.xlsx")

# Plot Time Series
paym %>% 
  plot_time_series(date, value, .smooth = FALSE)

# Split Data
splits <- initial_time_split(paym, prop = 0.9)

# Model 1: auto_arima
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))

# Model 2: arima_boost 
model_fit_arima_boosted <- arima_boost(
  min_n = 2, learn_rate = 0.015) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))

# Model 3: ets
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))

# Model 4: prophet 
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))

# Model 5: lm
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))

# Model 6: earth
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 


#Model 7: ARIMA (Modeltime) #First, we create a basic univariate ARIMA model using "arima_reg()" from modeltime package
model_fit_arima<- arima_reg(non_seasonal_ar = 0,
                            non_seasonal_differences = 1,
                            non_seasonal_ma = 1,
                            seasonal_period = 12,
                            seasonal_ar = 0,
                            seasonal_differences = 1,
                            seasonal_ma = 1) %>%
  set_engine(engine = "arima") %>%
  fit(value ~ date, data = training(splits))


#Model 8: ARIMA (Bayesmodels) #Now, we create the same model but from a Bayesian perspective with the package bayesmodels:
model_fit_arima_bayes<- sarima_reg(non_seasonal_ar = 0,
                                   non_seasonal_differences = 1,
                                   non_seasonal_ma = 1,
                                   seasonal_period = 12,
                                   seasonal_ar = 0,
                                   seasonal_differences = 1,
                                   seasonal_ma = 1,
                                   pred_seed = 100) %>%
  set_engine(engine = "stan") %>%
  fit(value ~ date, data = training(splits))

plot(model_fit_arima_bayes$fit$models$model_1)
  
#Model 9: Random Walk (Naive) (Bayesmodels) from a Bayesian perspective with the package bayesmodels:

model_fit_naive <- random_walk_reg(seasonal_random_walk = TRUE, seasonal_period = 12) %>%
  set_engine("stan") %>%
  fit(value ~ date + month(date), data = training(splits))

plot(model_fit_naive$fit$models$model_1)

# Model 10: we prepared the data and train a MARS (Multivariate Adaptive Regression Splines) model using a structured workflow below:

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

## putting all the models together

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_arima,
  model_fit_arima_bayes,
  model_fit_naive,
  wflw_fit_mars
)
models_tbl

## calibrate the models

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl


#Accuracy Metrics

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy


# Forecast & Accuracy Evaluation: 1.Visualizing the Forecast vs Test Data Set; 2.Evaluating the Test (Out of Sample) Accuracy

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
    .interactive      = interactive
  )

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
  )

#Refit to Full Dataset & Forecast Forward

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = paym)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
  )
refit_tbl %>%
  modeltime_forecast(h = "3 years") %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
  )

refit_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy#(
    #.interactive = interactive
  #)


# BEST Single MODEL FORECAST

# Bayesian ARIMA model
  
models_tbl1 <- modeltime_table(
  model_fit_arima_bayes
)

models_tbl1

## calibrate
calibration_tbl1 <- models_tbl1 %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl1


#Visualizing the Forecast vs Test Data Set

calibration_tbl1 %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
    .interactive      = interactive
  )

calibration_tbl1 %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
  )

#Refit to Full Dataset & Forecast Forward

refit_tbl1 <- calibration_tbl1 %>%
  modeltime_refit(data = paym)

refit_tbl1 %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

refit_tbl1 %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
  )



#################################################################################
# HYBRID models

library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)

# Load the data
library(readxl)
df <- read_excel("Documents/df.xlsx")

df <- ts(paym, start=c(2010, 1), end=c(2019, 12), frequency=12)

dat_train = subset(df, Class == 'Train')
dat_test = subset(df, Class == 'Test')

nrow(dat_train); nrow(dat_test)

#Preparing the Time Series Object

dat_ts <- ts(df, start = c(2010, 1), end = c(2019, 12), frequency = 12)

#lines 2 to 4

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

model_tbats <- tbats(dat_ts)
summary(model_tbats)

for_tbats <- forecast::forecast(model_tbats, h = 120)
df_tbats = as.data.frame(for_tbats)
dat_test$tbats = df_tbats$`Point Forecast`
mape(dat_test$unemploy, dat_test$tbats)

p <- predict(model_tbats, df)
resdiual <- p-df[[cases]]

library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(boostime)
library(bayesmodels)

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE

# Load the data
paym <- read_excel("Documents/df.xlsx")

paym %>%
  plot_time_series(date, values)

# Split Data 80/20
splits <- initial_time_split(paym, prop = 0.9)


#Model 1: ARIMA (Modeltime)
#First, we create a basic univariate ARIMA model using “Arima” using arima_reg()

# Model 1: arima ----
model_fit_arima<- arima_reg(non_seasonal_ar = 0,
                            non_seasonal_differences = 1,
                            non_seasonal_ma = 1,
                            seasonal_period = 12,
                            seasonal_ar = 0,
                            seasonal_differences = 1,
                            seasonal_ma = 1) %>%
  set_engine(engine = "arima") %>%
  fit(values ~ date, data = training(splits))

-------------------------------------------------------------------------------
  #Model 2: ARIMA (Bayesmodels)
  #Now, we create the same model but from a Bayesian perspective with the package bayesmodels:
  # Model 2: arima_boost ----
model_fit_arima_bayes<- sarima_reg(non_seasonal_ar = 0,
                                   non_seasonal_differences = 1,
                                   non_seasonal_ma = 1,
                                   seasonal_period = 12,
                                   seasonal_ar = 0,
                                   seasonal_differences = 1,
                                   seasonal_ma = 1,
                                   pred_seed = 100) %>%
  set_engine(engine = "stan") %>%
  fit(values ~ date, data = training(splits))

plot(model_fit_arima_bayes$fit$models$model_1)

-------------------------------------------------------------------------------  
  #Model 3: Random Walk (Naive) (Bayesmodels)
  
  model_fit_naive <- random_walk_reg(seasonal_random_walk = TRUE, seasonal_period = 12) %>%
  set_engine("stan") %>%
  fit(values ~ date + month(date), data = training(splits))

plot(model_fit_naive$fit$models$model_1)

-------------------------------------------------------------------------------
  
  recipe_spec <- recipe(values ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

## ---- paged.print = FALSE-----------------------------------------------------
models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_arima_bayes,
  model_fit_naive
)

models_tbl

## ---- paged.print = FALSE-----------------------------------------------------
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

## -----------------------------------------------------------------------------
#Accuracy Metrics

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

## -----------------------------------------------------------------------------
#Step 5 - Testing Set Forecast & Accuracy Evaluation
#There are 2 critical parts to an evaluation.
#1.Visualizing the Forecast vs Test Data Set
#2.Evaluating the Test (Out of Sample) Accuracy

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
    .interactive      = interactive
  )

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
  )
## ---- paged.print = F, message=F----------------------------------------------
#Refit to Full Dataset & Forecast Forward

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = paym)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
  )


refit_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

-------------------------------------------------------------------------------
  
  ## best model
  
  models_tbl <- modeltime_table(
    model_fit_arima_bayes
  )

models_tbl

## ---- paged.print = FALSE-----------------------------------------------------
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

## -----------------------------------------------------------------------------
#Accuracy Metrics

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

## -----------------------------------------------------------------------------
#Step 5 - Testing Set Forecast & Accuracy Evaluation
#There are 2 critical parts to an evaluation.
#1.Visualizing the Forecast vs Test Data Set
#2.Evaluating the Test (Out of Sample) Accuracy

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
    .interactive      = interactive
  )

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = paym
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 15, # For mobile screens
  )
## ---- paged.print = F, message=F----------------------------------------------
#Refit to Full Dataset & Forecast Forward

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = paym)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = paym) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
  )


refit_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )



