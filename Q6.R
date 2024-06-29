# a. Make a time plot of your data and describe the main features of the series.
data(package = "fpp")
install.packages("fpp")
library(fpp)

str(visitors)
head(visitors)
autoplot(visitors)
ggseasonplot(visitors)

## b. Split your data into a training set and a test set comprising the last two years of available data. Forecast the test set using Holt-Winters' multiplicative method.
visitors_train <- subset(visitors, 
                         end = length(visitors) - 24)
visitors_test <- subset(visitors,
                        start = length(visitors) - 23)
hw_mul_visitors_train <- hw(visitors_train,
                            h = 24,
                            seasonal = "multiplicative")

# c. Why is multiplicative seasonality necessary here?
autoplot(hw_mul_visitors_train)

## d Forecast the two-year test set using each of the following methods:
  # d-1. an ETS model;
  fc_ets_visitors_train <- forecast(ets(visitors_train), h = 24)
autoplot(fc_ets_visitors_train)
# d-2. an additive ETS model applied to a Box-Cox transformed series;
fc_ets_add_BoxCox_visitors_train <- forecast(
  ets(visitors_train, 
      lambda = BoxCox.lambda(visitors_train),
      additive.only = TRUE),
  h = 24
)
autoplot(fc_ets_add_BoxCox_visitors_train)
# d-3. a seasonal naive method;
fc_snaive_visitors_train <- snaive(visitors_train, h = 24)
autoplot(fc_snaive_visitors_train)
# d-4. an STL decomposition applied to the Box-Cox transformed data followed by an ETS model applied to the seasonally adjusted (transformed) data.
fc_BoxCox_stl_ets_visitors_train <- visitors_train %>%
  stlm(
    lambda = BoxCox.lambda(visitors_train),
    s.window = 13,
    robust = TRUE,
    method = "ets"
  ) %>%
  forecast(h = 24)
autoplot(fc_BoxCox_stl_ets_visitors_train)


# e. Which method gives the best forecasts? Does it pass the residual tests?
accuracy(hw_mul_visitors_train, visitors_test)
accuracy(fc_ets_visitors_train, visitors_test)
accuracy(fc_ets_add_BoxCox_visitors_train, visitors_test)
accuracy(fc_snaive_visitors_train, visitors_test)
accuracy(fc_BoxCox_stl_ets_visitors_train, visitors_test)


# f. Compare the same five methods using time series cross-validation with the tsCV function instead of using a training and test set. Do you come to the same conclusions?
fets_add_BoxCox <- function(y, h) {
  forecast(ets(
    y,
    lambda = BoxCox.lambda(y),
    additive.only = TRUE
  ),
  h = h)
}
fstlm <- function(y, h) {
  forecast(stlm(
    y, 
    lambda = BoxCox.lambda(y),
    s.window = frequency(y) + 1,
    robust = TRUE,
    method = "ets"
  ),
  h = h)
}
fets <- function(y, h) {
  forecast(ets(y),
           h = h)
}
# RMSE comparison of moedls 
sqrt(mean(tsCV(visitors, snaive, h = 1)^2, na.rm = TRUE))
sqrt(mean(tsCV(visitors, fets_add_BoxCox, h = 1)^2,
          na.rm = TRUE))
sqrt(mean(tsCV(visitors, fstlm, h = 1)^2,
          na.rm = TRUE))
sqrt(mean(tsCV(visitors, fets, h = 1)^2, na.rm = TRUE))
sqrt(mean(tsCV(visitors, hw, h = 1, 
               seasonal = "multiplicative")^2,
          na.rm = TRUE))


     