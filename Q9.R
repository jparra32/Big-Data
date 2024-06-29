install.packages("fpp")
install.packages("forecast")
install.packages("ggplot2")
install.packages("rmarkdown")

library(fpp)
data(package = "fpp")


##trying different 
library(forecast)

# Load the 'co2' dataset
data(co2)
co2

train <- window(co2, end = c(1996, 12))
h <- length(co2) - length(train)
ETS <- forecast(ets(train), h = h)
ARIMA <- forecast(auto.arima(train, lambda = 0, biasadj = TRUE), h = h)
STL <- stlf(train, lambda = 0, h = h, biasadj = TRUE)
NNAR <- forecast(nnetar(train), h = h)
TBATS <- forecast(tbats(train, biasadj = TRUE), h = h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]]) / 5

autoplot(co2) +
  autolayer(ETS, series = "ETS", PI = FALSE) +
  autolayer(ARIMA, series = "ARIMA", PI = FALSE) +
  autolayer(STL, series = "STL", PI = FALSE) +
  autolayer(NNAR, series = "NNAR", PI = FALSE) +
  autolayer(TBATS, series = "TBATS", PI = FALSE) +
  autolayer(Combination, series = "Combination") +
  xlab("Year") + ylab("Co2 Levels") +
  ggtitle("Monthly Mean Carbon Dioxide Levels")

autoplot(co2,  main = "Monthly Mean Carbon Dioxide Levels")


accuracy_table <- c(
  ETS = accuracy(ETS, co2)["Test set", "RMSE"],
  ARIMA = accuracy(ARIMA, co2)["Test set", "RMSE"],
  `STL-ETS` = accuracy(STL, co2)["Test set", "RMSE"],
  NNAR = accuracy(NNAR, co2)["Test set", "RMSE"],
  TBATS = accuracy(TBATS, co2)["Test set", "RMSE"],
  Combination = accuracy(Combination, co2)["Test set", "RMSE"]
)

accuracy_table


# First fit a model to the data
fit <- ets(co2/1000)

# Forecast six months ahead
h <- 6
fc <- forecast(fit, h = h)

# Simulate 10000 future sample paths
nsim <- 10000
sim <- numeric(nsim)
for (i in seq_len(nsim)) {
  sim[i] <- sum(simulate(fit, future = TRUE, nsim = h))
}
meanagg <- mean(sim)

sum_fc <- sum(fc[["mean"]][1:6])
meanagg <- meanagg

sum_fc
meanagg


quantile_80 <- quantile(sim, prob = c(0.1, 0.9))
quantile_95 <- quantile(sim, prob = c(0.025, 0.975))

quantile_80
quantile_95

