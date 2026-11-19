# Extracted from the supplied rendered R analysis HTML.
# Review paths, package versions, and execution order before publishing.

library(readxl)

library(forecast)

library(tseries)

library(ggplot2)

df <- read_excel("C:/Users/ASUS/OneDrive/Desktop/final_research/data set/Research_data_set.xlsx")
summary(df)

names(df)

head(df)

# ── Target variable
domestic <- as.numeric(as.character(df$`Domestic ( GWh)`))

domestic_ts <- ts(
  domestic,
  start     = c(2000, 1),
  frequency = 12
)

autoplot(domestic_ts) +
  ggtitle("Domestic Electricity Consumption") +
  xlab("Year") +
  ylab("GWh")

## ── Exogenous variables by column INDEX (avoids °C encoding issue)
# Col 1=Date, 2=Population, 3=MaxTemp, 4=MinTemp, 5=MaxRH, 6=MinRH,
# Col 7=IIp, 8=GDP, 9=Domestic (target), 

xreg_all <- cbind(
  Population = as.numeric(df[[2]]),
  MaxTemp    = as.numeric(df[[3]]),
  MinTemp    = as.numeric(df[[4]]),
  MaxRH      = as.numeric(df[[5]]),
  MinRH      = as.numeric(df[[6]]),
  IIp        = as.numeric(df[[7]])
)

cat("Any NAs in xreg_all:", anyNA(xreg_all), "\n")

head(xreg_all)

test_size <- 24
n         <- nrow(xreg_all)

# Target splits
train_ts <- window(
  domestic_ts,
  end = time(domestic_ts)[n - test_size]
)

test_ts <- window(
  domestic_ts,
  start = time(domestic_ts)[n - test_size + 1]
)

# Exogenous splits
xreg_train <- xreg_all[1:(n - test_size), ]
xreg_test  <- xreg_all[(n - test_size + 1):n, ]

cat("Train observations:", nrow(xreg_train), "\n")

cat("Test  observations:", nrow(xreg_test),  "\n")

lambda <- BoxCox.lambda(train_ts)
lambda

bc_train_ts <- BoxCox(train_ts, lambda)

autoplot(bc_train_ts) +
  ggtitle("Box-Cox Transformed Domestic Series (ARIMAX)")

bc_diff1 <- diff(bc_train_ts, differences = 1)

autoplot(bc_diff1) +
  ggtitle("Box-Cox Differenced Domestic Series (ARIMAX)")

par(mfrow = c(1, 2))

Acf(bc_diff1,  lag.max = 72, main = "ACF Domestic ARIMAX")
Pacf(bc_diff1, lag.max = 72, main = "PACF Domestic ARIMAX")

par(mfrow = c(1, 1))

arimax_bc_model <- auto.arima(
  bc_train_ts,
  xreg         = xreg_train,   # <── exogenous variables added here
  d            = 1,
  seasonal     = FALSE,
  stepwise     = FALSE,
  approximation = FALSE
)

summary(arimax_bc_model)

checkresiduals(arimax_bc_model)

Box.test(
  residuals(arimax_bc_model),
  lag  = 24,
  type = "Ljung-Box"
)

forecast_bc <- forecast(arimax_bc_model, xreg = xreg_test, h = 24)

forecast_original <- InvBoxCox(forecast_bc$mean, lambda)

actual <- as.numeric(test_ts)
pred   <- as.numeric(forecast_original)

rmse <- sqrt(mean((actual - pred)^2))
mae  <- mean(abs(actual - pred))
mape <- mean(abs((actual - pred) / actual)) * 100

cat("Test RMSE =", rmse, "\n")

cat("Test MAE  =", mae,  "\n")

cat("Test MAPE =", mape, "%\n")

plot_test_data <- data.frame(
  Time      = as.numeric(time(test_ts)),
  Actual    = as.numeric(test_ts),
  Predicted = as.numeric(forecast_original)
)

ggplot(plot_test_data, aes(x = Time)) +
  geom_line(aes(y = Actual,    color = "Actual"),    linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  expand_limits(y = c(200, max(plot_test_data$Predicted) + 50)) +
  ggtitle(paste0("Actual vs Predicted Domestic Consumption - ARIMAX (MAPE: ", round(mape, 2), "%)")) +
  xlab("Year") +
  ylab("GWh") +
  theme_minimal() +
  theme(
    legend.title  = element_blank(),
    legend.position = "bottom",
    plot.title    = element_text(size = 11),
    axis.title    = element_text(size = 9)
  )

future_exo <- matrix(NA, nrow = 3, ncol = ncol(xreg_train))
colnames(future_exo) <- colnames(xreg_train)

for (v in colnames(xreg_train)) {
  
  x_ts            <- ts(xreg_all[, v], start = c(2000, 1), frequency = 12)
  exo_model       <- auto.arima(x_ts)
  future_exo[, v] <- as.numeric(forecast(exo_model, h = 3)$mean)
  
  cat("Forecasted:", v, "→", round(future_exo[, v], 3), "\n")
}

print(round(future_exo, 3))

xreg_future_all <- rbind(xreg_test, future_exo)

future_bc <- suppressWarnings(
  forecast(arimax_bc_model, xreg = xreg_future_all, h = 27)
)

future_original <- InvBoxCox(future_bc$mean, lambda)
future_3        <- tail(future_original, 3)

last_time  <- time(test_ts)[length(test_ts)]
last_year  <- floor(last_time)
last_month <- round((last_time - last_year) * 12) + 1

future_dates <- seq(
  from   = as.Date(paste0(last_year, "-", last_month, "-01")),
  by     = "month",
  length.out = 4
)[-1]

future_labels <- format(future_dates, "%b %Y")

data.frame(
  Month        = future_labels,
  Forecast_GWh = as.numeric(future_3)
)
