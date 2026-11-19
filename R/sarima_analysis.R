# Extracted from the supplied rendered R analysis HTML.
# Review paths, package versions, and execution order before publishing.

library(readxl)

library(forecast)

library(tseries)

library(ggplot2)

df <- read_excel("C:/Users/ASUS/OneDrive/Desktop/final_research/data set/Research_data_set.xlsx")
str(df)

industrial <- as.numeric(df$`Industrial (GWh)`)

industrial_ts <- ts(
  industrial,
  start = c(2000, 1),
  frequency = 12
)

test_size <- 24

train_ts <- window(
  industrial_ts,
  end = time(industrial_ts)[length(industrial_ts) - test_size]
)

test_ts <- window(
  industrial_ts,
  start = time(industrial_ts)[length(industrial_ts) - test_size + 1]
)

lambda <- BoxCox.lambda(train_ts)
lambda

bc_train_ts <- BoxCox(train_ts, lambda)

autoplot(bc_train_ts) +
  ggtitle("Box-Cox Transformed Industrial Training Series") +
  xlab("Year") +
  ylab("Box-Cox Industrial")

bc_diff1 <- diff(bc_train_ts, differences = 1)

bc_diff1_seasonal <- diff(bc_diff1, lag = 12)

autoplot(bc_diff1_seasonal) +
  ggtitle("Box-Cox Industrial: First + Seasonal Differenced") +
  xlab("Year") +
  ylab("Differenced Box-Cox Industrial")

par(mfrow = c(1, 2))

Acf(
  bc_diff1_seasonal,
  lag.max = 72,
  main = "ACF: Box-Cox + First + Seasonal Difference"
)

Pacf(
  bc_diff1_seasonal,
  lag.max = 72,
  main = "PACF: Box-Cox + First + Seasonal Difference"
)

par(mfrow = c(1, 1))

sarima_bc_model <- auto.arima(
  bc_train_ts,
  d = 1,
  D = 1,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(sarima_bc_model)

checkresiduals(sarima_bc_model)

Box.test(
  residuals(sarima_bc_model),
  lag = 24,
  type = "Ljung-Box"
)

# forecast testing data
forecast_bc <- forecast(sarima_bc_model, h = 24)

forecast_original <- InvBoxCox(forecast_bc$mean, lambda)

actual <- as.numeric(test_ts)
pred   <- as.numeric(forecast_original)

rmse <- sqrt(mean((actual - pred)^2))
mae  <- mean(abs(actual - pred))
mape <- mean(abs((actual - pred)/actual)) * 100

cat("Test RMSE =", rmse, "\n")

cat("Test MAE  =", mae, "\n")

cat("Test MAPE =", mape, "%\n")

plot_test_data_sarima <- data.frame(
  Time = as.numeric(time(test_ts)),
  Actual = actual,
  Predicted = pred
)

ggplot(plot_test_data_sarima, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) + 
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  
  expand_limits(y = c(min(actual) * 0.7, max(pred) * 1.1)) + 
  
  ggtitle(paste0("Actual vs Predicted Industrial Consumption (MAPE: ", round(mape, 2), "%)")) +
  xlab("Year") + 
  ylab("GWh") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 11),
    axis.title = element_text(size = 9)
  )

h_total <- 24 + 3   # 24 test + 3 future

future_bc <- forecast(sarima_bc_model, h = h_total)

# Convert back
future_original <- InvBoxCox(future_bc$mean, lambda)

future_3 <- tail(future_original, 3)

last_time <- time(test_ts)[length(test_ts)]

last_year  <- floor(last_time)
last_month <- round((last_time - last_year) * 12) + 1

future_dates <- seq(
  from = as.Date(paste0(last_year, "-", last_month, "-01")),
  by = "month",
  length.out = 4
)[-1]

future_labels <- format(future_dates, "%b %Y")

future_forecast_table <- data.frame(
  Month = future_labels,
  Forecast_GWh = as.numeric(future_3)
)

print(future_forecast_table)
