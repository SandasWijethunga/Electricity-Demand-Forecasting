# Extracted from the supplied rendered R analysis HTML.
# Review paths, package versions, and execution order before publishing.

library(readxl)

library(forecast)

library(tseries)

library(ggplot2)

df <- read_excel("C:/Users/ASUS/OneDrive/Desktop/final_research/data set/Research_data_set.xlsx")
str(df)

industrial <- as.numeric(df[[10]])   

industrial_ts <- ts(
  industrial,
  start     = c(2000, 1),
  frequency = 12
)

xreg_all <- cbind(
  MaxRH      = as.numeric(df[[5]]),
  IIp        = as.numeric(df[[7]]),
  MaxTemp    = as.numeric(df[[3]])
  
)

cat("Any NAs in xreg_all:", anyNA(xreg_all), "\n")

head(xreg_all)

# Impute any NAs using column means
for (col in colnames(xreg_all)) {
  na_idx <- is.na(xreg_all[, col])
  if (any(na_idx)) {
    fill_val <- mean(xreg_all[, col], na.rm = TRUE)
    xreg_all[na_idx, col] <- fill_val
    cat("Imputed", sum(na_idx), "NA(s) in", col,
        "with mean =", round(fill_val, 3), "\n")
  }
}

cat("Any NAs after imputation:", anyNA(xreg_all), "\n")

test_size      <- 24
n              <- length(industrial_ts)
train_end_idx  <- n - test_size
test_start_idx <- train_end_idx + 1

# ── Target splits
train_ts <- window(industrial_ts,
                   end   = time(industrial_ts)[train_end_idx])
test_ts  <- window(industrial_ts,
                   start = time(industrial_ts)[test_start_idx])

# ── Exogenous splits
selected_vars <- c( "MaxRH","IIp","MaxTemp")

xreg_train <- xreg_all[1:train_end_idx,  selected_vars, drop = FALSE]
xreg_test  <- xreg_all[test_start_idx:n, selected_vars, drop = FALSE]

# Fix any NAs
for (j in 1:ncol(xreg_test)) {
  na_idx <- which(is.na(xreg_test[, j]))
  if (length(na_idx) > 0)
    xreg_test[na_idx, j] <- tail(xreg_train[, j], 1)
}

cat("Final selected:", paste(selected_vars, collapse = ", "), "\n")

cat("Train observations:", nrow(xreg_train), "\n")

cat("Test  observations:", nrow(xreg_test),  "\n")

lambda <- BoxCox.lambda(train_ts)
lambda

bc_train_ts <- BoxCox(train_ts, lambda)

autoplot(bc_train_ts) +
  ggtitle("Box-Cox Transformed Industrial Training Series") +
  xlab("Year") +
  ylab("Box-Cox Industrial")

bc_diff1          <- diff(bc_train_ts, differences = 1)
bc_diff1_seasonal <- diff(bc_diff1, lag = 12)

autoplot(bc_diff1_seasonal) +
  ggtitle("Box-Cox Industrial: First + Seasonal Differenced") +
  xlab("Year") +
  ylab("Differenced Box-Cox Industrial")

par(mfrow = c(1, 2))

Acf(bc_diff1_seasonal,  lag.max = 72,
    main = "ACF: Box-Cox + First + Seasonal Difference")
Pacf(bc_diff1_seasonal, lag.max = 72,
    main = "PACF: Box-Cox + First + Seasonal Difference")

par(mfrow = c(1, 1))

sarimax_bc_model <- auto.arima(
  bc_train_ts,
  xreg          = xreg_train,   
  d             = 1,
  D             = 1,
  seasonal      = TRUE,
  stepwise      = FALSE,
  approximation = FALSE
)

summary(sarimax_bc_model)

checkresiduals(sarimax_bc_model)

Box.test(
  residuals(sarimax_bc_model),
  lag  = 24,
  type = "Ljung-Box"
)

forecast_bc <- suppressWarnings(
  forecast(sarimax_bc_model, xreg = xreg_test, h = 24)
)

forecast_original <- InvBoxCox(forecast_bc$mean, lambda)

cat("Any NA in forecast_original:", anyNA(forecast_original), "\n")

actual <- as.numeric(test_ts)
pred   <- as.numeric(forecast_original)

rmse <- sqrt(mean((actual - pred)^2))
mae  <- mean(abs(actual - pred))
mape <- mean(abs((actual - pred) / actual)) * 100

cat("Test RMSE =", rmse, "\n")

cat("Test MAE  =", mae,  "\n")

cat("Test MAPE =", mape, "%\n")

plot_test_data_sarimax <- data.frame(
  Time      = as.numeric(time(test_ts)),
  Actual    = actual,
  Predicted = pred
)

ggplot(plot_test_data_sarimax, aes(x = Time)) +
  geom_line(aes(y = Actual,    color = "Actual"),    linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 0.8,
            linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  expand_limits(y = c(min(actual) * 0.7, max(pred) * 1.1)) +
  ggtitle(paste0("Actual vs Predicted Industrial Consumption - SARIMAX (MAPE: ",
                 round(mape, 2), "%)")) +
  xlab("Year") +
  ylab("GWh") +
  theme_minimal() +
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    plot.title      = element_text(size = 11),
    axis.title      = element_text(size = 9)
  )

future_exo <- matrix(NA, nrow = 3, ncol = ncol(xreg_train))
colnames(future_exo) <- colnames(xreg_train)

for (v in colnames(xreg_train)) {
  x_ts               <- ts(xreg_all[, v], start = c(2000, 1), frequency = 12)
  exo_model          <- auto.arima(x_ts)
  future_exo[, v]    <- as.numeric(forecast(exo_model, h = 3)$mean)
  cat("Forecasted:", v, "→", round(future_exo[, v], 3), "\n")
}

print(round(future_exo, 3))

h_total         <- 24 + 3
xreg_future_all <- rbind(xreg_test, future_exo)

future_bc <- suppressWarnings(
  forecast(sarimax_bc_model, xreg = xreg_future_all, h = h_total)
)

future_original <- InvBoxCox(future_bc$mean, lambda)
future_3        <- tail(future_original, 3)

last_time  <- time(test_ts)[length(test_ts)]
last_year  <- floor(last_time)
last_month <- round((last_time - last_year) * 12) + 1

future_dates <- seq(
  from       = as.Date(paste0(last_year, "-", last_month, "-01")),
  by         = "month",
  length.out = 4
)[-1]

future_labels <- format(future_dates, "%b %Y")

future_forecast_table <- data.frame(
  Month        = future_labels,
  Forecast_GWh = round(as.numeric(future_3), 3)
)

print(future_forecast_table)
