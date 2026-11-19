
## Results and Model Evaluation

The forecasting models were evaluated using a common
24-month out-of-sample testing period (January 2023 –
December 2024).

### Industrial Electricity Demand

| Model | RMSE (GWh) | MAE (GWh) | MAPE |
|---|---:|---:|---:|
| SARIMA | 41.3014 | 37.2678 | 9.9281% |
| SARIMAX | 34.0899 | 31.0391 | 8.2810% |
| Multivariate AR-Net | 33.7377 | 23.4929 | 6.57% |

### Domestic Electricity Demand

| Model | RMSE (GWh) | MAE (GWh) | MAPE |
|---|---:|---:|---:|
| ARIMA | 67.9628 | 65.3856 | 17.3519% |
| ARIMAX | 24.4961 | 21.2320 | 5.6827% |
| Multivariate AR-Net | 20.1374 | 16.4838 | 4.3422% |

### Key Findings

- Multivariate AR-Net achieved the best forecasting
  performance in both sectors.
- Domestic forecasting achieved a MAPE of 4.34%.
- Industrial forecasting achieved a MAPE of 6.57%.
- Exogenous variables substantially improved the
  performance of statistical forecasting models.
- The results demonstrate the effectiveness of
  multivariate modelling for sector-level electricity
  demand forecasting.

### Three-Month Ahead Forecast (2025)

| Month | Industrial (GWh) | Domestic (GWh) |
|---|---:|---:|
| January | 384.878 | 392.205 |
| February | 350.398 | 460.603 |
| March | 344.393 | 428.356 |

