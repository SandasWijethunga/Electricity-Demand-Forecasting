# Electricity Demand Forecasting: Statistical and Neural Approaches

Sector-wise forecasting research on domestic and industrial electricity consumption, comparing statistical time-series models with an autoregressive neural-network approach.

## Research scope

- Observation period in the supplied workbook: January 2000–December 2024 (300 monthly observations).
- Sectors: domestic and industrial electricity consumption.
- Statistical analyses supplied as rendered R reports: ARIMA, SARIMA, ARIMAX, and SARIMAX.
- Neural forecasting supplied as Python notebooks: domestic and industrial AR-Net implementations.

## Repository contents

- `notebooks/`: supplied Python notebooks for domestic and industrial AR-Net experiments.
- `R/`: R code extracted from the supplied rendered HTML analyses. Review and test before treating these scripts as clean, reproducible runs.
- `data/`: instructions for adding data only when redistribution is permitted.
- `results/` and `visualizations/`: locations for verified comparison tables and figures.

## Data access

The original Excel workbook is intentionally not included. Confirm redistribution rights for all source series before publishing a dataset. See `data/README.md`.

## Setup

Create an environment and install the Python dependencies:

```bash
python -m venv .venv
# Windows: .venv\Scripts\activate
# macOS/Linux: source .venv/bin/activate
pip install -r requirements.txt
```

The supplied notebooks were authored for Google Colab and currently use Colab's upload interface. They require adaptation of the data-loading cell before running locally.

For R analyses, install the packages referenced by the scripts (including `readxl`, `forecast`, `tseries`, and `ggplot2`) and update any local file paths.

## Evaluation and results

Add only verified metrics calculated from the final evaluation protocol. Clearly identify the test period, forecast horizon, units, and metric definitions. Include a Diebold–Mariano comparison only if the test has actually been run on comparable forecast errors.

## Publication status

The associated research has been selected for conference presentation. The unpublished manuscript is not included in this repository. A citation and publication link can be added when public release is permitted.

## Reproducibility notes

The supplied notebooks and extracted R code are preserved as source material, not yet independently rerun. Review data paths, preprocessing, train/test splits, package versions, and saved outputs before describing results as reproducible.
