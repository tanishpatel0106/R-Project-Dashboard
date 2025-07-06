# 📊 Sales Dashboard (Shiny)

A dynamic and interactive financial dashboard built using R and Shiny, designed to analyze and visualize sales, expenses, forecasts, and profitability for specific sites and periods. This application integrates monthly actuals with forecast data to deliver rich KPI visualizations and comparative analysis.

---

## 🧩 Features

* 📈 **KPI Bar Plot** – Visual comparison of actual vs. forecast metrics (Income, COGS, Gross Profit, EBITDA) for Periods 1–6 of 2025
* 🧮 **KPI Cards** – Delta indicators (MoM, YoY, Forecast Δ) for each financial metric
* 🍩 **Expense Distribution Donut Chart** – Hierarchical sunburst chart for COGS and Expenses breakdown
* 🔮 **GL Forecast Trend** – Month-wise forecast vs actual line charts for Income, COGS, and Expenses
* 📋 **Data Validation Tab** – Filtered raw sales data table and summary statistics using `skimr`
* 💾 **CSV Export** – Downloadable KPI summary

---

## 🗂️ Folder Structure

```
.
├── app.R                  # Main Shiny application
├── data/
│   ├── output-1.xlsx      # Actual financial data
│   └── Forecast.xlsx      # Forecasted financial data
├── README.md              # Project documentation
```

---

## 📦 Dependencies

This project uses the following R packages:

```r
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(DT)
library(shinycssloaders)
library(bslib)
library(skimr)
```

Install all required packages with:

```r
install.packages(c(
  "shiny", "readxl", "dplyr", "tidyr", "lubridate", "ggplot2",
  "plotly", "scales", "DT", "shinycssloaders", "bslib", "skimr"
))
```

---

## 🚀 How to Run the App

1. Clone or download this repository.
2. Place your Excel data files (`output-1.xlsx`, `Forecast.xlsx`) inside the `/data` folder.
3. Open RStudio or your R console.
4. Run the app:

```r
shiny::runApp("app.R")
```

> The app will launch in your browser at `http://127.0.0.1:xxxx`.

---

## 🧠 Data Logic Summary

* **Source Filtering**: Data is filtered for `CompanyCode == "C077"` and `SiteCode == "L077"` for years 2024–2025.
* **KPI Derivations**:

  * `Gross_Profit = Income - COGS`
  * `EBITDA = Gross_Profit - Expenses + Other_Income`
* **Comparative Deltas**:

  * MoM Δ: Current vs Previous Period (P6 vs P5)
  * YoY Δ: Current vs Same Period Last Year (P6 2025 vs P6 2024)
  * Forecast Δ: Current vs Forecast (P6 2025)

---

## 📌 Project Scope

This dashboard is tailored to **Period 6 of 2025**, offering:

* Forecast analysis up to P6.
* MoM, YoY, and Forecast comparison of KPIs.
* A sunburst breakdown of expenses and COGS.
* A clean interface using the Flatly theme from `bslib`.

---

## 📤 Output

* 📄 `kpi_summary.csv`: Downloadable summary of monthly KPI aggregates.
* 📊 Interactive charts (Plotly-enabled)
* 📘 Summary statistics from `skimr` in the Data Validation tab.

---

## 🧑‍💻 Author

**Tanish Patel**
For inquiries or extensions, please contact the developer.
