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

# ---- Load Data ----

data_path <- "data"

sales_data <- read_excel(file.path(data_path, "output-1.xlsx"), sheet = "Sheet1")
forecast_data <- read_excel(file.path(data_path, "Forecast.xlsx"), sheet = "Sheet1")

sales_filtered <- sales_data %>%
  filter(CompanyCode == "C077", SiteCode == "L077", Year >= 2024, Year <= 2025)

sales_summary <- sales_filtered %>%
  group_by(Year, Period) %>%
  summarise(
    Income = sum(TotalAmount[AccountType == "Income"], na.rm = TRUE),
    COGS = sum(TotalAmount[AccountType == "CostOfGoodsSold"], na.rm = TRUE),
    Expenses = sum(TotalAmount[AccountType == "Expense"], na.rm = TRUE),
    Other_Income = sum(TotalAmount[AccountType == "OtherIncome"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Gross_Profit = Income - COGS,
    EBITDA = Gross_Profit - Expenses + Other_Income,
    date = make_date(Year, Period, 1)
  ) %>%
  arrange(date)

forecast_summary <- forecast_data %>%
  filter(CompanyCode == "C077", SiteCode == "L077") %>%
  group_by(Forecasted_For_Year, Forecasted_For_Period, Field) %>%
  summarise(Forecasted = sum(Forecasted_Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = make_date(Forecasted_For_Year, Forecasted_For_Period, 1)) %>%
  pivot_wider(names_from = Field, values_from = Forecasted)

current <- sales_summary %>% filter(Year == 2025, Period == 6)
prev_month <- sales_summary %>% filter(Year == 2025, Period == 5)
year_ago <- sales_summary %>% filter(Year == 2024, Period == 6)
current_forecast <- forecast_summary %>% filter(Forecasted_For_Year == 2025, Forecasted_For_Period == 6)

actual_vals <- c(
  Income = current$Income,
  COGS = current$COGS,
  Gross_Profit = current$Gross_Profit,
  EBITDA = current$EBITDA,
  Expenses = current$Expenses
)

prev_vals <- c(
  Income = prev_month$Income,
  COGS = prev_month$COGS,
  Gross_Profit = prev_month$Gross_Profit,
  EBITDA = prev_month$EBITDA,
  Expenses = prev_month$Expenses
)

yearago_vals <- c(
  Income = year_ago$Income,
  COGS = year_ago$COGS,
  Gross_Profit = year_ago$Gross_Profit,
  EBITDA = year_ago$EBITDA,
  Expenses = year_ago$Expenses
)

forecast_vals <- c(
  Income = current_forecast$Income,
  COGS = current_forecast$COGS,
  Gross_Profit = current_forecast$Income - current_forecast$COGS,
  EBITDA = current_forecast$Income - current_forecast$COGS - current_forecast$Expense,
  Expenses = current_forecast$Expense
)

mom_delta <- (actual_vals - prev_vals) / prev_vals
yoy_delta <- (actual_vals - yearago_vals) / yearago_vals
forecast_delta <- (actual_vals - forecast_vals) / forecast_vals

# Data for top bar plot
forecast_long <- forecast_summary %>%
  mutate(
    Gross_Profit = Income - COGS,
    EBITDA = Income - COGS - Expense
  ) %>%
  select(date, Income, COGS, Gross_Profit, EBITDA) %>%
  pivot_longer(-date, names_to = "metric", values_to = "Forecast")

top_plot_data <- sales_summary %>%
  filter(Year == 2025, Period <= 6) %>%
  select(date, Income, COGS, Gross_Profit, EBITDA) %>%
  pivot_longer(-date, names_to = "metric", values_to = "Actual") %>%
  left_join(forecast_long, by = c("date", "metric"))

# Data for mini forecast chart
mini_actual <- sales_summary %>%
  filter(Year == 2025, Period <= 6) %>%
  select(date, Income, COGS, Expenses) %>%
  pivot_longer(-date, names_to = "Field", values_to = "Actual")

mini_forecast <- forecast_summary %>%
  filter(Forecasted_For_Year == 2025, Forecasted_For_Period <= 6) %>%
  select(date, Income, COGS, Expense) %>%
  rename(Expenses = Expense) %>%
  pivot_longer(-date, names_to = "Field", values_to = "Forecast")

mini_data <- left_join(mini_actual, mini_forecast, by = c("date", "Field"))

# Expense donut data
donut_inner <- sales_filtered %>%
  filter(Year == 2025, Period == 6, AccountType %in% c("CostOfGoodsSold", "Expense")) %>%
  group_by(AccountType) %>%
  summarise(value = sum(TotalAmount, na.rm = TRUE), .groups = "drop")

donut_outer <- sales_filtered %>%
  filter(Year == 2025, Period == 6, AccountType %in% c("CostOfGoodsSold", "Expense")) %>%
  group_by(AccountType, AccountNumber, Description) %>%
  summarise(value = sum(TotalAmount, na.rm = TRUE), .groups = "drop")

theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- navbarPage(
  title = "Sales Dashboard",
  theme = theme,
  tabPanel(
    "Dashboard",
    fluidPage(
      fluidRow(
        column(12, withSpinner(plotlyOutput("kpi_bar")))
      ),
      fluidRow(
        column(2, uiOutput("income_box")),
        column(2, uiOutput("cogs_box")),
        column(2, uiOutput("gp_box")),
        column(2, uiOutput("ebitda_box")),
        column(2, uiOutput("exp_box"))
      ),
      fluidRow(
        column(6, withSpinner(plotlyOutput("expense_donut"))),
        column(6, withSpinner(plotlyOutput("gl_forecast")))
      )
    )
  ),
  tabPanel(
    "Data Validation",
    h4("Raw Data - Period 6, 2025"),
    withSpinner(DTOutput("data_table")),
    h4("Summary Statistics"),
    verbatimTextOutput("data_summary")
  ),
  tabPanel(
    "Download",
    downloadButton("download_summary", "Download KPI Summary")
  )
)

server <- function(input, output, session) {
  output$kpi_bar <- renderPlotly({
    p <- ggplot(top_plot_data, aes(x = date, y = Actual, fill = metric)) +
      geom_col(position = position_dodge()) +
      geom_line(aes(y = Forecast, color = metric, group = metric),
                linetype = "dotted", size = 1) +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_labels = "%b", breaks = unique(top_plot_data$date)) +
      labs(x = NULL, y = NULL, fill = NULL, color = NULL) +
      theme_minimal()
    ggplotly(p)
  })

  make_box <- function(title, value, mom, yoy, fcst, color) {
    value_box(
      title = title,
      value = comma(value),
      p(tags$ul(
        tags$li(paste0("MoM Δ: ", percent(mom, accuracy = 0.1))),
        tags$li(paste0("YoY Δ: ", percent(yoy, accuracy = 0.1))),
        tags$li(paste0("Forecast Δ: ", percent(fcst, accuracy = 0.1)))
      )),
      theme_color = color
    )
  }

  output$income_box <- renderUI({
    make_box("Income", actual_vals["Income"], mom_delta["Income"],
             yoy_delta["Income"], forecast_delta["Income"], "primary")
  })
  output$cogs_box <- renderUI({
    make_box("COGS", actual_vals["COGS"], mom_delta["COGS"],
             yoy_delta["COGS"], forecast_delta["COGS"], "danger")
  })
  output$gp_box <- renderUI({
    make_box("Gross Profit", actual_vals["Gross_Profit"], mom_delta["Gross_Profit"],
             yoy_delta["Gross_Profit"], forecast_delta["Gross_Profit"], "success")
  })
  output$ebitda_box <- renderUI({
    make_box("EBITDA", actual_vals["EBITDA"], mom_delta["EBITDA"],
             yoy_delta["EBITDA"], forecast_delta["EBITDA"], "warning")
  })
  output$exp_box <- renderUI({
    make_box("Expenses", actual_vals["Expenses"], mom_delta["Expenses"],
             yoy_delta["Expenses"], forecast_delta["Expenses"], "secondary")
  })

  output$expense_donut <- renderPlotly({
    sb_data <- bind_rows(
      donut_inner %>% transmute(labels = AccountType, parents = "", values = value),
      donut_outer %>% transmute(labels = Description, parents = AccountType, values = value)
    )
    plot_ly(sb_data, labels = ~labels, parents = ~parents, values = ~values,
            type = "sunburst", branchvalues = "total")
  })

  output$gl_forecast <- renderPlotly({
    p <- ggplot(mini_data, aes(x = date)) +
      geom_line(aes(y = Actual, color = Field)) +
      geom_line(aes(y = Forecast, color = Field), linetype = "dotted") +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_labels = "%b", breaks = unique(mini_data$date)) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_minimal()
    ggplotly(p)
  })

  output$data_table <- renderDT({
    datatable(sales_filtered %>% filter(Year == 2025, Period == 6))
  })

  output$data_summary <- renderPrint({
    skimr::skim(sales_filtered %>% filter(Year == 2025, Period == 6))
  })

  output$download_summary <- downloadHandler(
    filename = function() {
      "kpi_summary.csv"
    },
    content = function(file) {
      write.csv(sales_summary, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
