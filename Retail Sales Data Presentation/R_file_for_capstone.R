# Online Retail analysis (revenue by month/country, invoice-level clustering).
# Run from this folder (setwd to it). Writes to ./outputs (git-ignored).
# Packages: install beforehand, e.g.
#   install.packages(c("readxl", "dplyr", "ggplot2", "writexl"))

library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

data_file  <- "Online Retail.xlsx"
output_dir <- "outputs"
dir.create(output_dir, showWarnings = FALSE)

white_theme <- theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Load (once) ----
retail_raw <- read_excel(data_file, sheet = "Online Retail")

retail_all <- retail_raw %>%
  mutate(
    TotalPrice   = Quantity * UnitPrice,
    InvoiceDay   = as.Date(InvoiceDate),
    InvoiceMonth = format(InvoiceDate, "%Y-%m"),   # sorts chronologically
    IsReturn     = Quantity < 0 | startsWith(as.character(InvoiceNo), "C")
  )

# ---- Returns vs. sales ----
# Returns/cancellations have negative Quantity (and "C"-prefixed invoices). They are
# kept out of the sales analysis below and summarised separately.
cat("Rows:", nrow(retail_all), "| return/cancellation rows:", sum(retail_all$IsReturn),
    "| rows with UnitPrice <= 0:", sum(retail_all$UnitPrice <= 0), "\n")

returns_summary <- retail_all %>%
  filter(IsReturn) %>%
  group_by(InvoiceMonth) %>%
  summarise(ReturnRows = n(), ReturnValue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

retail_data <- retail_all %>% filter(!IsReturn, UnitPrice > 0)

# ---- Invoice-level totals ----
# One row per invoice. (Averaging TotalPrice over line items gives the average
# line-item value, not the average invoice total.)
invoice_totals <- retail_data %>%
  group_by(InvoiceNo) %>%
  summarise(InvoiceTotal = sum(TotalPrice, na.rm = TRUE),
            Country      = first(Country),
            InvoiceMonth = first(InvoiceMonth),
            .groups = "drop")

revenue_by_month <- invoice_totals %>%
  group_by(InvoiceMonth) %>%
  summarise(TotalRevenue = sum(InvoiceTotal),
            AverageInvoiceTotal = mean(InvoiceTotal), .groups = "drop")

revenue_by_country <- invoice_totals %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(InvoiceTotal),
            AverageInvoiceTotal = mean(InvoiceTotal), .groups = "drop")

revenue_by_country_month <- invoice_totals %>%
  group_by(Country, InvoiceMonth) %>%
  summarise(TotalRevenue = sum(InvoiceTotal),
            AverageInvoiceTotal = mean(InvoiceTotal), .groups = "drop")

daily_sales <- retail_data %>%
  group_by(InvoiceDay) %>%
  summarise(DailyTotal = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

monthly_quantity <- retail_data %>%
  group_by(InvoiceMonth) %>%
  summarise(TotalQuantity = sum(Quantity, na.rm = TRUE), .groups = "drop")

stock_code_invoice_revenue <- retail_data %>%
  group_by(StockCode, InvoiceNo) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

stock_code_avg_revenue <- stock_code_invoice_revenue %>%
  group_by(StockCode) %>%
  summarise(AverageRevenuePerInvoice = mean(TotalRevenue), .groups = "drop")

# ---- Clustering (exploratory) ----
# k-means on a single, heavily right-skewed variable. Log-transform so a few very
# large wholesale invoices do not dominate; k = 3 is a judgement call, not tuned.
set.seed(123)
invoice_totals <- invoice_totals %>% mutate(LogInvoiceTotal = log10(InvoiceTotal))
km <- kmeans(scale(invoice_totals$LogInvoiceTotal), centers = 3, nstart = 25)
invoice_totals$Cluster <- factor(km$cluster)

cluster_summary <- invoice_totals %>%
  group_by(Cluster) %>%
  summarise(Invoices = n(),
            MinTotal = min(InvoiceTotal),
            MedianTotal = median(InvoiceTotal),
            MaxTotal = max(InvoiceTotal), .groups = "drop")
print(cluster_summary)

# ---- Exports (fixed names, no date stamps) ----
write_xlsx(list(
  InvoiceTotals = invoice_totals,
  RevenueByMonth = revenue_by_month,
  RevenueByCountry = revenue_by_country,
  RevenueByCountryMonth = revenue_by_country_month,
  DailySales = daily_sales,
  MonthlyQuantity = monthly_quantity,
  ReturnsByMonth = returns_summary,
  ClusterSummary = cluster_summary
), path = file.path(output_dir, "OnlineRetailSummary.xlsx"))

write.csv(stock_code_invoice_revenue, file.path(output_dir, "StockCode_Invoice_Revenue.csv"), row.names = FALSE)
write.csv(stock_code_avg_revenue, file.path(output_dir, "StockCode_Avg_Revenue.csv"), row.names = FALSE)

# ---- Charts ----
save_plot <- function(p, name, width = 10, height = 6) {
  ggsave(file.path(output_dir, name), p, width = width, height = height)
}

save_plot(
  ggplot(revenue_by_month, aes(x = InvoiceMonth, y = TotalRevenue)) +
    geom_col(fill = "steelblue") +
    labs(title = "Total Revenue by Month", x = "Month", y = "Total Revenue (GBP)") + white_theme,
  "TotalRevenueByMonth.png")

save_plot(
  ggplot(revenue_by_country, aes(x = reorder(Country, -TotalRevenue), y = TotalRevenue)) +
    geom_col(fill = "coral") +
    labs(title = "Total Revenue by Country", x = "Country", y = "Total Revenue (GBP)") + white_theme,
  "TotalRevenueByCountry.png")

save_plot(
  ggplot(revenue_by_month, aes(x = InvoiceMonth, y = AverageInvoiceTotal)) +
    geom_col(fill = "darkgreen") +
    labs(title = "Average Invoice Total by Month", x = "Month", y = "Average Invoice Total (GBP)") + white_theme,
  "AverageInvoiceByMonth.png")

save_plot(
  ggplot(monthly_quantity, aes(x = InvoiceMonth, y = TotalQuantity, group = 1)) +
    geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
    labs(title = "Monthly Quantity Sold", x = "Month", y = "Units") + white_theme,
  "MonthlyQuantity.png")

save_plot(
  ggplot(daily_sales, aes(x = InvoiceDay, y = DailyTotal)) +
    geom_line(color = "blue") +
    labs(title = "Daily Sales Total", x = "Date", y = "Total Sales (GBP)") + white_theme,
  "DailySales.png")

save_plot(
  ggplot(invoice_totals, aes(x = InvoiceMonth, y = InvoiceTotal, color = Cluster)) +
    geom_jitter(width = 0.3, alpha = 0.6) +
    scale_y_log10() +
    labs(title = "K-Means Clusters of Invoice Totals (log scale)", x = "Month", y = "Invoice Total (GBP)") + white_theme,
  "ClusteringPlot.png", width = 12)
