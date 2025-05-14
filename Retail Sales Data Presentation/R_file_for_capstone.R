#clear environment
rm(list = ls(all.names = TRUE))


library(readxl)

# Define the path
file_path <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set/Online Retail.xlsx"

# Read Excel file 
retail_data <- read_excel(file_path)

# View the first few rows
head(retail_data)

library(dplyr)

# total price column
retail_data <- retail_data %>%
  mutate(TotalPrice = Quantity * UnitPrice)

#Extract just the Date (without time)
retail_data <- retail_data %>%
  mutate(InvoiceDay = as.Date(InvoiceDate))

#Group by day and sum sales
daily_sales <- retail_data %>%
  group_by(InvoiceDay) %>%
  summarise(DailyTotal = sum(TotalPrice, na.rm = TRUE))

#View results
head(daily_sales)

library(ggplot2)

ggplot(daily_sales, aes(x = InvoiceDay, y = DailyTotal)) +
  geom_line(color = "blue") +
  labs(title = "Daily Sales Total", x = "Date", y = "Total Sales (£)") +
  theme_minimal()

# Load today's date
today_date <- Sys.Date()

#Define folder path
folder_path <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set"

#Create file name with today's date
file_name <- paste0("sales_by_date_", today_date, ".csv")

#path to save the CSV
output_path <- file.path(folder_path, file_name)

#Write CSV
write.csv(daily_sales, output_path, row.names = FALSE)

#confirmation
cat("Exported to:", output_path, "\n")

#open CSV file 
browseURL(output_path)


library(lubridate)

#Format InvoiceDay and YearMonth
daily_sales <- daily_sales %>%
  mutate(
    YearMonth = format(InvoiceDay, "%B %Y"),               # "December 2010"
    InvoiceDayFormatted = format(InvoiceDay, "%B %e, %Y")  # "December 1, 2010"
  )

#Find top sales day for each month
max_sales_per_month <- daily_sales %>%
  group_by(YearMonth) %>%
  slice_max(order_by = DailyTotal, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(InvoiceDay)

#Select clean table
max_sales_per_month_clean <- max_sales_per_month %>%
  select(YearMonth, InvoiceDay = InvoiceDayFormatted, DailyTotal)

#Export to CSV
folder_path <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set"
today_date <- Sys.Date()
file_name <- paste0("top_sales_day_per_month_", today_date, ".csv")
output_path <- file.path(folder_path, file_name)

write.csv(max_sales_per_month_clean, output_path, row.names = FALSE)
browseURL(output_path)  # Open file automatically (Windows)

#Create bar chart
ggplot(max_sales_per_month_clean, aes(x = reorder(YearMonth, DailyTotal), y = DailyTotal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top Sales Day of Each Month",
    x = "Month",
    y = "Total Sales (£)"
  ) +
  theme_minimal()

#Create a Year-Month column 
retail_data <- retail_data %>%
  mutate(YearMonth = format(InvoiceDay, "%Y-%m"))

#Group by YearMonth and sum TotalPrice
monthly_sales <- retail_data %>%
  group_by(YearMonth) %>%
  summarise(MonthlyTotal = sum(TotalPrice, na.rm = TRUE)) %>%
  ungroup()

# Preview
head(monthly_sales)

#Plot
ggplot(monthly_sales, aes(x = YearMonth, y = MonthlyTotal, group = 1)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen") +
  labs(
    title = "Monthly Sales Total",
    x = "Month",
    y = "Total Sales (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Define export path
file_name <- paste0("monthly_sales_", today_date, ".csv")
output_path <- file.path(folder_path, file_name)

#Export to CSV
write.csv(monthly_sales, output_path, row.names = FALSE)
browseURL(output_path)

#Ensure InvoiceDay is in Date format
retail_data <- retail_data %>%
  mutate(
    InvoiceDay = as.Date(InvoiceDate),
    YearMonth = format(InvoiceDay, "%B %Y")  # e.g., "December 2010"
  )

#Group by InvoiceDay and sum Quantity
daily_quantity <- retail_data %>%
  group_by(InvoiceDay, YearMonth) %>%
  summarise(DailyQuantity = sum(Quantity, na.rm = TRUE), .groups = "drop")

#Find top sales day by quantity for each month
top_quantity_days <- daily_quantity %>%
  group_by(YearMonth) %>%
  slice_max(order_by = DailyQuantity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(InvoiceDay)

#Clean and format for export
top_quantity_days_clean <- top_quantity_days %>%
  mutate(InvoiceDayFormatted = format(InvoiceDay, "%B %e, %Y")) %>%
  select(YearMonth, InvoiceDay = InvoiceDayFormatted, DailyQuantity)

#Export to CSV
today_date <- Sys.Date()
file_name <- paste0("top_quantity_day_per_month_", today_date, ".csv")
output_path <- file.path(folder_path, file_name)

write.csv(top_quantity_days_clean, output_path, row.names = FALSE)
browseURL(output_path)



ggplot(top_quantity_days_clean, aes(x = reorder(YearMonth, DailyQuantity), y = DailyQuantity)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(
    title = "Top Sales Day of Each Month (by Quantity)",
    x = "Month",
    y = "Total Quantity Sold"
  ) +
  theme_minimal()



#Ensure InvoiceDay and YearMonth are present
retail_data <- retail_data %>%
  mutate(
    InvoiceDay = as.Date(InvoiceDate),
    YearMonth = format(InvoiceDay, "%Y-%m")  # e.g., "2010-12"
  )

#Group by YearMonth and sum Quantity
monthly_quantity <- retail_data %>%
  group_by(YearMonth) %>%
  summarise(MonthlyQuantity = sum(Quantity, na.rm = TRUE)) %>%
  ungroup()

#Preview results
head(monthly_quantity)



ggplot(monthly_quantity, aes(x = YearMonth, y = MonthlyQuantity, group = 1)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple") +
  labs(
    title = "Total Quantity of Items Sold by Month",
    x = "Month",
    y = "Total Quantity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(monthly_quantity, aes(x = YearMonth, y = MonthlyQuantity, group = 1)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple") +
  labs(
    title = "Total Quantity of Items Sold by Month",
    x = "Month",
    y = "Total Quantity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#save CSV
file_name <- paste0("monthly_quantity_", Sys.Date(), ".csv")
output_path <- file.path(folder_path, file_name)

write.csv(monthly_quantity, output_path, row.names = FALSE)
browseURL(output_path)



#Ensure InvoiceDay and YearMonth are present
retail_data <- retail_data %>%
  mutate(
    InvoiceDay = as.Date(InvoiceDate),
    YearMonth = format(InvoiceDay, "%Y-%m")
  )

#Group by YearMonth and compute both totals
monthly_summary <- retail_data %>%
  group_by(YearMonth) %>%
  summarise(
    MonthlyRevenue = sum(TotalPrice, na.rm = TRUE),
    MonthlyQuantity = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  )

#Preview combined table
head(monthly_summary)

#Save to CSV
file_name <- paste0("monthly_summary_", Sys.Date(), ".csv")
output_path <- file.path(folder_path, file_name)

write.csv(monthly_summary, output_path, row.names = FALSE)
browseURL(output_path)



#install uninstalled packages
install.packages("openxlsx") 
install.packages("zoo")
install.packages("writexl")

# Load necessary libraries



library(writexl)

# Define file paths
file_path <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set/Online Retail.xlsx"
output_dir <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/graphics/"

#Read Excel file into retail_data
retail_data <- read_excel(file_path)

#Calculate TotalPrice if it is not already present
retail_data <- retail_data %>%
  mutate(TotalPrice = Quantity * UnitPrice)

#Create InvoiceMonth column as Year-Month format
retail_data <- retail_data %>%
  mutate(InvoiceMonth = format(InvoiceDate, "%B %Y"))

#Group data for charts and calculations
revenue_by_month <- retail_data %>%
  group_by(InvoiceMonth) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

revenue_by_country <- retail_data %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

average_by_month <- retail_data %>%
  group_by(InvoiceMonth) %>%
  summarise(AverageInvoiceTotal = mean(TotalPrice, na.rm = TRUE), .groups = "drop")

average_by_country <- retail_data %>%
  group_by(Country) %>%
  summarise(AverageInvoiceTotal = mean(TotalPrice, na.rm = TRUE), .groups = "drop")

revenue_by_country_month <- retail_data %>%
  group_by(Country, InvoiceMonth) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

average_by_country_month <- retail_data %>%
  group_by(Country, InvoiceMonth) %>%
  summarise(AverageInvoiceTotal = mean(TotalPrice, na.rm = TRUE), .groups = "drop")

#Save data to Excel file
write_xlsx(list(
  "Revenue by Month" = revenue_by_month,
  "Revenue by Country" = revenue_by_country,
  "Average by Month" = average_by_month,
  "Average by Country" = average_by_country,
  "Revenue by Country and Month" = revenue_by_country_month,
  "Average by Country and Month" = average_by_country_month
), path = "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/Online_Retail_Analysis.xlsx")

# Chart 1: Total Revenue by Month
p1 <- ggplot(revenue_by_month, aes(x = InvoiceMonth, y = TotalRevenue)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(TotalRevenue, 2)), vjust = -0.5, size = 3) +
  labs(title = "Total Revenue by Month", x = "Month", y = "Total Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Chart 1 as a PNG file
ggsave(filename = paste0(output_dir, "total_revenue_by_month.png"), plot = p1, width = 10, height = 6)

# Chart 2: Total Revenue by Country
p2 <- ggplot(revenue_by_country, aes(x = Country, y = TotalRevenue, fill = Country)) +
  geom_col() +
  labs(title = "Total Revenue by Country", x = "Country", y = "Total Revenue") +
  theme_minimal()

# Save Chart 2 as a PNG file
ggsave(filename = paste0(output_dir, "total_revenue_by_country.png"), plot = p2, width = 10, height = 6)

# Chart 3: Average Invoice Total by Month
p3 <- ggplot(average_by_month, aes(x = InvoiceMonth, y = AverageInvoiceTotal)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(AverageInvoiceTotal, 2)), vjust = -0.5, size = 3) +
  labs(title = "Average Invoice Total by Month", x = "Month", y = "Average Invoice Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Chart 3 as a PNG file
ggsave(filename = paste0(output_dir, "average_invoice_total_by_month.png"), plot = p3, width = 10, height = 6)

# Chart 4: Average Invoice Total by Country and Month
p4 <- ggplot(average_by_country_month, aes(x = InvoiceMonth, y = AverageInvoiceTotal, fill = Country)) +
  geom_col(position = "dodge") +
  labs(title = "Average Invoice Total by Country and Month", x = "Month", y = "Average Invoice Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Chart 4 as a PNG file
ggsave(filename = paste0(output_dir, "average_invoice_total_by_country_and_month.png"), plot = p4, width = 10, height = 6)

# Chart 5: Total Revenue by Country and Month
p5 <- ggplot(revenue_by_country_month, aes(x = InvoiceMonth, y = TotalRevenue, fill = Country)) +
  geom_col(position = "dodge") +
  labs(title = "Total Revenue by Country and Month", x = "Month", y = "Total Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Chart 5 as a PNG file
ggsave(filename = paste0(output_dir, "total_revenue_by_country_and_month.png"), plot = p5, width = 10, height = 6)

# Chart 6: Average Invoice Total by Country and Month
p6 <- ggplot(average_by_country_month, aes(x = InvoiceMonth, y = AverageInvoiceTotal, fill = Country)) +
  geom_col(position = "dodge") +
  labs(title = "Average Invoice Total by Country and Month", x = "Month", y = "Average Invoice Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Chart 6 as a PNG file
ggsave(filename = paste0(output_dir, "average_invoice_total_by_country_and_month.png"), plot = p6, width = 10, height = 6)

#k mean clustering

#Check the column names in the dataset
colnames(retail_data)


# Load necessary libraries

library(writexl)

library(lubridate)
library(tidyr)
library(scales)
library(ggthemes)
library(ggrepel)

#Define the theme for white background
my_white_theme <- theme_minimal() + theme(plot.background = element_rect(fill = "white", color = NA),
                                          panel.background = element_rect(fill = "white", color = NA))

#Define file paths
file_path <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set/Online Retail.xlsx"
output_folder <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone"
graphics_folder <- file.path(output_folder, "graphics")
dir.create(graphics_folder, showWarnings = FALSE)

#Load data
retail_data <- read_excel(file_path)

#Create TotalPrice field
retail_data <- retail_data %>% mutate(TotalPrice = Quantity * UnitPrice)

#Convert InvoiceDate to InvoiceMonth
retail_data <- retail_data %>% mutate(InvoiceMonth = format(InvoiceDate, "%B %Y"))

#Summarize by Invoice
invoice_totals <- retail_data %>%
  group_by(InvoiceNo) %>%
  summarise(InvoiceTotal = sum(TotalPrice, na.rm = TRUE),
            Country = first(Country),
            InvoiceMonth = first(format(InvoiceDate, "%B %Y")),
            .groups = "drop")

# Write invoice totals to CSV
write.csv(invoice_totals, file.path(output_folder, paste0("invoice_totals_", Sys.Date(), ".csv")), row.names = FALSE)

#summary by month
revenue_by_month <- invoice_totals %>%
  group_by(InvoiceMonth) %>%
  summarise(TotalRevenue = sum(InvoiceTotal), AverageInvoiceTotal = mean(InvoiceTotal), .groups = "drop")

#Summary by country
revenue_by_country <- invoice_totals %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(InvoiceTotal), AverageInvoiceTotal = mean(InvoiceTotal), .groups = "drop")

#Summary by country and month
revenue_by_country_month <- invoice_totals %>%
  group_by(Country, InvoiceMonth) %>%
  summarise(TotalRevenue = sum(InvoiceTotal),
            AverageInvoiceTotal = mean(InvoiceTotal), .groups = "drop")

#Save summaries to Excel workbook
write_xlsx(list(
  InvoiceTotals = invoice_totals,
  RevenueByMonth = revenue_by_month,
  RevenueByCountry = revenue_by_country,
  RevenueByCountryMonth = revenue_by_country_month
), path = file.path(output_folder, "OnlineRetailSummary.xlsx"))

# Chart 1: Total Revenue by Month
p1 <- ggplot(revenue_by_month, aes(x = InvoiceMonth, y = TotalRevenue)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(TotalRevenue, 2)), vjust = -0.5, size = 3) +
  labs(title = "Total Revenue by Month", x = "Month", y = "Total Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + my_white_theme
ggsave(file.path(graphics_folder, "TotalRevenueByMonth.png"), p1)

# Chart 2: Total Revenue by Country
p2 <- ggplot(revenue_by_country, aes(x = reorder(Country, -TotalRevenue), y = TotalRevenue)) +
  geom_col(fill = "coral") +
  geom_text(aes(label = round(TotalRevenue, 2)), vjust = -0.5, size = 3) +
  labs(title = "Total Revenue by Country", x = "Country", y = "Total Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + my_white_theme
ggsave(file.path(graphics_folder, "TotalRevenueByCountry.png"), p2)

# Chart 3: Average Invoice Total by Month
p3 <- ggplot(revenue_by_month, aes(x = InvoiceMonth, y = AverageInvoiceTotal)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(AverageInvoiceTotal, 2)), vjust = -0.5, size = 3) +
  labs(title = "Average Invoice Total by Month", x = "Month", y = "Average Invoice Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + my_white_theme
ggsave(file.path(graphics_folder, "AverageInvoiceByMonth.png"), p3)

# Chart 4: Average Invoice Total by Country and Month
p4 <- ggplot(revenue_by_country_month, aes(x = InvoiceMonth, y = AverageInvoiceTotal, fill = Country)) +
  geom_col(position = "dodge") +
  labs(title = "Average Invoice Total by Country and Month", x = "Month", y = "Average Invoice Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + my_white_theme
ggsave(file.path(graphics_folder, "AverageInvoiceByCountryMonth.png"), p4)

# clustering setup
clustering_data <- invoice_totals %>% select(InvoiceTotal)
kmeans_result <- kmeans(clustering_data, centers = 3)
invoice_totals$Cluster <- as.factor(kmeans_result$cluster)

#Clustering plot
p5 <- ggplot(invoice_totals, aes(x = InvoiceMonth, y = InvoiceTotal, color = Cluster)) +
  geom_jitter(width = 0.3, alpha = 0.6) +
  labs(title = "K-Means Clustering of Invoice Totals", x = "Month", y = "Invoice Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + my_white_theme
ggsave(file.path(graphics_folder, "ClusteringPlot.png"), p5)





# Define path to Excel file
file_path <- "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set/Online Retail.xlsx"

#read Excel file into the retail_data object
retail_data <- read_excel(file_path)

#Calculate TotalPrice if not already available
retail_data <- retail_data %>%
  mutate(TotalPrice = Quantity * UnitPrice)

#Filter and calculate total revenue for each invoice that contains a particular StockCode
# This will keep only relevant rows for each StockCode and group by InvoiceNo to get the total revenue per invoice
stock_code_invoice_revenue <- retail_data %>%
  group_by(StockCode, InvoiceNo) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

#Optionally calculate the average TotalRevenue per invoice for each StockCode
stock_code_avg_revenue <- stock_code_invoice_revenue %>%
  group_by(StockCode) %>%
  summarise(AverageRevenuePerInvoice = mean(TotalRevenue), .groups = "drop")

#View the results for stock_code_invoice_revenue and stock_code_avg_revenue
print(stock_code_invoice_revenue)
print(stock_code_avg_revenue)

#Optionally, save the results as CSVs
write.csv(stock_code_invoice_revenue, "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set/StockCode_Invoice_Revenue.csv", row.names = FALSE)
write.csv(stock_code_avg_revenue, "C:/Users/micah/OneDrive/Classes/Coursera/Google Data Analytics/capstone/data set/StockCode_Avg_Revenue.csv", row.names = FALSE)


#Ensure TotalPrice and InvoiceMonth are present
retail_data <- retail_data %>%
  mutate(
    TotalPrice = Quantity * UnitPrice,
    InvoiceMonth = format(as.Date(InvoiceDate), "%Y-%m")  # You can also use "%B %Y" if you prefer "December 2010" format
  )

#Group by InvoiceMonth and Country, then summarise
monthly_country_revenue <- retail_data %>%
  group_by(InvoiceMonth, Country) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

#Preview result
head(monthly_country_revenue)


#Define output file path
file_name <- paste0("monthly_country_revenue_", Sys.Date(), ".csv")
output_path <- file.path(output_dir, file_name)

#export
write.csv(monthly_country_revenue, output_path, row.names = FALSE)
browseURL(output_path)  # Automatically open on Windows

#Filter for only the months of interest
target_months <- c("2010-12", "2011-11")

#Filter and group
selected_months_revenue <- retail_data %>%
  filter(InvoiceMonth %in% target_months) %>%
  group_by(Country, InvoiceMonth) %>%
  summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

#Reshape the data: one row per Country, columns for each month
revenue_pivot <- selected_months_revenue %>%
  pivot_wider(
    names_from = InvoiceMonth,
    values_from = TotalRevenue,
    names_prefix = "",
    names_sep = "_"
  ) %>%
  rename(
    December_2010_Revenue = `2010-12`,
    November_2011_Revenue = `2011-11`
  )

#Export to CSV
file_name <- paste0("country_revenue_dec2010_nov2011_", Sys.Date(), ".csv")
output_path <- file.path(output_dir, file_name)

write.csv(revenue_pivot, output_path, row.names = FALSE)
browseURL(output_path)  # Automatically open file

#clustering

# --- K-Means Clustering Analysis ---

# Prepare data for clustering: Using InvoiceTotal
clustering_data <- invoice_totals %>%
  select(InvoiceTotal)

# Perform K-Means clustering with a chosen number of clusters (e.g., 3)
# can experiment wiith different numbers of centers
kmeans_result <- kmeans(clustering_data, centers = 3)

# Add the cluster assignment to the invoice_totals data frame
invoice_totals$Cluster <- as.factor(kmeans_result$cluster)

# Visualization of Clustering Results with Improved X-Axis Labels ---

#Rotate the X-Axis Labels
p5_rotated <- ggplot(invoice_totals, aes(x = InvoiceMonth, y = InvoiceTotal, color = Cluster)) +
  geom_jitter(width = 0.3, alpha = 0.6) +
  labs(title = "K-Means Clustering of Invoice Totals (Rotated Labels)", x = "Month", y = "Invoice Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  my_white_theme
ggsave(file.path(graphics_folder, "ClusteringPlot_rotated_labels.png"), p5_rotated, width = 10, height = 6)

#Abbreviate Month Names for X-Axis Labels
invoice_totals_abbr <- invoice_totals %>%
  mutate(InvoiceMonthAbbr = format(as.Date(paste0("01 ", InvoiceMonth), format = "%d %B %Y"), "%b %Y"))
p5_abbreviated <- ggplot(invoice_totals_abbr, aes(x = InvoiceMonthAbbr, y = InvoiceTotal, color = Cluster)) +
  geom_jitter(width = 0.3, alpha = 0.6) +
  labs(title = "K-Means Clustering of Invoice Totals (Abbreviated Months)", x = "Month", y = "Invoice Total") +
  my_white_theme
ggsave(file.path(graphics_folder, "ClusteringPlot_abbreviated_labels.png"), p5_abbreviated, width = 10, height = 6)

#Increase Plot Width (can be combined with rotation or abbreviation)
p5_rotated_wider <- ggplot(invoice_totals, aes(x = InvoiceMonth, y = InvoiceTotal, color = Cluster)) +
  geom_jitter(width = 0.3, alpha = 0.6) +
  labs(title = "K-Means Clustering of Invoice Totals (Rotated Labels, Wider)", x = "Month", y = "Invoice Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  my_white_theme
ggsave(file.path(graphics_folder, "ClusteringPlot_rotated_wider.png"), p5_rotated_wider, width = 12, height = 6)



