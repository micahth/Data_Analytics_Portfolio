# Retail Sales Data Presentation

**Question:** What do online-retail transactions show about revenue over time and by country, and what can't the data tell us?

**Findings:** See `google_data_analytics_capstone_powerpoint.pptx`. The script produces the underlying tables and charts; no headline numbers are asserted here until the script is re-run and checked.

## Data

`Online Retail.xlsx`: 541,909 line items, Dec 2010 onward (invoice-level transactions of a UK-based online retailer), columns `InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, TotalPrice, CustomerID, Country`. This matches the public UCI "Online Retail" dataset; the exact URL, license and retrieval date were not recorded, so confirm before reuse. `TotalPrice` was added in Excel (`Quantity * UnitPrice`); the script recomputes it. The file is ~24 MB and should move to Releases or Git LFS.

## Method notes

- Cancellations/returns (negative `Quantity`, or invoice numbers starting with `C`) and rows with `UnitPrice <= 0` are excluded from sales figures and summarised separately.
- "Average invoice total" is computed per invoice, not per line item.
- Clustering is exploratory: k-means (k = 3, seeded, `nstart = 25`) on log-scaled invoice totals only. Clusters are described in `ClusterSummary`; no further interpretation is claimed.

## Run

```r
# from this folder
install.packages(c("readxl","dplyr","ggplot2","writexl"))
source("R_file_for_capstone.R")
```

Outputs go to `outputs/` (git-ignored): `OnlineRetailSummary.xlsx`, stock-code CSVs, and PNG charts.
