# Authorized Push Payment Fraud and Data Analytics

**Question:** Can decision trees, random forests and KNN identify likely authorized push payment (APP) fraud in a transaction dataset?

**Answer, with caveats:** The random forest performed best, with 80.9% test accuracy (see the paper). Accuracy alone is not interpretable without the fraud base rate: the script writes the test-set base rate (`base_rate_test`) and AUC, sensitivity, specificity, precision and F1 for all three models to `outputs/model_summary_test_set.csv`. Run the script and report those numbers alongside accuracy.

Limitations:
- Random train/validation/test split, with features measured at the same time as the label. This does not support the claim of catching scams "before they happen"; that would need a time-based split (the data has a `month` column) and a leakage audit. Neither was done.
- The models are not like-for-like: the tree uses all columns, the random forest an explicit 16-predictor formula, KNN dummy-encoded columns, and classification thresholds differ (tree: Youden on validation; random forest: 0.5; KNN: majority vote).
- Random forest tuning covers `ntree` only (no `mtry`).

## Data

`data with eliminated variables for working with.xlsx`: 40,000 rows, 17 columns, sheet `Base potamus experiment`. Described in the paper as synthetic transactions. The column set resembles the public Bank Account Fraud (BAF) dataset, but the exact source, URL, license and retrieval date are **not recorded** and should be confirmed. Which variables were eliminated, and why, is also not documented.

| Column | Notes |
|---|---|
| `fraud_bool` | Label (1 = potential fraud) |
| `income`, `customer_age`, `employment_status`, `housing_status`, `credit_risk_score`, `proposed_credit_limit` | Customer-level |
| `days_since_request`, `intended_balcon_amount`, `payment_type`, `source`, `foreign_request`, `session_length_in_minutes`, `month` | Request/session-level (negative session lengths are set to missing and median-imputed) |
| `velocity_6h`, `velocity_24h`, `velocity_4w` | Request-velocity measures |

Meanings are inferred from names; no codebook is available.

## Run

```r
# from this folder
install.packages(c("readxl","dplyr","caret","ggplot2","rpart","rpart.plot",
  "pROC","ISLR","randomForest","class","e1071","tidyr","scales"))
source("r_script_for_final_paper.R")
```

Outputs: `outputs/model_summary_test_set.csv`, `outputs/rf_variable_importance.csv`. The paper (`bus_598_final_draft_micah_holmquist_pdf.pdf`) and slides are included.
