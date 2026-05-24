#Clear workspace
rm(list = ls())

# -----------------------------
# Install packages (FIXED)
# -----------------------------
pkgs <- c(
  "readxl","dplyr","caret","ggplot2",
  "rpart","rpart.plot","pROC","ISLR",
  "randomForest","class","e1071",
  "tidyr","scales"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)) install.packages(to_install)

# -----------------------------
# Load libraries (FIXED)
# -----------------------------
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(pROC)
library(ISLR)
library(randomForest)
library(class)
library(e1071)
library(tidyr)
library(scales)

# -----------------------------
# Load Excel file
# -----------------------------
data <- read_excel("C:\\Users\\micah\\OneDrive\\Classes\\NMU\\Spring 2026\\BUS 598 Directed Study Business Analytics Project\\dataset\\data with eliminated variables for working with.xlsx")

# Convert negative session_length_in_minutes to NA (MISSING)
data <- data %>%
  mutate(session_length_in_minutes = ifelse(session_length_in_minutes < 0, NA, session_length_in_minutes))

# View first few rows
head(data)

# Check for missing data
anyNA(data)
colSums(is.na(data))

# -----------------------------
# fraud_bool cleaning (keep your logic)
# -----------------------------
str(data$fraud_bool)
unique(data$fraud_bool)
table(data$fraud_bool, useNA = "ifany")

fraud_raw <- trimws(as.character(data$fraud_bool))
table(fraud_raw, useNA = "ifany")

fraud_lower <- tolower(fraud_raw)
data$fraud_bool <- dplyr::case_when(
  fraud_lower %in% c("1", "true", "t", "yes", "y") ~ 1L,
  fraud_lower %in% c("0", "false", "f", "no", "n") ~ 0L,
  TRUE ~ NA_integer_
)

table(data$fraud_bool, useNA = "ifany")
stopifnot(!anyNA(data$fraud_bool))

# Ensure fraud_bool is treated as "0"/"1" for plot
df_plot <- transform(
  data,
  fraud_label = factor(
    trimws(as.character(fraud_bool)),
    levels = c("0", "1"),
    labels = c("Non-Fraudulent", "Fraudulent")
  )
)

ggplot(df_plot, aes(x = fraud_label)) +
  geom_bar(width = 0.6) +
  scale_y_continuous(
    limits = c(0, 40000),
    breaks = seq(0, 40000, by = 10000)
  ) +
  labs(x = NULL, y = "Count") +
  theme_minimal()

# -----------------------------
# customer_age chart
# -----------------------------
df_plot <- transform(
  data,
  customer_age_decade = factor(
    as.character(customer_age),
    levels = as.character(seq(10, 90, by = 10))
  )
)

ggplot(df_plot, aes(x = customer_age_decade)) +
  geom_bar(width = 0.6) +
  scale_y_continuous(
    limits = c(0, 12000),
    breaks = seq(0, 12000, by = 2000)
  ) +
  labs(
    title = "Customer Age (Grouped by Decades)",
    x = "Customer Age",
    y = "Count"
  ) +
  theme_minimal()

# customer_age summary
age_levels <- as.character(seq(10, 90, by = 10))
age_fac <- factor(as.character(data$customer_age), levels = age_levels)

age_counts <- table(age_fac, useNA = "ifany")
age_counts

age_props <- prop.table(age_counts)
round(100 * age_props, 1)

age_cumprops <- cumsum(age_props)
round(100 * age_cumprops, 1)

# Boxplot (log scale) for days_since_request
boxplot(log1p(data$days_since_request),
        horizontal = TRUE,
        xlab = "log1p(days_since_request)",
        main = "Days Since Request (horizontal boxplot of log1p values)")

# -----------------------------
# Use dynamic n_total everywhere (FIXED)
# -----------------------------
n_total <- nrow(data)

# payment_type bar chart
dfp <- as.data.frame(table(data$payment_type), stringsAsFactors = FALSE)
names(dfp) <- c("payment_type", "count")
dfp$pct <- 100 * dfp$count / n_total
dfp$payment_type <- factor(dfp$payment_type, levels = c("AA", "AB", "AC", "AD", "AE"))

ggplot(dfp, aes(x = payment_type, y = pct)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(dfp$pct, na.rm = TRUE) * 1.12)
  ) +
  labs(x = "payment_type", y = "Percent of records") +
  theme_minimal()

# employment_status bar chart
dfe <- as.data.frame(table(data$employment_status), stringsAsFactors = FALSE)
names(dfe) <- c("employment_status", "count")
dfe$pct <- 100 * dfe$count / n_total

dfe$employment_status <- factor(
  dfe$employment_status,
  levels = c("CA", "CB", "CC", "CD", "CE", "CF", "CG")
)

p <- ggplot(dfe, aes(x = employment_status, y = count)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(
    limits = c(0, max(dfe$count, na.rm = TRUE) * 1.10),
    breaks = pretty(c(0, max(dfe$count, na.rm = TRUE) * 1.10), n = 8)
  ) +
  labs(x = "employment_status", y = "Count") +
  theme_minimal()
p

# housing_status bar chart
dfh <- as.data.frame(table(data$housing_status), stringsAsFactors = FALSE)
names(dfh) <- c("housing_status", "count")
dfh$pct <- 100 * dfh$count / n_total

p <- ggplot(dfh, aes(x = housing_status, y = count)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "housing_status", y = "Count") +
  theme_minimal()
p

# foreign_request bar chart
dff <- as.data.frame(table(data$foreign_request), stringsAsFactors = FALSE)
names(dff) <- c("foreign_request", "count")
dff$pct <- 100 * dff$count / n_total

dff$foreign_request <- factor(
  dff$foreign_request,
  levels = c("0", "1"),
  labels = c("Domestic Request", "Foreign Request")
)

p <- ggplot(dff, aes(x = foreign_request, y = count)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Count") +
  theme_minimal()
p

# source bar chart
dfs <- as.data.frame(table(data$source), stringsAsFactors = FALSE)
names(dfs) <- c("source", "count")
dfs$pct <- 100 * dfs$count / n_total

dfs$source <- factor(
  dfs$source,
  levels = c("INTERNET", "TELEAPP"),
  labels = c("Internet", "Telephone")
)

p <- ggplot(dfs, aes(x = source, y = count)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Source", y = "Count") +
  theme_minimal()
p

# Check negative lengths (should be none after conversion)
x <- data$session_length_in_minutes
sum(x < 0, na.rm = TRUE)
summary(x)

# Descriptive statistics for session_length_in_minutes (already cleaned)
x <- data$session_length_in_minutes
n <- sum(!is.na(x))
mean_x <- mean(x, na.rm = TRUE)
sd_x <- sd(x, na.rm = TRUE)
var_x <- var(x, na.rm = TRUE)
se_x <- sd_x / sqrt(n)
median_x <- median(x, na.rm = TRUE)

ux <- unique(x[!is.na(x)])
mode_x <- ux[which.max(tabulate(match(x[!is.na(x)], ux)))]

skew_x <- e1071::skewness(x, na.rm = TRUE, type = 2)
kurt_x <- e1071::kurtosis(x, na.rm = TRUE, type = 2)

min_x <- min(x, na.rm = TRUE)
max_x <- max(x, na.rm = TRUE)
range_x <- max_x - min_x

DescriptiveStatisticsSessionLengthInMinutes <- data.frame(
  Mean = mean_x,
  Standard_Error = se_x,
  Median = median_x,
  Mode = mode_x,
  Standard_Deviation = sd_x,
  Sample_Variance = var_x,
  Kurtosis = kurt_x,
  Skewness = skew_x,
  Range = range_x,
  Minimum = min_x,
  Maximum = max_x,
  Count = n
)

print(DescriptiveStatisticsSessionLengthInMinutes)
cat("\nRounded:\n")
print(round(DescriptiveStatisticsSessionLengthInMinutes, 2))

# month bar chart
dfm <- as.data.frame(table(data$month), stringsAsFactors = FALSE)
names(dfm) <- c("month", "count")
dfm$pct <- 100 * dfm$count / n_total

dfm$month <- as.integer(dfm$month)
dfm$month <- factor(dfm$month, levels = 0:7, labels = paste0("M", 1:8))

p <- ggplot(dfm, aes(x = month, y = count)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Month", y = "Count") +
  theme_minimal()
p

# -----------------------------
# Convert fraud_bool to 2-level factor (keep your mapping)
# -----------------------------
data$fraud_bool <- as.integer(as.character(data$fraud_bool))
table(data$fraud_bool, useNA = "ifany")

data$fraud_bool <- factor(
  data$fraud_bool,
  levels = c(1, 0),
  labels = c("potential_fraud", "good")
)

levels(data$fraud_bool)
table(data$fraud_bool, useNA = "ifany")

# -----------------------------
# Train/Val/Test split
# -----------------------------
set.seed(123)

train_index <- createDataPartition(data$fraud_bool, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
remaining_data <- data[-train_index, ]

val_index <- createDataPartition(remaining_data$fraud_bool, p = 0.5, list = FALSE)
val_data <- remaining_data[val_index, ]
test_data <- remaining_data[-val_index, ]

# Impute missing session_length_in_minutes using training median
med_sess <- median(train_data$session_length_in_minutes, na.rm = TRUE)
train_data$session_length_in_minutes[is.na(train_data$session_length_in_minutes)] <- med_sess
val_data$session_length_in_minutes[is.na(val_data$session_length_in_minutes)]     <- med_sess
test_data$session_length_in_minutes[is.na(test_data$session_length_in_minutes)]   <- med_sess

cat("Training Data Distribution:\n"); print(table(train_data$fraud_bool))
cat("\nValidation Data Distribution:\n"); print(table(val_data$fraud_bool))
cat("\nTesting Data Distribution:\n"); print(table(test_data$fraud_bool))

# Combined dataset for visualization
combined_data <- bind_rows(
  train_data %>% mutate(set = "Training"),
  val_data   %>% mutate(set = "Validation"),
  test_data  %>% mutate(set = "Testing")
)

ggplot(combined_data, aes(x = set, fill = fraud_bool)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Dataset split", fill = "Class") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()

# Pie charts
create_pie_chart <- function(df, dataset_name) {
  pie_data <- as.data.frame(table(df$fraud_bool)) %>%
    rename(Class = Var1, Count = Freq) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  ggplot(pie_data, aes(x = "", y = Percentage, fill = Class)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    labs(title = paste(dataset_name, "Data Distribution"), fill = "Class") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    theme_void()
}

print(create_pie_chart(train_data, "Training"))
print(create_pie_chart(val_data, "Validation"))
print(create_pie_chart(test_data, "Testing"))

# ============================================================
# DECISION TREE
# ============================================================
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(123)
fit_rpart <- train(
  fraud_bool ~ .,
  data = train_data,
  method = "rpart",
  metric = "ROC",
  trControl = ctrl,
  tuneLength = 20
)

fit_rpart
plot(fit_rpart)

# Readable decision tree graphic (pruned)
cp_plot <- 0.01
tree_pruned <- prune(fit_rpart$finalModel, cp = cp_plot)

# NOTE: predict(tree_pruned, type="prob") may behave differently by object;
# keeping your code as-is, but if this errors, tell me and I'll adjust.
node_prob  <- predict(tree_pruned, type = "prob")[, "potential_fraud"]
pred_class <- levels(train_data$fraud_bool)[tree_pruned$frame$yval]
pct_label  <- paste0(round(100 * node_prob, 0), "%")
node_labels <- paste0(pred_class, "\n", pct_label)
label_map <- setNames(node_labels, rownames(tree_pruned$frame))

node_fun_pct <- function(x, labs, digits, varlen) {
  unname(label_map[rownames(x$frame)])
}

rpart.plot(
  tree_pruned,
  type = 2,
  extra = 0,
  under = TRUE,
  fallen.leaves = TRUE,
  box.palette = "Blues",
  branch.lty = 1,
  main = paste0("Decision Tree (pruned, cp = ", cp_plot, "): Fraud Probability"),
  nn = TRUE,
  yesno = 2,
  faclen = 0,
  varlen = 0,
  tweak = 1.1,
  node.fun = node_fun_pct
)

mtext(
  "Note: % in each node is estimated P(Potential Fraud) for observations reaching that node.",
  side = 1, line = 4, cex = 0.85
)

# ---- Evaluate on validation set (Tree) ----
val_prob_tree <- predict(fit_rpart, newdata = val_data, type = "prob")[, "potential_fraud"]
val_roc_tree <- pROC::roc(
  response  = val_data$fraud_bool,
  predictor = val_prob_tree,
  levels    = c("good", "potential_fraud")
)
val_auc_tree <- as.numeric(pROC::auc(val_roc_tree))
cat("\nDecision Tree validation AUC:", round(val_auc_tree, 4), "\n")

# ---- OPTION 2: Choose threshold using Youden on validation ROC (FIXED) ----
# coords() returns a data.frame (or matrix) when transpose=FALSE, so use [[ ]] to extract scalars.
best_tree <- pROC::coords(
  val_roc_tree,
  x = "best",
  best.method = "youden",
  transpose = FALSE
)

thr_tree <- as.numeric(best_tree[["threshold"]])
cat("Decision Tree threshold selected from validation (Youden):", thr_tree, "\n")

# Validation confusion matrix at chosen threshold (consistent with selection)
val_pred_tree <- factor(ifelse(val_prob_tree >= thr_tree, "potential_fraud", "good"),
                        levels = c("potential_fraud", "good"))
cm_tree_val <- confusionMatrix(val_pred_tree, val_data$fraud_bool, positive = "potential_fraud")
print(cm_tree_val)

# ============================================================
# RANDOM FOREST
# ============================================================
ntree_grid <- c(500, 600, 700, 800, 900, 1000)

rf_models  <- vector("list", length(ntree_grid))
names(rf_models) <- paste0("ntree_", ntree_grid)

rf_results <- data.frame()

rf_formula <- fraud_bool ~ income + customer_age + days_since_request + intended_balcon_amount +
  payment_type + velocity_6h + velocity_24h + velocity_4w + employment_status +
  credit_risk_score + housing_status + proposed_credit_limit + foreign_request +
  source + session_length_in_minutes + month

for (i in seq_along(ntree_grid)) {
  nt <- ntree_grid[i]
  cat("\n==============================\n")
  cat("Fitting random forest with ntree =", nt, "\n")
  cat("==============================\n")
  
  rf_models[[i]] <- randomForest(
    rf_formula,
    data = train_data,
    ntree = nt,
    importance = TRUE
  )
  
  val_prob_rf <- predict(rf_models[[i]], newdata = val_data, type = "prob")[, "potential_fraud"]
  val_roc_rf  <- pROC::roc(val_data$fraud_bool, val_prob_rf, levels = c("good", "potential_fraud"))
  val_auc_rf  <- as.numeric(pROC::auc(val_roc_rf))
  
  val_pred_rf <- factor(ifelse(val_prob_rf >= 0.5, "potential_fraud", "good"),
                        levels = c("potential_fraud", "good"))
  cm_rf <- confusionMatrix(val_pred_rf, val_data$fraud_bool, positive = "potential_fraud")
  
  row <- data.frame(
    ntree = nt,
    AUC = val_auc_rf,
    Accuracy = unname(cm_rf$overall["Accuracy"]),
    Sensitivity = unname(cm_rf$byClass["Sensitivity"]),
    Specificity = unname(cm_rf$byClass["Specificity"])
  )
  
  rf_results <- rbind(rf_results, row)
  
  cat("\nValidation metrics:\n")
  print(row)
}

cat("\nAll validation results:\n")
print(rf_results)

# RF metrics plot (long format)
rf_results$ntree <- as.numeric(rf_results$ntree)

rf_long <- rf_results %>%
  tidyr::pivot_longer(
    cols = c("AUC", "Accuracy", "Sensitivity", "Specificity"),
    names_to = "Metric",
    values_to = "Value"
  )

rf_long$Metric <- factor(rf_long$Metric, levels = c("AUC","Accuracy","Sensitivity","Specificity"))

p_rf_metrics <- ggplot(rf_long, aes(x = ntree, y = Value)) +
  geom_line(linewidth = 0.8, color = "#1F4E79") +
  geom_point(size = 2, color = "#1F4E79") +
  facet_wrap(~ Metric, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = rf_results$ntree) +
  labs(
    title = "Random Forest Performance on Validation Set vs Number of Trees",
    x = "Number of trees (ntree)",
    y = "Metric value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p_rf_metrics)

# Best RF by validation AUC
best_rf_idx <- which.max(rf_results$AUC)
best_rf_name <- paste0("ntree_", rf_results$ntree[best_rf_idx])
best_rf <- rf_models[[best_rf_name]]
cat("\nBest RF by validation AUC:", best_rf_name, "\n")

# ============================================================
# K-NEAREST NEIGHBORS (KNN)
# ============================================================
train_knn <- train_data
val_knn   <- val_data
test_knn  <- test_data

char_cols <- names(train_knn)[sapply(train_knn, is.character)]
if (length(char_cols) > 0) {
  train_knn[char_cols] <- lapply(train_knn[char_cols], as.factor)
  val_knn[char_cols]   <- lapply(val_knn[char_cols], as.factor)
  test_knn[char_cols]  <- lapply(test_knn[char_cols], as.factor)
}

dummies <- dummyVars(fraud_bool ~ ., data = train_knn, fullRank = TRUE)

train.X <- predict(dummies, newdata = train_knn)
val.X   <- predict(dummies, newdata = val_knn)
test.X  <- predict(dummies, newdata = test_knn)

pp <- preProcess(train.X, method = c("center", "scale"))
train.X <- predict(pp, train.X)
val.X   <- predict(pp, val.X)
test.X  <- predict(pp, test.X)

train.Y <- train_knn$fraud_bool
val.Y   <- val_knn$fraud_bool
test.Y  <- test_knn$fraud_bool

# k grid search on validation
k_grid <- seq(1, 51, by = 2)
knn_results <- data.frame(k = k_grid, Accuracy = NA_real_, Error = NA_real_, AUC = NA_real_)

for (i in seq_along(k_grid)) {
  k <- k_grid[i]
  
  pred <- class::knn(train = train.X, test = val.X, cl = train.Y, k = k, prob = TRUE)
  acc <- mean(pred == val.Y)
  err <- 1 - acc
  
  pwin <- attr(pred, "prob")
  p_potential <- ifelse(pred == "potential_fraud", pwin, 1 - pwin)
  
  roc_obj <- pROC::roc(val.Y, p_potential, levels = c("good", "potential_fraud"))
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  knn_results$Accuracy[i] <- acc
  knn_results$Error[i] <- err
  knn_results$AUC[i] <- auc_val
}

print(knn_results)

best_k_auc <- knn_results$k[which.max(knn_results$AUC)]
best_k_acc <- knn_results$k[which.max(knn_results$Accuracy)]
cat("\nBest k by validation AUC:", best_k_auc, "\n")
cat("Best k by validation Accuracy:", best_k_acc, "\n")

best_k <- best_k_auc

# ============================================================
# FINAL TEST SET COMPARISON (Tree vs RF vs KNN)
# ============================================================
eval_binary_model <- function(truth, prob_pf, pred_label, model_name) {
  truth <- factor(truth, levels = c("potential_fraud","good"))
  pred_label <- factor(pred_label, levels = c("potential_fraud","good"))
  
  cm <- caret::confusionMatrix(pred_label, truth, positive = "potential_fraud")
  
  roc_obj <- pROC::roc(
    response  = truth,
    predictor = prob_pf,
    levels    = c("good","potential_fraud")
  )
  auc <- as.numeric(pROC::auc(roc_obj))
  
  TP <- unname(cm$table["potential_fraud","potential_fraud"])
  FP <- unname(cm$table["potential_fraud","good"])
  FN <- unname(cm$table["good","potential_fraud"])
  TN <- unname(cm$table["good","good"])
  
  data.frame(
    Model = model_name,
    AUC = auc,
    Accuracy = unname(cm$overall["Accuracy"]),
    Sensitivity = unname(cm$byClass["Sensitivity"]),
    Specificity = unname(cm$byClass["Specificity"]),
    Precision = unname(cm$byClass["Pos Pred Value"]),
    F1 = unname(cm$byClass["F1"]),
    TP = TP, FP = FP, FN = FN, TN = TN,
    stringsAsFactors = FALSE
  )
}

# 1) Tree on TEST (use thr_tree from validation Youden)
test_prob_tree <- predict(fit_rpart, newdata = test_data, type = "prob")[, "potential_fraud"]
test_pred_tree <- ifelse(test_prob_tree >= thr_tree, "potential_fraud", "good")

res_tree <- eval_binary_model(
  truth = test_data$fraud_bool,
  prob_pf = test_prob_tree,
  pred_label = test_pred_tree,
  model_name = paste0("Decision Tree (thr=", round(thr_tree, 3), ")")
)

# 2) RF on TEST (threshold 0.5)
test_prob_rf <- predict(best_rf, newdata = test_data, type = "prob")[, "potential_fraud"]
test_pred_rf <- ifelse(test_prob_rf >= 0.5, "potential_fraud", "good")

res_rf <- eval_binary_model(
  truth = test_data$fraud_bool,
  prob_pf = test_prob_rf,
  pred_label = test_pred_rf,
  model_name = paste0("Random Forest (", best_rf_name, ", thr=0.5)")
)

# 3) KNN on TEST (use best_k from validation AUC)
knn_pred_test <- class::knn(train = train.X, test = test.X, cl = train.Y, k = best_k, prob = TRUE)
pwin_test <- attr(knn_pred_test, "prob")
test_prob_knn <- ifelse(knn_pred_test == "potential_fraud", pwin_test, 1 - pwin_test)

res_knn <- eval_binary_model(
  truth = test_data$fraud_bool,
  prob_pf = test_prob_knn,
  pred_label = knn_pred_test,
  model_name = paste0("KNN (k=", best_k, ")")
)

# Combined summary table
summary_test <- dplyr::bind_rows(res_tree, res_rf, res_knn) %>%
  dplyr::arrange(dplyr::desc(AUC))

summary_test_print <- summary_test %>%
  dplyr::mutate(
    dplyr::across(c(AUC, Accuracy, Sensitivity, Specificity, Precision, F1), ~ round(.x, 3))
  )

cat("\n================ FINAL TEST SET SUMMARY (Tree vs RF vs KNN) ================\n")
print(summary_test_print, row.names = FALSE)

# Optional: save
# write.csv(summary_test_print, "model_summary_test_set.csv", row.names = FALSE)