# =====================================================
# STATISTICAL MODEL TRAINING - Churn Prediction Model
# =====================================================
# This script trains a logistic regression model to predict customer churn
# Input: churn_data (dataframe with customer data)
# Output: final_churn_model.rds (saved model file)

library(ggplot2)
library(dplyr)
library(scales)
library(MASS)
library(pROC)
library(caret)
library(ResourceSelection)

## --------------------------------------------------------------
##  CLEAN + FILTER: 
##  - Only "Laptop & Accessory", "Mobile", "Mobile Phone"
##  - Remove rows with ANY missing value
## --------------------------------------------------------------

# Start with your data
df <- churn_data   # or whatever your object is called
df <- df %>% dplyr::select(-CustomerID)  

# 1. Replace blank strings with NA
df <- df %>%
  mutate(across(everything(),
                ~ ifelse(trimws(.) == "" | . == "NA", NA, .)))

# 2. Filter to only 3 categories
df_filtered <- df %>%
  filter(PreferedOrderCat %in% c("Laptop & Accessory", "Mobile", "Mobile Phone"))

# 3. Remove ANY row with ANY NA → using base R na.omit()
new_churn <- na.omit(df_filtered)

# 4. Check results
cat("Original rows:", nrow(df), "\n")
cat("After category filter:", nrow(df_filtered), "\n")
cat("After na.omit():", nrow(new_churn), "\n")

# Churn rate 
churn_rate <- mean(new_churn$Churn) * 100
cat("Overall churn rate:", churn_rate, "%\n")

## --------------------------------------------------------------
##  VISUALIZATIONS
## --------------------------------------------------------------

# Visualization 1: Churn by Category
churn_by_cat <- new_churn %>%
  count(PreferedOrderCat, Churn) %>%
  group_by(PreferedOrderCat) %>%
  mutate(pct = n / sum(n) * 100,
         label = ifelse(Churn == 0, "Retained", "Churn"),
         pct_label = percent(pct / 100, accuracy = 0.1)) %>%
  ungroup()

ggplot(churn_by_cat, aes(x = PreferedOrderCat, y = pct, fill = label)) +
  geom_col(position = "fill", colour = "white", width = 0.7) +
  geom_text(aes(label = pct_label),
            position = position_fill(vjust = 0.5),
            size = 4.5, color = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Retained" = "#2ca02c", "Churn" = "#d62728")) +
  labs(
    title = "Churn Rate by Preferred Product Category",
    x = "Preferred Order Category",
    y = "Proportion of Customers",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# Visualization 2: Churn by Gender
churn_by_gender <- new_churn %>%
  count(Gender, Churn, .drop = FALSE) %>%
  group_by(Gender) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    label = ifelse(Churn == 0, "Retained", "Churn"),
    pct_label = percent(pct / 100, accuracy = 0.1)
  )

ggplot(churn_by_gender, aes(x = Gender, y = pct, fill = label)) +
  geom_col(position = "fill", colour = "white", width = 0.6) +
  geom_text(aes(label = pct_label),
            position = position_fill(vjust = 0.5),
            size = 5, color = "white", fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Retained" = "#2ca02c", "Churn" = "#d62728")) +
  labs(
    title = "Churn Rate by Gender",
    x = "Gender",
    y = "Proportion of Customers",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.x = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

## --------------------------------------------------------------
##  BUILDING THE STATISTICAL MODEL
## --------------------------------------------------------------

# STEP 1: Prepare data for modeling
# Remove first column and keep Churn at the end
new_churn1 <- new_churn[,-c(1)]

# Add Churn back as the last column
new_churn1 <- cbind(new_churn1, new_churn$Churn)

# Rename the new Churn column to "Churn"
names(new_churn1)[ncol(new_churn1)] <- "Churn"

# Ensure factor levels are set correctly BEFORE modeling
if("PreferedOrderCat" %in% names(new_churn1)) {
  new_churn1$PreferedOrderCat <- factor(new_churn1$PreferedOrderCat,
                                        levels = c("Laptop & Accessory", "Mobile", "Mobile Phone",
                                                   "Grocery", "Fashion", "Others"))
}
if("PreferredLoginDevice" %in% names(new_churn1)) {
  new_churn1$PreferredLoginDevice <- factor(new_churn1$PreferredLoginDevice,
                                            levels = c("Computer", "Mobile Phone", "Phone"))
}
if("MaritalStatus" %in% names(new_churn1)) {
  new_churn1$MaritalStatus <- factor(new_churn1$MaritalStatus,
                                     levels = c("Single", "Married", "Divorced"))
}
if("Gender" %in% names(new_churn1)) {
  new_churn1$Gender <- factor(new_churn1$Gender, levels = c("Male", "Female"))
}

# STEP 2: Build models
# Full logistic model with ALL features
full_model <- glm(Churn ~ ., data = new_churn1, family = binomial)
summary(full_model)

# Base (null) model — only intercept
base_model <- glm(Churn ~ 1, data = new_churn1, family = binomial)
summary(base_model)

# Stepwise selection (start from full, remove weak features)
stepwmodel <- stepAIC(full_model, direction = "both", scope = full_model, trace = FALSE)
summary(stepwmodel)

# Compare model quality using AIC
cat("\nModel Comparison (AIC - lower is better):\n")
cat("Full model AIC:", AIC(full_model), "\n")
cat("Base model AIC:", AIC(base_model), "\n")
cat("Stepwise model AIC:", AIC(stepwmodel), "\n")

# Variables that affected churn
cat("\nSignificant variables in final model:\n")
print(summary(stepwmodel)$coefficients)

## --------------------------------------------------------------
##  MODEL VALIDATION & TESTING
## --------------------------------------------------------------

# STEP 3: Split data into train/test sets
set.seed(123)
idx   <- sample(nrow(new_churn1), 0.75*nrow(new_churn1))
train <- new_churn1[idx, ]
test  <- new_churn1[-idx, ]

# Refit final model on train only
final_train <- glm(formula(stepwmodel), data = train, family = binomial)

# Predict on test
test$prob <- predict(final_train, newdata = test, type = "response")
test$pred <- ifelse(test$prob > 0.5, 1, 0)

# Model Performance Metrics
# 1. AUC - Area Under ROC Curve
roc_obj <- roc(test$Churn, test$prob)
cat("\nTest AUC =", round(auc(roc_obj), 3), "(Aim for > 0.80)\n")

# 2. Confusion Matrix
cat("\nConfusion Matrix:\n")
print(confusionMatrix(factor(test$pred), factor(test$Churn), positive = "1"))

# 3. Calibration test
calib_pvalue <- hoslem.test(test$Churn, test$prob)$p.value
cat("\nHosmer-Lemeshow test p-value:", round(calib_pvalue, 4), 
    ifelse(calib_pvalue > 0.05, "(Good calibration)", "(Poor calibration)"), "\n")

# 4. Optimal threshold
coords <- coords(roc_obj, "best", ret = c("threshold", "sens", "spec"))
best_thresh <- coords$threshold
cat("\nOptimal threshold =", round(best_thresh, 3), "\n")

test$pred_opt <- ifelse(test$prob > best_thresh, 1, 0)
cat("\nConfusion Matrix with Optimal Threshold:\n")
print(confusionMatrix(factor(test$pred_opt), factor(test$Churn), positive = "1"))

# 5. Risk grouping
test$risk <- cut(test$prob, 
                 breaks = c(0, 0.1, 0.3, 1), 
                 labels = c("Low (<10%)", "Medium (10-30%)", "High (>30%)"))

cat("\nRisk Distribution:\n")
print(table(test$risk, test$Churn))

## --------------------------------------------------------------
##  SAVE MODEL FOR APP USE
## --------------------------------------------------------------

# Save the trained model
saveRDS(final_train, "final_churn_model.rds")
cat("\n✓ Model saved as: final_churn_model.rds\n")

# Save cleaned training data for demo
new_churn_clean <- new_churn1
saveRDS(new_churn_clean, "demo_data.rds")
cat("✓ Demo data saved as: demo_data.rds\n")

cat("\n========================================\n")
cat("Model training complete!\n")
cat("You can now run the app with: churn_app.R\n")
cat("========================================\n")

