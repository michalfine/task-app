# Quick Setup Script - Creates a minimal test model from test CSV
# This allows the app to run even without full training data

library(dplyr)
library(MASS)

cat("Setting up test model...\n")

# Read the test CSV
test_file <- "test_customer_template.csv"
if(!file.exists(test_file)) {
  stop("test_customer_template.csv not found!")
}

df <- read.csv(test_file, stringsAsFactors = FALSE)

# Create a minimal dataset by replicating test rows
# This creates enough data to train a simple model
set.seed(123)
n_replicates <- 100

# Replicate and add some variation
df_expanded <- df[rep(1:nrow(df), each = n_replicates), ]

# Add some random variation to numeric columns
numeric_cols <- c("Tenure", "WarehouseToHome", "NumberOfDeviceRegistered",
                 "SatisfactionScore", "NumberOfAddress", "Complain",
                 "DaySinceLastOrder", "HourSpendOnApp", "OrderAmountHikeFromlastYear",
                 "CouponUsed", "OrderCount", "CashbackAmount")

for(col in numeric_cols) {
  if(col %in% names(df_expanded)) {
    df_expanded[[col]] <- as.numeric(df_expanded[[col]]) + rnorm(nrow(df_expanded), 0, 0.1)
    df_expanded[[col]] <- pmax(df_expanded[[col]], 0)  # Ensure non-negative
  }
}

# Create a synthetic Churn column based on some features
# Higher churn probability for: low satisfaction, high days since last order, complaints
df_expanded$Churn <- rbinom(nrow(df_expanded), 1, 
                           prob = plogis(
                             -2 + 
                             0.1 * (5 - df_expanded$SatisfactionScore) +
                             0.01 * df_expanded$DaySinceLastOrder +
                             0.5 * df_expanded$Complain
                           ))

# Set factor levels
if("PreferedOrderCat" %in% names(df_expanded)) {
  df_expanded$PreferedOrderCat <- factor(df_expanded$PreferedOrderCat,
                                        levels = c("Laptop & Accessory", "Mobile", "Mobile Phone",
                                                   "Grocery", "Fashion", "Others"))
}
if("PreferredLoginDevice" %in% names(df_expanded)) {
  df_expanded$PreferredLoginDevice <- factor(df_expanded$PreferredLoginDevice,
                                            levels = c("Computer", "Mobile Phone", "Phone"))
}
if("MaritalStatus" %in% names(df_expanded)) {
  df_expanded$MaritalStatus <- factor(df_expanded$MaritalStatus,
                                     levels = c("Single", "Married", "Divorced"))
}
if("Gender" %in% names(df_expanded)) {
  df_expanded$Gender <- factor(df_expanded$Gender, levels = c("Male", "Female"))
}

# Remove CustomerID if it exists (not needed for model)
if("CustomerID" %in% names(df_expanded)) {
  df_expanded <- df_expanded %>% dplyr::select(-CustomerID)
}

# Remove other non-model columns
cols_to_remove <- c("CityTier", "PreferredPaymentMode")
for(col in cols_to_remove) {
  if(col %in% names(df_expanded)) {
    df_expanded <- df_expanded %>% dplyr::select(-all_of(col))
  }
}

# Train a simple logistic model
cat("Training model on", nrow(df_expanded), "synthetic observations...\n")

full_model <- glm(Churn ~ ., data = df_expanded, family = binomial)
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)

# Save the model
saveRDS(step_model, "final_churn_model.rds")
cat("✓ Model saved as final_churn_model.rds\n")

# Save demo data (without Churn column for prediction)
demo_data <- df_expanded %>% dplyr::select(-Churn)
saveRDS(demo_data, "demo_data.rds")
cat("✓ Demo data saved as demo_data.rds\n")

cat("\nSetup complete! You can now run the app with:\n")
cat("  Rscript run_churn_app.R\n")
cat("  or\n")
cat("  R -e 'source(\"run_churn_app.R\")'\n\n")

