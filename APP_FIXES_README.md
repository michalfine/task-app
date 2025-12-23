# Churn Prediction App - Fixes and Usage Guide

## Key Issues Fixed

### 1. **Factor Level Mismatch** (Critical Fix)
**Problem**: The `score_customers` function was creating `df_clean` but then modifying `df`, causing factor levels to not be applied correctly.

**Solution**: 
- Now properly sets factor levels BEFORE prediction
- Factor levels match exactly what was used in training:
  - `PreferedOrderCat`: "Laptop & Accessory", "Mobile", "Mobile Phone", "Grocery", "Fashion", "Others"
  - `PreferredLoginDevice`: "Computer", "Mobile Phone", "Phone"
  - `MaritalStatus`: "Single", "Married", "Divorced"
  - `Gender`: "Male", "Female"

### 2. **Demo Data Reference Error**
**Problem**: App referenced `new_churn_clean` which didn't exist.

**Solution**: 
- Now saves demo data as `demo_data.rds` after training
- App checks if demo data exists before loading

### 3. **Missing Error Handling**
**Problem**: App would crash silently when files had wrong format or missing columns.

**Solution**: 
- Added comprehensive try-catch blocks
- Clear error messages for users
- Validates required columns before processing
- Checks for empty dataframes after filtering

### 4. **Data Type Issues**
**Problem**: Numeric columns might be read as character strings.

**Solution**: 
- Explicitly converts numeric columns to numeric type
- Handles blank strings and "NA" values properly

### 5. **Category Filtering**
**Problem**: Uploaded data might include categories not in training data.

**Solution**: 
- Filters to only valid categories (same as training)
- Shows clear error if no valid rows found

## How to Use the Fixed App

### Step 1: Train the Model
Run the first part of the script (up to line ~270) to:
- Clean your data
- Train the model
- Save `final_churn_model.rds` and `demo_data.rds`

### Step 2: Run the App
The Shiny app will automatically:
- Load the saved model
- Load demo data (if available)
- Be ready to accept CSV uploads

### Step 3: Upload CSV File
Your CSV must contain these columns (case-sensitive):
- `Tenure` (numeric)
- `WarehouseToHome` (numeric)
- `NumberOfDeviceRegistered` (numeric)
- `PreferedOrderCat` (must be: "Laptop & Accessory", "Mobile", or "Mobile Phone")
- `PreferredLoginDevice` ("Computer", "Mobile Phone", or "Phone")
- `SatisfactionScore` (numeric, 1-5)
- `MaritalStatus` ("Single", "Married", or "Divorced")
- `NumberOfAddress` (numeric)
- `Complain` (0 or 1)
- `DaySinceLastOrder` (numeric)
- `HourSpendOnApp` (numeric)
- `OrderAmountHikeFromlastYear` (numeric)
- `CouponUsed` (numeric)
- `OrderCount` (numeric)
- `CashbackAmount` (numeric)
- `Gender` ("Male" or "Female")
- `CustomerID` (optional, for tracking)

### Step 4: View Results
- Dashboard shows summary statistics
- Risk distribution pie chart
- High-risk customer table
- Downloadable predictions CSV

## Testing the App

1. **Test with Demo Data**: Click "Load Demo Data" button
2. **Test with Upload**: Create a test CSV with one row matching the format above
3. **Check Error Messages**: Try uploading invalid data to see helpful error messages

## Important Notes

- **PreferedOrderCat** must be one of: "Laptop & Accessory", "Mobile", or "Mobile Phone"
- All numeric columns must be actual numbers (not text)
- Missing values will cause rows to be removed
- Factor levels must match exactly (case-sensitive)

## Troubleshooting

**App doesn't load**: 
- Make sure `final_churn_model.rds` exists in your working directory
- Check that all required packages are installed

**Upload fails**:
- Check column names match exactly (case-sensitive)
- Ensure PreferedOrderCat has valid values
- Check for missing values in required columns

**Predictions seem wrong**:
- Verify factor levels match training data
- Check that numeric columns are actually numeric
- Ensure data format matches training data format

