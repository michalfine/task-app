# Churn Prediction App - Quick Start Guide

## ✅ Setup Complete!

The app has been set up and is ready to use. Here's what was created:

### Files Created:
1. **`churn_app_fixed.R`** - Complete fixed R script (training + app)
2. **`run_churn_app.R`** - Standalone app launcher
3. **`setup_test_model.R`** - Quick test model setup script
4. **`test_customer_template.csv`** - Test CSV file with 3 sample customers
5. **`launch_app.sh`** - Convenient launcher script
6. **`final_churn_model.rds`** - Trained model (created by setup)
7. **`demo_data.rds`** - Demo dataset (created by setup)

## 🚀 How to Run the App

### Option 1: Use the launcher script (Easiest)
```bash
./launch_app.sh
```

### Option 2: Run directly with R
```bash
Rscript run_churn_app.R
```

### Option 3: Run from R console
```r
source("run_churn_app.R")
```

## 📊 Testing the App

### Test 1: Load Demo Data
1. Open the app (it should open automatically in your browser)
2. Click the **"Load Demo Data"** button
3. You should see ~300 customers scored with churn predictions

### Test 2: Upload Test CSV
1. Click **"Upload Your Customer CSV"**
2. Select `test_customer_template.csv`
3. The app will process the 3 test customers and show predictions

## 📁 Test File Format

The `test_customer_template.csv` contains 3 sample customers:
- **TEST001**: Mobile Phone customer, 12 months tenure
- **TEST002**: Laptop & Accessory customer, 24 months tenure  
- **TEST003**: Mobile customer, 6 months tenure, has complaint

All required columns are included and properly formatted.

## 🔍 What the App Shows

1. **Summary Statistics**:
   - Total customers scored
   - High-risk customers count
   - Expected customers saved
   - Campaign ROI

2. **Visualizations**:
   - Risk distribution pie chart
   - Top 50 high-risk customers table

3. **Download**:
   - Full predictions CSV with churn probabilities and risk groups

## ⚠️ Important Notes

- **PreferedOrderCat** must be one of: "Laptop & Accessory", "Mobile", or "Mobile Phone"
- All numeric columns must be actual numbers
- Missing values will cause rows to be removed
- Factor levels must match exactly (case-sensitive)

## 🛠️ Troubleshooting

**App won't start:**
- Make sure `final_churn_model.rds` exists
- Check that all R packages are installed:
  ```r
  install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "ggplot2", "scales"))
  ```

**Upload fails:**
- Check column names match exactly (case-sensitive)
- Ensure PreferedOrderCat has valid values
- Verify no missing values in required columns

**Predictions seem wrong:**
- Verify factor levels match training data
- Check that numeric columns are actually numeric
- Ensure data format matches training data format

## 📝 Next Steps

To use with your real data:
1. Replace `test_customer_template.csv` with your actual customer data
2. Ensure your CSV follows the exact format shown in the app's data dictionary
3. Upload and view predictions!

The app is currently running in the background. Check your browser for the Shiny app window!

