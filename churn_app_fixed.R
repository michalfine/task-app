library(ggplot2)
library(dplyr)
library(scales)
library(MASS)
library(pROC)
library(caret)
library(ResourceSelection)
library(shiny)
library(shinydashboard)
library(DT)

## --------------------------------------------------------------
##  CLEAN + FILTER: 
##  - Only "Laptop & Accessory", "Mobile", "Mobile Phone"
##  - Remove rows with ANY missing value
## --------------------------------------------------------------

# Start with your data
df <- churn_data   # or whatever your object is called
df <- df %>% select(-CustomerID)  

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

# Visualization 1
# Summarise churn % per category stacked bar chart
churn_by_cat <- new_churn %>%
  count(PreferedOrderCat, Churn) %>%
  group_by(PreferedOrderCat) %>%
  mutate(pct = n / sum(n) * 100,
         label = ifelse(Churn == 0, "Retained", "Churn"),
         pct_label = percent(pct / 100, accuracy = 0.1)) %>%
  ungroup()

# 2. Plot: 100% stacked bar chart
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

# Visualization 2 gender
# 1. Create summary table with pct and label
churn_by_gender <- new_churn %>%
  count(Gender, Churn, .drop = FALSE) %>%
  group_by(Gender) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    label = ifelse(Churn == 0, "Retained", "Churn"),
    pct_label = percent(pct / 100, accuracy = 0.1)
  )

# 2. Plot: 100% stacked bar chart (Gender)
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

# Summarize churn % per gender
churn_by_gender <- new_churn %>%
  count(Gender, Churn) %>%
  group_by(Gender) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = ifelse(Churn == 0, "Retained", "Churn"),
    pct_label = paste0(round(pct, 1), "%")
  ) %>%
  ungroup()

# Plot mini pies per gender
ggplot(churn_by_gender, aes(x = "", y = pct, fill = label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ Gender, ncol = 2) +
  geom_text(aes(label = pct_label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5) +
  scale_fill_manual(values = c("Retained" = "#2ca02c", "Churn" = "#d62728")) +
  labs(
    title = "Customer Churn by Gender",
    fill = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


## Analysis --> Building models
# Step wise function model 
# Uses all customer info to predict churn
full_model <- glm(Churn~., data = new_churn, family = binomial) 

summary(full_model)

# 2. BASE MODEL (intercept only)
base_model <- glm(Churn ~ 1, data = new_churn, family = binomial)
summary(base_model)

# 3. STEPWISE MODEL (start from base, grow to full)
step_model <- stepAIC(base_model, direction = "both", scope = formula(full_model), trace = FALSE)

# 4. SHOW FINAL MODEL
step_model
summary(step_model)

# STEP 1 
# Prof code --> model w/o customer id (useless)
# 1. Remove first column and keep Churn at the end
new_churn1 <- new_churn[,-c(1)]

# 2. Add Churn back as the last column
new_churn1 <- cbind(new_churn1, new_churn$Churn)

# 3. Rename the new Churn column (19th column) to "Churn"
names(new_churn1)[ncol(new_churn1)] <- "Churn"

# 4. Ensure factor levels are set correctly BEFORE modeling
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

# 5. Full logistic model with ALL features
full_model <- glm(Churn ~ ., data = new_churn1, family = binomial)
summary(full_model)

# 6. Base (null) model — only intercept
base_model <- glm(Churn ~ 1, data = new_churn1, family = binomial)
summary(base_model)

# 7. Stepwise selection (start from full, remove weak features)
stepwmodel <- stepAIC(full_model, direction = "both", scope = full_model, trace = FALSE)
summary(stepwmodel)

# 8. Compare model quality using AIC
AIC(full_model) # score (lower is better) 
AIC(base_model) # basic guess (high/worse)
AIC(stepwmodel) # best one (lowest/best)

# Variables that affected churn
summary(stepwmodel)$coefficients

# STEP 2 --> Testing how good prediction is 
# Model Diagnostics (Run on Test Set)
set.seed(123)
idx   <- sample(nrow(new_churn1), 0.75*nrow(new_churn1))
train <- new_churn1[idx, ]
test  <- new_churn1[-idx, ]

# Refit final model on train only
final_train <- glm(formula(stepwmodel), data = train, family = binomial)

# Predict on test
test$prob <- predict(final_train, newdata = test, type = "response")
test$pred <- ifelse(test$prob > 0.5, 1, 0)

indexrand <- sample(nrow(test), 1)
testnew <- test[indexrand,] 
probnew <- predict(final_train, newdata = testnew, type = "response")
prednew <- ifelse(probnew > 0.5, 1, 0)

# 1. AUC - general info
# Creates curve to measure overall guessing accuracy
roc_obj <- roc(test$Churn, test$prob)
cat("Test AUC =", round(auc(roc_obj), 3), "\n")   # Aim for > 0.80

test$prob <- as.numeric(test$prob)
# Table
test_sheet <- data.frame(
  Predicted_Probability = round(test$prob, 4), 
  Actual_Churn          = test$Churn   
)
test_sheet
cat("Total test customers:", nrow(test_sheet), "\n")

# 2. Confusion Matrix --> table shows how well prediction did 
# Creates a table showing right/wrong guesses (how many real churners were caught)
confusionMatrix(factor(test$pred), factor(test$Churn), positive = "1")

# 3. Calibration test
# Checks if probabilities match reality (high pvalue is good)
hoslem.test(test$Churn, test$prob)$p.value   # > 0.05 = good calibration

# FIXING STEP 2 --> fix prob
calib_data <- data.frame(
  pred_logit = log(test$prob / (1 - test$prob)),
  churn = test$Churn
)
calib_model <- glm(churn ~ pred_logit, data = calib_data, family = binomial)
test$prob_cal <- predict(calib_model, type = "response")

# Check calibration
cat("New p-value:", 
    hoslem.test(test$Churn, test$prob_cal)$p.value, "\n")

# STEP 3 OPTIMAL THRESHOLD --> finds best cutoff for yes/no
coords <- coords(roc_obj, "best", ret = c("threshold", "sens", "spec"))
best_thresh <- coords$threshold
cat("Optimal threshold =", round(best_thresh, 3), "\n")

test$pred_opt <- ifelse(test$prob_cal > best_thresh, 1, 0)

# Final performance
confusionMatrix(factor(test$pred_opt), factor(test$Churn), positive = "1")

# STEP 4 --> grouping customers by Churn Risk (low med high risk)
test$risk <- cut(test$prob, 
                 breaks = c(0, 0.1, 0.3, 1), 
                 labels = c("Low (<10%)", "Medium (10-30%)", "High (>30%)"))

table(test$risk, test$Churn)

# Save the model and training data for the app
saveRDS(final_train, "final_churn_model.rds")

# Save cleaned training data for demo
new_churn_clean <- new_churn1  # This will be used in the app demo
saveRDS(new_churn_clean, "demo_data.rds")

cat("Model saved successfully! File created:\n")
cat(file.path(getwd(), "final_churn_model.rds"), "\n")

# **Based on the results our client can make $88,731 profit 


# =====================================================
# FINAL CHURN PREVENTION DASHBOARD – PROFESSIONAL & CLIENT-READY
# Includes full data dictionary + works perfectly
# =====================================================

# Load your trained model
churn_model <- readRDS("final_churn_model.rds")

# Load demo data if it exists
if(file.exists("demo_data.rds")) {
  demo_data <- readRDS("demo_data.rds")
} else {
  demo_data <- NULL
}

# ------------------- SAFE SCORING FUNCTION -------------------
score_customers <- function(df) {
  # Required columns based on the model
  needed <- c("Tenure", "WarehouseToHome", "NumberOfDeviceRegistered",
              "PreferedOrderCat", "PreferredLoginDevice", "SatisfactionScore",
              "MaritalStatus", "NumberOfAddress", "Complain", "DaySinceLastOrder",
              "HourSpendOnApp", "OrderAmountHikeFromlastYear", "CouponUsed",
              "OrderCount", "CashbackAmount", "Gender")
  
  # Check which columns are available
  missing_cols <- setdiff(needed, names(df))
  if(length(missing_cols) > 0) {
    showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")), 
                     type = "error", duration = 8)
    return(NULL)
  }
  
  # Select only needed columns
  df_work <- df[, needed, drop = FALSE]
  
  # Replace blank strings with NA
  df_work <- df_work %>%
    mutate(across(everything(),
                  ~ ifelse(trimws(as.character(.)) == "" | as.character(.) == "NA", NA, .)))
  
  # Remove rows with any NA
  df_work <- na.omit(df_work)
  
  if(nrow(df_work) == 0) {
    showNotification("No complete rows found after removing missing values!", 
                     type = "error", duration = 8)
    return(NULL)
  }
  
  # Filter to only valid categories (same as training)
  if("PreferedOrderCat" %in% names(df_work)) {
    valid_cats <- c("Laptop & Accessory", "Mobile", "Mobile Phone")
    df_work <- df_work %>% filter(PreferedOrderCat %in% valid_cats)
    
    if(nrow(df_work) == 0) {
      showNotification("No rows with valid PreferedOrderCat (must be: Laptop & Accessory, Mobile, or Mobile Phone)", 
                       type = "error", duration = 8)
      return(NULL)
    }
  }
  
  # Set factor levels to match training data EXACTLY
  if("PreferedOrderCat" %in% names(df_work)) {
    df_work$PreferedOrderCat <- factor(df_work$PreferedOrderCat,
                                      levels = c("Laptop & Accessory", "Mobile", "Mobile Phone",
                                                 "Grocery", "Fashion", "Others"))
  }
  if("PreferredLoginDevice" %in% names(df_work)) {
    df_work$PreferredLoginDevice <- factor(df_work$PreferredLoginDevice,
                                          levels = c("Computer", "Mobile Phone", "Phone"))
  }
  if("MaritalStatus" %in% names(df_work)) {
    df_work$MaritalStatus <- factor(df_work$MaritalStatus,
                                   levels = c("Single", "Married", "Divorced"))
  }
  if("Gender" %in% names(df_work)) {
    df_work$Gender <- factor(df_work$Gender, levels = c("Male", "Female"))
  }
  
  # Ensure numeric columns are numeric
  numeric_cols <- c("Tenure", "WarehouseToHome", "NumberOfDeviceRegistered",
                   "SatisfactionScore", "NumberOfAddress", "Complain", 
                   "DaySinceLastOrder", "HourSpendOnApp", "OrderAmountHikeFromlastYear",
                   "CouponUsed", "OrderCount", "CashbackAmount")
  
  for(col in numeric_cols) {
    if(col %in% names(df_work)) {
      df_work[[col]] <- as.numeric(as.character(df_work[[col]]))
    }
  }
  
  # Try to predict
  tryCatch({
    df_work$Churn_Prob <- predict(churn_model, newdata = df_work, type = "response")
    df_work$Risk_Group <- cut(df_work$Churn_Prob,
                             breaks = c(0, 0.1, 0.3, 1),
                             labels = c("Low Risk (<10%)", "Medium Risk (10–30%)", "High Risk (>30%)"))
    
    # Add CustomerID back if it exists in original
    if("CustomerID" %in% names(df)) {
      df_work$CustomerID <- df[rownames(df_work), "CustomerID"]
    }
    
    return(df_work)
  }, error = function(e) {
    showNotification(paste("Prediction error:", e$message), type = "error", duration = 10)
    return(NULL)
  })
}

# ------------------- UI -------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Customer Churn Prevention Tool", titleWidth = 400),
  
  dashboardSidebar(
    width = 380,
    tags$style(HTML("
      .sidebar { background: #2c3e50; color: white; }
      .sidebar a { color: #ecf0f1 !important; }
    ")),
    
    fileInput("file", "Upload Your Customer CSV", accept = ".csv"),
    br(),
    actionButton("demo", "Load Demo Data", class = "btn-success btn-lg btn-block"),
    br(),
    
    # BEAUTIFUL DATA DICTIONARY / README
    div(style = "background:#34495e; color:white; padding:20px; border-radius:12px; margin:15px; font-size:14px;",
        h4(icon("book"), " Required CSV Format – Data Dictionary", style = "color:#1abc9c; margin-top:0;"),
        p("Your CSV must contain these columns (case-sensitive). Missing columns = no prediction."),
        tags$table(style = "width:100%; font-size:13px; line-height:1.6;",
                   tags$thead(tags$tr(
                     tags$th(style = "text-align:left; padding:8px;", "Variable"),
                     tags$th(style = "text-align:left; padding:8px;", "Description")
                   )),
                   tags$tbody(
                     tags$tr(tags$td(tags$b("CustomerID")),           tags$td("Unique ID – optional, just for tracking")),
                     tags$tr(tags$td(tags$b("Tenure")),               tags$td("Months/years customer has been with you")),
                     tags$tr(tags$td(tags$b("PreferredLoginDevice")), tags$td("Mobile Phone / Computer / Phone")),
                     tags$tr(tags$td(tags$b("CityTier")),             tags$td("1 = big city, 3 = small town")),
                     tags$tr(tags$td(tags$b("WarehouseToHome")),      tags$td("Distance from warehouse to home")),
                     tags$tr(tags$td(tags$b("PreferredPaymentMode")), tags$td("Debit Card, Credit Card, UPI, COD, etc.")),
                     tags$tr(tags$td(tags$b("Gender")),               tags$td("Male / Female")),
                     tags$tr(tags$td(tags$b("HourSpendOnApp")),       tags$td("Hours spent on app/website last month")),
                     tags$tr(tags$td(tags$b("NumberOfDeviceRegistered")), tags$td("How many devices registered")),
                     tags$tr(tags$td(tags$b("PreferedOrderCat")),     tags$td("Laptop & Accessory, Mobile Phone, Fashion, etc.")),
                     tags$tr(tags$td(tags$b("SatisfactionScore")),    tags$td("1–5 rating")),
                     tags$tr(tags$td(tags$b("MaritalStatus")),        tags$td("Single / Married / Divorced")),
                     tags$tr(tags$td(tags$b("NumberOfAddress")),      tags$td("Number of saved addresses")),
                     tags$tr(tags$td(tags$b("Complain")),             tags$td("1 = had complaint last month, 0 = none")),
                     tags$tr(tags$td(tags$b("OrderAmountHikeFromlastYear")), tags$td("% change in order value vs last year")),
                     tags$tr(tags$td(tags$b("CouponUsed")),           tags$td("Number of coupons used last month")),
                     tags$tr(tags$td(tags$b("OrderCount")),           tags$td("Number of orders last month")),
                     tags$tr(tags$td(tags$b("DaySinceLastOrder")),    tags$td("Days since last purchase")),
                     tags$tr(tags$td(tags$b("CashbackAmount")),       tags$td("Average cashback last month"))
                   )
        ),
        br(),
        div(style = "text-align:center; font-size:16px; color:#1abc9c;",
            tags$strong("LTV = $764"), "  |  ", tags$strong("Offer Cost = $30")
        ),
        br(),
        div(style = "background:#e74c3c; padding:10px; border-radius:8px; margin-top:10px;",
            tags$strong("IMPORTANT:"), " PreferedOrderCat must be one of: Laptop & Accessory, Mobile, or Mobile Phone"
        )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { box-shadow: 0 6px 15px rgba(0,0,0,0.12); border-radius: 12px; }
      .value-box { font-size: 36px; font-weight: bold; }
      .small-box { height: 130px; }
    "))),
    
    fluidRow(
      valueBoxOutput("total_cust", width = 3),
      valueBoxOutput("high_risk", width = 3),
      valueBoxOutput("expected_saved", width = 3),
      valueBoxOutput("roi_box", width = 3)
    ),
    
    fluidRow(
      box(title = "Churn Risk Distribution", width = 6, status = "primary", solidHeader = TRUE,
          plotOutput("risk_pie", height = "380px")),
      box(title = "Top 50 High-Risk Customers – Act Now!", width = 6, status = "danger", solidHeader = TRUE,
          DTOutput("high_risk_table"))
    ),
    
    fluidRow(
      box(title = "Full Predictions – Download for Campaign", width = 12, status = "info", solidHeader = TRUE,
          downloadButton("download", "Download CSV with Predictions", class = "btn-primary btn-lg"),
          br(), br(),
          DTOutput("full_table"))
    )
  )
)

# ------------------- SERVER -------------------
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      result <- score_customers(df)
      if(!is.null(result)) {
        data(result)
        showNotification(paste("Successfully loaded", nrow(result), "customers!"), 
                         type = "message", duration = 4)
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$demo, {
    if(is.null(demo_data)) {
      showNotification("Demo data not available. Please train the model first.", 
                       type = "error", duration = 6)
      return()
    }
    # Remove Churn column if it exists for demo
    demo_for_pred <- demo_data
    if("Churn" %in% names(demo_for_pred)) {
      demo_for_pred <- demo_for_pred %>% select(-Churn)
    }
    result <- score_customers(demo_for_pred)
    if(!is.null(result)) {
      data(result)
      showNotification(paste("Demo loaded –", nrow(result), "customers scored!"), 
                      type = "message", duration = 6)
    }
  })
  
  output$total_cust <- renderValueBox({ 
    req(data())
    valueBox(nrow(data()), "Customers Scored", icon("users"), color = "aqua") 
  })
  
  output$high_risk  <- renderValueBox({ 
    req(data())
    valueBox(sum(data()$Risk_Group == "High Risk (>30%)", na.rm = TRUE), 
             "High Risk", icon("exclamation-triangle"), color = "red") 
  })
  
  output$expected_saved <- renderValueBox({
    req(data())
    target <- data() %>% filter(Risk_Group %in% c("Medium Risk (10–30%)","High Risk (>30%)"))
    valueBox(round(sum(target$Churn_Prob, na.rm = TRUE), 0), 
             "Expected Saved", icon("heart"), color = "green")
  })
  
  output$roi_box <- renderValueBox({
    req(data())
    target <- data() %>% filter(Risk_Group %in% c("Medium Risk (10–30%)","High Risk (>30%)"))
    if(nrow(target) > 0) {
      roi <- round((sum(target$Churn_Prob, na.rm = TRUE) * 764) / max(1, nrow(target) * 30), 1)
      valueBox(paste0(roi, "×"), "Campaign ROI", icon("dollar-sign"), color = "purple")
    } else {
      valueBox("0×", "Campaign ROI", icon("dollar-sign"), color = "purple")
    }
  })
  
  output$risk_pie <- renderPlot({
    req(data())
    risk_summary <- data() %>% 
      count(Risk_Group) %>% 
      mutate(pct = n/sum(n)*100)
    
    ggplot(risk_summary, aes("", pct, fill = Risk_Group)) +
      geom_col(width = 1, color = "white", size = 2) +
      coord_polar("y") +
      geom_text(aes(label = paste0(round(pct), "%")), 
                position = position_stack(vjust = 0.5),
                color = "white", size = 11, fontface = "bold") +
      scale_fill_manual(values = c("Low Risk (<10%)" = "#27ae60",
                                   "Medium Risk (10–30%)" = "#f39c12",
                                   "High Risk (>30%)" = "#e74c3c")) +
      theme_void() + 
      theme(legend.position = "bottom", legend.title = element_blank())
  })
  
  output$high_risk_table <- renderDT({
    req(data())
    high_risk_data <- data() %>%
      filter(Risk_Group == "High Risk (>30%)") %>%
      arrange(desc(Churn_Prob)) %>%
      head(50)
    
    # Select columns, prioritizing CustomerID if it exists
    cols_to_show <- c("Churn_Prob", "Risk_Group")
    if("CustomerID" %in% names(high_risk_data)) {
      cols_to_show <- c("CustomerID", cols_to_show)
    }
    cols_to_show <- c(cols_to_show, setdiff(names(high_risk_data), cols_to_show))
    cols_to_show <- cols_to_show[cols_to_show %in% names(high_risk_data)]
    
    datatable(high_risk_data[, cols_to_show, drop = FALSE],
              options = list(pageLength = 10, scrollX = TRUE), 
              rownames = FALSE) %>%
      formatRound(columns = "Churn_Prob", digits = 4)
  })
  
  output$full_table <- renderDT({
    req(data())
    cols_to_show <- c("Churn_Prob", "Risk_Group")
    if("CustomerID" %in% names(data())) {
      cols_to_show <- c("CustomerID", cols_to_show)
    }
    cols_to_show <- c(cols_to_show, setdiff(names(data()), cols_to_show))
    cols_to_show <- cols_to_show[cols_to_show %in% names(data())]
    
    datatable(data()[, cols_to_show, drop = FALSE],
              options = list(pageLength = 15, scrollX = TRUE), 
              rownames = FALSE) %>%
      formatRound(columns = "Churn_Prob", digits = 4)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("Churn_Predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# ------------------- RUN -------------------
shinyApp(ui, server)

