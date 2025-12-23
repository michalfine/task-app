# =====================================================
# CHURN PREDICTION APP - Shiny Dashboard
# =====================================================
# This is the Shiny app that uses the trained model to predict churn
# Requires: final_churn_model.rds (created by train_model.R)

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(scales)

# Check if model exists
if(!file.exists("final_churn_model.rds")) {
  stop("ERROR: final_churn_model.rds not found!\n",
       "Please run train_model.R first to create the model.")
}

# Load your trained model
churn_model <- readRDS("final_churn_model.rds")
cat("✓ Model loaded successfully\n")

# Load demo data if it exists
if(file.exists("demo_data.rds")) {
  demo_data <- readRDS("demo_data.rds")
  cat("✓ Demo data loaded\n")
} else {
  demo_data <- NULL
  cat("⚠ Demo data not found (demo button will be disabled)\n")
}

# ------------------- SCORING FUNCTION -------------------
score_customers <- function(df, skip_category_filter = FALSE) {
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
  
  # Check for missing values before removing rows (for better error messages)
  missing_info <- data.frame(
    column = names(df_work),
    missing_count = sapply(df_work, function(x) sum(is.na(x)))
  )
  missing_info <- missing_info[missing_info$missing_count > 0, ]
  
  # Remove rows with any NA
  df_work <- na.omit(df_work)
  
  if(nrow(df_work) == 0) {
    if(nrow(missing_info) > 0) {
      missing_cols <- paste(missing_info$column, collapse = ", ")
      showNotification(paste("No complete rows found! Missing values in:", missing_cols, 
                             ". Please ensure all required columns have values."), 
                       type = "error", duration = 10)
    } else {
      showNotification("No complete rows found after removing missing values!", 
                       type = "error", duration = 8)
    }
    return(NULL)
  }
  
  # Filter to only valid categories (same as training) - skip for demo data
  if(!skip_category_filter && "PreferedOrderCat" %in% names(df_work)) {
    # Convert to character first and trim whitespace to handle any spacing issues
    df_work$PreferedOrderCat <- trimws(as.character(df_work$PreferedOrderCat))
    valid_cats <- c("Laptop & Accessory", "Mobile", "Mobile Phone")
    
    # Check what values we have before filtering (for better error messages)
    unique_vals_before <- unique(df_work$PreferedOrderCat[!is.na(df_work$PreferedOrderCat)])
    
    df_work <- df_work %>% filter(PreferedOrderCat %in% valid_cats)
    
    if(nrow(df_work) == 0) {
      # Show what values were actually found for debugging
      if(length(unique_vals_before) > 0) {
        showNotification(paste("No rows with valid PreferedOrderCat. Found values:", paste(unique_vals_before, collapse = ", "), 
                                ". Must be exactly: Laptop & Accessory, Mobile, or Mobile Phone"), 
                         type = "error", duration = 10)
      } else {
        showNotification("No rows with valid PreferedOrderCat (must be: Laptop & Accessory, Mobile, or Mobile Phone)", 
                         type = "error", duration = 8)
      }
      return(NULL)
    }
  }
  
  # Set factor levels to match training data EXACTLY
  if("PreferedOrderCat" %in% names(df_work)) {
    # Ensure it's character and trimmed (in case skip_category_filter was used)
    df_work$PreferedOrderCat <- trimws(as.character(df_work$PreferedOrderCat))
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
    if(!is.null(demo_data)) {
      actionButton("demo", "Load Demo Data", class = "btn-success btn-lg btn-block")
    },
    br(),
    
    # DATA DICTIONARY
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
                     tags$tr(tags$td(tags$b("NumberOfAddress")),       tags$td("Number of saved addresses")),
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
  
  if(!is.null(demo_data)) {
    observeEvent(input$demo, {
      # Remove Churn column if it exists for demo
      demo_for_pred <- demo_data
      if("Churn" %in% names(demo_for_pred)) {
        demo_for_pred <- demo_for_pred %>% dplyr::select(-Churn)
      }
      # Skip category filter for demo data since it's already validated
      result <- score_customers(demo_for_pred, skip_category_filter = TRUE)
      if(!is.null(result)) {
        data(result)
        showNotification(paste("Demo loaded –", nrow(result), "customers scored!"), 
                          type = "message", duration = 6)
      }
    })
  }
  
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

# ------------------- RUN APP -------------------
cat("\n")
cat("========================================\n")
cat("Starting Churn Prediction App...\n")
cat("========================================\n")
cat("App will open in your default browser\n")
cat("Press Ctrl+C (or Cmd+C on Mac) to stop\n")
cat("========================================\n\n")

shinyApp(ui, server)

