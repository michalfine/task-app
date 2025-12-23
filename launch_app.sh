#!/bin/bash
# Launch script for Churn Prediction App

cd "$(dirname "$0")"

echo "=========================================="
echo "Churn Prediction App Launcher"
echo "=========================================="
echo ""
echo "Starting Shiny app..."
echo ""
echo "The app will open in your default browser"
echo "If it doesn't open automatically, look for a URL like:"
echo "  http://127.0.0.1:XXXX"
echo ""
echo "Press Ctrl+C to stop the app"
echo "=========================================="
echo ""

# Run the R script
Rscript run_churn_app.R

