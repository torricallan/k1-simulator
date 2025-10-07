# K1 500m Kayak Race Simulation - Shiny Web Application
# Entry point for deployment platforms (Render, shinyapps.io, etc.)

cat("=== K1 500m Kayak Race Simulator ===\n")
cat("Initializing application for deployment...\n")

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(deSolve)

# Source the main application components
source("personalized_k1_simulation.R")

# Load the Shiny app definition from k1_shiny_app.R
# This file contains the UI and server definitions
source("k1_shiny_app.R")

# The app is now ready and will be launched by the deployment platform