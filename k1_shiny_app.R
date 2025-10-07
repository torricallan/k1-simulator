# K1 500m Kayak Race Simulation - Shiny Web App
# Interactive simulation with velocity and power curve visualization

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(deSolve)

# Source the simulation functions
source("personalized_k1_simulation.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "K1 500m Kayak Race Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation", tabName = "simulation", icon = icon("water")),
      menuItem("Pace Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Main simulation tab
      tabItem(tabName = "simulation",
        # First row: Input panel and main plots
        fluidRow(
          # Input panel
          box(
            title = "Your Physiological Parameters", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            
            h4("Enter Your Parameters:"),
            
            # Speed unit selection
            radioButtons("speed_unit", "Speed Units:",
                        choices = list("km/h" = "kmh", "min/km" = "pace", "m/s" = "ms"),
                        selected = "kmh",
                        inline = TRUE),
            
            br(),
            
            # Maximum velocity input (dynamically updated based on unit)
            conditionalPanel(
              condition = "input.speed_unit == 'kmh'",
              numericInput("max_velocity_kmh", 
                          "Maximum Velocity (km/h):",
                          value = 19.4, 
                          min = 14.0, 
                          max = 29.0, 
                          step = 0.5)
            ),
            conditionalPanel(
              condition = "input.speed_unit == 'pace'",
              textInput("max_velocity_pace", 
                       "Maximum Velocity (mm:ss/km):",
                       value = "3:06",
                       placeholder = "e.g., 3:06")
            ),
            conditionalPanel(
              condition = "input.speed_unit == 'ms'",
              numericInput("max_velocity_ms", 
                          "Maximum Velocity (m/s):",
                          value = 5.4, 
                          min = 4.0, 
                          max = 8.0, 
                          step = 0.1)
            ),
            
            helpText("Your peak sprint speed capability"),
            
            # Test pace input (dynamically updated based on unit)
            conditionalPanel(
              condition = "input.speed_unit == 'kmh'",
              numericInput("test_pace_kmh", 
                          "Max Distance Test Pace (km/h):",
                          value = 18.0, 
                          min = 12.6, 
                          max = 25.0, 
                          step = 0.5)
            ),
            conditionalPanel(
              condition = "input.speed_unit == 'pace'",
              textInput("test_pace_pace", 
                       "Max Distance Test Pace (mm:ss/km):",
                       value = "3:18",
                       placeholder = "e.g., 3:18")
            ),
            conditionalPanel(
              condition = "input.speed_unit == 'ms'",
              numericInput("test_pace_ms", 
                          "Max Distance Test Pace (m/s):",
                          value = 5.0, 
                          min = 3.5, 
                          max = 7.0, 
                          step = 0.1)
            ),
            
            helpText("Pace for your distance test (e.g., 120% of threshold)"),
            
            numericInput("max_distance", 
                        "Maximum Distance at Test Pace (m):",
                        value = 250, 
                        min = 100, 
                        max = 500, 
                        step = 10),
            
            helpText("Distance you can sustain at the test pace"),
            
            # Critical speed input (dynamically updated based on unit)
            conditionalPanel(
              condition = "input.speed_unit == 'kmh'",
              numericInput("critical_speed_kmh", 
                          "Critical Aerobic Speed (km/h):",
                          value = 16.6, 
                          min = 10.8, 
                          max = 21.6, 
                          step = 0.5)
            ),
            conditionalPanel(
              condition = "input.speed_unit == 'pace'",
              textInput("critical_speed_pace", 
                       "Critical Aerobic Speed (mm:ss/km):",
                       value = "3:36",
                       placeholder = "e.g., 3:36")
            ),
            conditionalPanel(
              condition = "input.speed_unit == 'ms'",
              numericInput("critical_speed_ms", 
                          "Critical Aerobic Speed (m/s):",
                          value = 4.6, 
                          min = 3.0, 
                          max = 6.0, 
                          step = 0.1)
            ),
            
            helpText("Your sustainable aerobic threshold pace"),
            
            numericInput("body_weight", 
                        "Body Weight (kg):",
                        value = 85, 
                        min = 50, 
                        max = 120, 
                        step = 1),
            
            helpText("Your body weight"),
            
            # 400m pace input with unit conversion
            conditionalPanel(
              condition = "input.speed_unit == 'kmh'",
              numericInput("pace_400m_kmh", 
                          "400m Training Pace (km/h) [Optional]:",
                          value = NA, 
                          min = 10, 
                          max = 25, 
                          step = 0.1)
            ),
            
            conditionalPanel(
              condition = "input.speed_unit == 'pace'",
              textInput("pace_400m_pace", 
                       "400m Training Pace (mm:ss/km) [Optional]:",
                       value = "",
                       placeholder = "e.g., 4:00")
            ),
            
            conditionalPanel(
              condition = "input.speed_unit == 'ms'",
              numericInput("pace_400m_ms", 
                          "400m Training Pace (m/s) [Optional]:",
                          value = NA, 
                          min = 2.8, 
                          max = 7.0, 
                          step = 0.1)
            ),
            
            helpText("Your 400m training piece pace (helps refine W' calculation)"),
            
            br(),
            actionButton("simulate", "Run Simulation", 
                        class = "btn-primary btn-lg", 
                        style = "width: 100%")
          ),
          
          # Velocity plot
          box(
            title = "Velocity Profile", 
            status = "info", 
            solidHeader = TRUE,
            width = 4,
            
            conditionalPanel(
              condition = "input.simulate == 0",
              h4("Run simulation to see velocity profile", 
                 style = "text-align: center; color: #999; margin-top: 150px;")
            ),
            
            conditionalPanel(
              condition = "input.simulate > 0",
              plotlyOutput("velocity_plot", height = "450px")
            )
          ),
          
          # Power plot  
          box(
            title = "Power Profile", 
            status = "warning", 
            solidHeader = TRUE,
            width = 4,
            
            conditionalPanel(
              condition = "input.simulate == 0",
              h4("Run simulation to see power profile", 
                 style = "text-align: center; color: #999; margin-top: 150px;")
            ),
            
            conditionalPanel(
              condition = "input.simulate > 0",
              plotlyOutput("power_plot", height = "450px")
            )
          )
        ),
        
        # Second row: Performance Summary
        fluidRow(
          box(
            title = "Performance Summary", 
            status = "success", 
            solidHeader = TRUE,
            width = 12,
            
            conditionalPanel(
              condition = "input.simulate == 0",
              h4("Click 'Run Simulation' to see your results", 
                 style = "text-align: center; color: #999;")
            ),
            
            conditionalPanel(
              condition = "input.simulate > 0",
              
              fluidRow(
                column(6,
                  h4("Calculated Parameters"),
                  verbatimTextOutput("calculated_params")
                ),
                column(6,
                  h4("Race Results"),
                  verbatimTextOutput("race_results")
                )
              ),
              
              fluidRow(
                column(12,
                  h4("Test Input Calibration"),
                  verbatimTextOutput("calibration_results"),
                  style = "margin-top: 15px;"
                )
              )
            )
          )
        )
      ),
      
      # Perturbation Analysis tab  
      tabItem(tabName = "analysis",
        
        # Pace sufficiency analysis report
        fluidRow(
          box(
            title = "Pace Sufficiency Analysis",
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            
            conditionalPanel(
              condition = "typeof simulation_results() == 'undefined' || simulation_results() == null",
              div(
                style = "text-align: center; margin: 40px 0;",
                h4("⚠️ Run a simulation first", style = "color: #f39c12;"),
                p("Go to the Simulation tab and run a simulation with your parameters to see pace sufficiency analysis.", style = "color: #666;")
              )
            ),
            
            conditionalPanel(
              condition = "typeof simulation_results() != 'undefined' && simulation_results() != null",
              
              div(
                style = "margin-bottom: 15px;",
                h4("Can your physiological parameters sustain your test pace over 500m?"),
                p("Analysis runs automatically after simulation completion.", style = "color: #666;")
              ),
              
              conditionalPanel(
                condition = "typeof analysis_results() == 'undefined' || analysis_results() == null",
                h5("Analyzing pace sufficiency...", style = "text-align: center; color: #999; margin: 30px 0;")
              ),
              
              conditionalPanel(
                condition = "typeof analysis_results() != 'undefined' && analysis_results() != null",
                div(
                  h5("Analysis Results:"),
                  verbatimTextOutput("analysis_report"),
                  br(),
                  downloadButton("download_analysis",
                                "Download Detailed Report",
                                class = "btn-info",
                                icon = icon("download"))
                )
              )
            )
          )
        )
      ),
      
      # About tab
      tabItem(tabName = "about",
        box(
          title = "About the K1 500m Simulator", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          
          h3("K1 500m Kayak Race Simulation"),
          p("This interactive simulator models elite K1 500m kayak racing performance based on your physiological parameters."),
          
          h4("How It Works:"),
          tags$ul(
            tags$li("Converts your velocity capabilities to power outputs using drag equations"),
            tags$li("Calculates your Anaerobic Work Capacity (W') from test pace and distance"),
            tags$li("Models realistic fatigue-based power decline during the race"),
            tags$li("Simulates race velocity using differential equations")
          ),
          
          h4("Input Parameters:"),
          tags$ul(
            tags$li(strong("Maximum Velocity:"), " Your peak sprint speed capability"),
            tags$li(strong("Max Distance Test Pace:"), " Specific pace for measuring anaerobic capacity (typically 120-130% of threshold)"),
            tags$li(strong("Maximum Distance:"), " Distance you can sustain at the test pace"),
            tags$li(strong("Critical Speed:"), " Your aerobic threshold pace (sustainable for ~30-60 minutes)"),
            tags$li(strong("Body Weight:"), " Used to adjust drag characteristics")
          ),
          
          h4("Physiological Accuracy:"),
          p("The simulation uses research-validated models including:"),
          tags$ul(
            tags$li("Hydrodynamic drag equation: P = 0.5 × Cd × ρ × A × v³"),
            tags$li("Critical Power and W' (Anaerobic Work Capacity) concepts"),
            tags$li("Exponential fatigue decay based on kayak physiology research"),
            tags$li("Body weight adjustments for drag coefficient and wetted area")
          ),
          
          h4("Performance Benchmarks:"),
          p("Results are compared against world record performance:"),
          tags$ul(
            tags$li("Men's World Record: 95.15 seconds (Roi Rodríguez, 2018)"),
            tags$li("Women's World Record: 106.19 seconds (Aimee Fisher, 2024)")
          ),
          
          br(),
          p("Developed using R and Shiny with research-based kayak performance models.")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Unit conversion functions
  kmh_to_ms <- function(kmh) kmh / 3.6
  ms_to_kmh <- function(ms) ms * 3.6
  pace_to_ms <- function(pace) 1000 / (pace * 60)  # min/km to m/s
  ms_to_pace <- function(ms) 1000 / (ms * 60)      # m/s to min/km
  
  # Helper function for null coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Parse mm:ss format to decimal minutes
  parse_mmss_to_decimal <- function(mmss_string) {
    if (is.null(mmss_string) || mmss_string == "" || is.na(mmss_string)) {
      return(NA)
    }
    
    # Handle both "m:ss" and decimal formats for backwards compatibility
    if (grepl(":", mmss_string)) {
      parts <- strsplit(mmss_string, ":")[[1]]
      if (length(parts) != 2) return(NA)
      
      minutes <- as.numeric(parts[1])
      seconds <- as.numeric(parts[2])
      
      if (is.na(minutes) || is.na(seconds) || seconds >= 60 || seconds < 0) {
        return(NA)
      }
      
      return(minutes + seconds/60)
    } else {
      # Fallback to decimal format
      decimal_val <- as.numeric(mmss_string)
      if (is.na(decimal_val)) return(NA)
      return(decimal_val)
    }
  }
  
  # Reactive values to store simulation results
  simulation_results <- reactiveVal(NULL)
  
  # Convert inputs to m/s based on selected units
  get_converted_values <- reactive({
    # Get the appropriate input values based on unit selection
    max_vel <- switch(input$speed_unit,
                     "kmh" = kmh_to_ms(input$max_velocity_kmh %||% 19.4),
                     "pace" = pace_to_ms(parse_mmss_to_decimal(input$max_velocity_pace) %||% 3.1),
                     "ms" = input$max_velocity_ms %||% 5.4)
    
    test_pace <- switch(input$speed_unit,
                       "kmh" = kmh_to_ms(input$test_pace_kmh %||% 18.0),
                       "pace" = pace_to_ms(parse_mmss_to_decimal(input$test_pace_pace) %||% 3.3),
                       "ms" = input$test_pace_ms %||% 5.0)
    
    crit_speed <- switch(input$speed_unit,
                        "kmh" = kmh_to_ms(input$critical_speed_kmh %||% 16.6),
                        "pace" = pace_to_ms(parse_mmss_to_decimal(input$critical_speed_pace) %||% 3.6),
                        "ms" = input$critical_speed_ms %||% 4.6)
    
    # 400m pace conversion (optional input)
    pace_400m <- switch(input$speed_unit,
                       "kmh" = if(!is.na(input$pace_400m_kmh)) kmh_to_ms(input$pace_400m_kmh) else NA,
                       "pace" = {
                         parsed_400m <- parse_mmss_to_decimal(input$pace_400m_pace)
                         if(!is.na(parsed_400m)) pace_to_ms(parsed_400m) else NA
                       },
                       "ms" = if(!is.na(input$pace_400m_ms)) input$pace_400m_ms else NA)
    
    list(
      max_velocity = max_vel,
      test_pace = test_pace,
      critical_speed = crit_speed,
      pace_400m = pace_400m
    )
  })
  
  # Validation for inputs - only run when inputs are available
  observe({
    # Validate mm:ss format for pace inputs
    if (input$speed_unit == "pace") {
      # Check max velocity pace format
      if (!is.null(input$max_velocity_pace) && input$max_velocity_pace != "") {
        if (is.na(parse_mmss_to_decimal(input$max_velocity_pace))) {
          showNotification("Maximum velocity pace must be in mm:ss format (e.g., 3:06)", 
                          type = "error", duration = 5)
        }
      }
      
      # Check test pace format
      if (!is.null(input$test_pace_pace) && input$test_pace_pace != "") {
        if (is.na(parse_mmss_to_decimal(input$test_pace_pace))) {
          showNotification("Test pace must be in mm:ss format (e.g., 3:18)", 
                          type = "error", duration = 5)
        }
      }
      
      # Check critical speed pace format
      if (!is.null(input$critical_speed_pace) && input$critical_speed_pace != "") {
        if (is.na(parse_mmss_to_decimal(input$critical_speed_pace))) {
          showNotification("Critical speed pace must be in mm:ss format (e.g., 3:36)", 
                          type = "error", duration = 5)
        }
      }
      
      # Check 400m pace format if provided
      if (!is.null(input$pace_400m_pace) && input$pace_400m_pace != "") {
        if (is.na(parse_mmss_to_decimal(input$pace_400m_pace))) {
          showNotification("400m pace must be in mm:ss format (e.g., 4:00)", 
                          type = "error", duration = 5)
        }
      }
    }
    
    converted <- get_converted_values()
    
    # Check if all converted values are available
    if (is.null(converted$max_velocity) || is.null(converted$critical_speed) ||
        is.null(converted$test_pace) || 
        is.na(converted$max_velocity) || is.na(converted$critical_speed) ||
        is.na(converted$test_pace)) {
      return()  # Exit early if inputs aren't ready
    }
    
    if (converted$max_velocity <= converted$critical_speed) {
      showNotification("Maximum velocity must be greater than critical speed", 
                      type = "warning", duration = 5)
    }
    
    if (converted$test_pace <= converted$critical_speed) {
      showNotification("Test pace must be greater than critical speed", 
                      type = "warning", duration = 5)
    }
    
    if (converted$test_pace >= converted$max_velocity) {
      showNotification("Test pace must be less than maximum velocity", 
                      type = "warning", duration = 5)
    }
  })
  
  # Run simulation when button is clicked
  observeEvent(input$simulate, {
    
    # Get converted values
    converted <- get_converted_values()
    
    # Validate inputs - check for NULL/NA first
    if (is.null(converted$max_velocity) || is.null(converted$critical_speed) ||
        is.null(converted$test_pace) || is.null(input$max_distance) ||
        is.null(input$body_weight) ||
        is.na(converted$max_velocity) || is.na(converted$critical_speed) ||
        is.na(converted$test_pace) || is.na(input$max_distance) ||
        is.na(input$body_weight)) {
      showNotification("Please ensure all parameters are entered", 
                      type = "error", duration = 5)
      return()
    }
    
    # Validate input relationships
    if (converted$max_velocity <= converted$critical_speed ||
        converted$test_pace <= converted$critical_speed ||
        converted$test_pace >= converted$max_velocity) {
      showNotification("Please check your input parameters", 
                      type = "error", duration = 5)
      return()
    }
    
    # Show progress
    withProgress(message = 'Running simulation...', value = 0, {
      
      incProgress(0.2, detail = "Setting up parameters...")
      
      # Create input list using converted values
      user_inputs <- list(
        max_velocity = converted$max_velocity,
        max_distance_pace = converted$test_pace,
        max_distance = input$max_distance,
        critical_speed = converted$critical_speed,
        body_weight = input$body_weight,
        pace_400m = converted$pace_400m
      )
      
      incProgress(0.4, detail = "Calculating power parameters...")
      
      # Run simulation
      tryCatch({
        results <- run_personalized_simulation(user_inputs, estimated_time = 110)
        simulation_results(results)
        
        incProgress(0.8, detail = "Generating plots...")
        
        showNotification("Simulation completed successfully!", 
                        type = "message", duration = 3)
        
        incProgress(1.0, detail = "Done!")
        
      }, error = function(e) {
        showNotification(paste("Simulation error:", e$message), 
                        type = "error", duration = 10)
      })
    })
  })
  
  # Calculated parameters output
  output$calculated_params <- renderText({
    results <- simulation_results()
    if (is.null(results)) return("")
    
    power_profile <- results$power_profile[[1]]
    user_inputs <- results$user_inputs[[1]]
    
    paste(
      sprintf("Maximum Power: %.0f watts", power_profile$max_power),
      sprintf("Test Pace Power: %.0f watts", power_profile$test_pace_power),
      sprintf("Critical Power: %.0f watts", power_profile$critical_power),
      sprintf("Anaerobic Work Capacity: %.1f kJ", power_profile$w_prime/1000),
      sprintf("Power-to-Weight: %.1f W/kg", power_profile$max_power/user_inputs$body_weight),
      sep = "\n"
    )
  })
  
  # Race results output
  output$race_results <- renderText({
    results <- simulation_results()
    if (is.null(results)) return("")
    
    user_inputs <- results$user_inputs[[1]]
    final_time <- tail(results$time, 1)
    avg_velocity <- mean(results$velocity)
    avg_power <- mean(results$power)
    
    world_record <- 95.15
    performance_pct <- (world_record / final_time) * 100
    
    performance_level <- if (final_time <= world_record) {
      "🏆 WORLD RECORD PACE!"
    } else if (performance_pct >= 90) {
      "🥇 Elite level"
    } else if (performance_pct >= 80) {
      "🥈 Competitive club level"
    } else {
      "🥉 Recreational level"
    }
    
    paste(
      sprintf("Race Time: %.2f seconds", final_time),
      sprintf("Average Velocity: %.2f m/s (%.1f km/h)", avg_velocity, avg_velocity * 3.6),
      sprintf("Average Power: %.0f watts", avg_power),
      sprintf("Total Energy: %.1f kJ", tail(results$energy, 1)/1000),
      sprintf("Performance: %.1f%% of WR", performance_pct),
      sprintf("Level: %s", performance_level),
      sep = "\n"
    )
  })
  
  # Calibration results output
  output$calibration_results <- renderText({
    results <- simulation_results()
    if (is.null(results)) return("")
    
    calibration <- results$calibration_results[[1]]
    power_profile <- results$power_profile[[1]]
    
    # Format user test info
    user_test_info <- sprintf("Your test: %.0fm at %.1f km/h (%.1f min/km)",
                             calibration$user_test$distance,
                             calibration$user_test$actual_pace_kmh,
                             calibration$user_test$actual_pace_minkm)
    
    # Validation message
    validation_msg <- calibration$validation_message
    
    # Performance predictions
    key_predictions <- calibration$predictions[calibration$predictions$distance %in% c(100, 150, 200, 250, 300), ]
    predictions_text <- paste(sapply(1:nrow(key_predictions), function(i) {
      pred <- key_predictions[i, ]
      sprintf("  %3.0fm: %.1fs (%.1f km/h)", 
              pred$distance, pred$predicted_time, pred$predicted_pace_kmh)
    }), collapse = "\n")
    
    paste(
      user_test_info,
      validation_msg,
      "",
      sprintf("Based on W' capacity (%.1f kJ), predicted performances:", power_profile$w_prime/1000),
      predictions_text,
      sep = "\n"
    )
  })
  
  # Velocity plot
  output$velocity_plot <- renderPlotly({
    results <- simulation_results()
    if (is.null(results)) return(NULL)
    
    user_inputs <- results$user_inputs[[1]]
    
    p <- ggplot(results, aes(x = time, y = velocity)) +
      geom_line(color = "#2E86AB", size = 1.2) +
      geom_hline(yintercept = user_inputs$max_velocity, 
                 linetype = "dashed", color = "#A23B72", alpha = 0.7) +
      geom_hline(yintercept = user_inputs$critical_speed, 
                 linetype = "dashed", color = "#F18F01", alpha = 0.7) +
      labs(title = "Velocity Throughout Race", 
           x = "Time (s)", 
           y = "Velocity (m/s)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  # Power plot
  output$power_plot <- renderPlotly({
    results <- simulation_results()
    if (is.null(results)) return(NULL)
    
    power_profile <- results$power_profile[[1]]
    
    p <- ggplot(results, aes(x = time, y = power)) +
      geom_line(color = "#C73E1D", size = 1.2) +
      geom_hline(yintercept = power_profile$max_power, 
                 linetype = "dashed", color = "#A23B72", alpha = 0.7) +
      geom_hline(yintercept = power_profile$critical_power, 
                 linetype = "dashed", color = "#F18F01", alpha = 0.7) +
      labs(title = "Power Output Strategy", 
           x = "Time (s)", 
           y = "Power (W)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  # === PERTURBATION ANALYSIS SERVER LOGIC ===
  
  # Reactive values to store analysis results
  analysis_results <- reactiveVal(NULL)
  
  # Run perturbation analysis automatically when simulation completes
  observe({
    
    # Check if simulation results exist
    if (is.null(simulation_results())) {
      analysis_results(NULL)  # Clear previous analysis
      return()
    }
    
    # Get user inputs from simulation results
    user_inputs <- simulation_results()$user_inputs[[1]]
    
    # Show progress notification
    showNotification("Running pace sufficiency analysis...", 
                    type = "message", duration = 2, id = "analysis_progress")
    
    # Run pace sufficiency analysis automatically
    tryCatch({
      sufficiency_analysis <- analyze_pace_sufficiency(user_inputs, simulation_results(), input$speed_unit)
      
      # Store results
      analysis_results(sufficiency_analysis)
      
      removeNotification("analysis_progress")
      showNotification("Pace sufficiency analysis completed!", 
                      type = "message", duration = 2)
      
    }, error = function(e) {
      removeNotification("analysis_progress")
      showNotification(paste("Analysis failed:", e$message), 
                      type = "error", duration = 5)
      analysis_results(NULL)
    })
  })
  
  
  # Generate pace sufficiency analysis report
  output$analysis_report <- renderText({
    
    req(analysis_results())
    
    # Get analysis data
    test_pace <- analysis_results()$test_pace
    race_avg_velocity <- analysis_results()$race_avg_velocity
    pace_deficit <- analysis_results()$pace_deficit
    sufficiency_ratio <- analysis_results()$sufficiency_ratio
    verdict <- analysis_results()$verdict
    limiting_factors <- analysis_results()$limiting_factors
    target_improvements <- analysis_results()$target_improvements
    baseline <- analysis_results()$baseline
    
    # Status symbol
    status_symbol <- switch(verdict$status,
                           "SUFFICIENT" = "✅",
                           "MARGINAL" = "⚠️",
                           "INSUFFICIENT" = "❌")
    
    # Start report
    report_text <- c(
      "PACE SUFFICIENCY ANALYSIS",
      "",
      sprintf("Test Pace Target: %.2f m/s (%.1f km/h)", test_pace, test_pace * 3.6),
      sprintf("Race Average Pace: %.2f m/s (%.1f km/h)", race_avg_velocity, race_avg_velocity * 3.6),
      sprintf("Achievement Ratio: %.1f%% of target", sufficiency_ratio),
      "",
      sprintf("%s VERDICT: %s", status_symbol, verdict$status),
      sprintf("%s", verdict$message)
    )
    
    # Add pace deficit info if insufficient
    if (pace_deficit > 0.05) {
      report_text <- c(report_text,
        "",
        sprintf("Pace Deficit: %.2f m/s (%.1f km/h slower than target)", 
                pace_deficit, pace_deficit * 3.6)
      )
      
      # Show limiting factors
      if (length(limiting_factors) > 0) {
        report_text <- c(report_text,
          "",
          "LIMITING FACTORS:"
        )
        
        factor_labels <- list(
          critical_speed = "Critical Speed (Aerobic Base)",
          max_velocity = "Maximum Velocity (Sprint Capacity)",
          test_pace = "Test Pace Power Sustainability",
          w_prime = "Anaerobic Capacity (W')"
        )
        
        priority <- 1
        for (factor_name in names(limiting_factors)) {
          factor_data <- limiting_factors[[factor_name]]
          label <- factor_labels[[factor_name]] %||% factor_name
          
          report_text <- c(report_text,
            sprintf("%d. %s", priority, label),
            sprintf("   %s", factor_data$factor)
          )
          
          if (!is.null(factor_data$current) && !is.null(factor_data$needed)) {
            report_text <- c(report_text,
              sprintf("   Current: %.2f | Needed: %.2f", 
                      factor_data$current, factor_data$needed)
            )
          }
          report_text <- c(report_text, "")
          priority <- priority + 1
        }
      }
      
      # Show target improvements
      if (length(target_improvements) > 1) {  # More than just message
        report_text <- c(report_text,
          "RECOMMENDED IMPROVEMENTS:"
        )
        
        param_labels <- list(
          critical_speed = "Critical Speed",
          max_velocity = "Maximum Velocity",
          body_weight = "Body Weight"
        )
        
        param_units <- list(
          critical_speed = "m/s",
          max_velocity = "m/s",
          body_weight = "kg"
        )
        
        for (param_name in names(target_improvements)) {
          if (param_name == "message") next
          
          improvement_data <- target_improvements[[param_name]]
          label <- param_labels[[param_name]] %||% param_name
          unit <- param_units[[param_name]] %||% ""
          
          # Use formatted strings for pretty display (mm:ss for pace)
          if (!is.null(improvement_data$current_formatted) && !is.null(improvement_data$unit)) {
            display_unit <- improvement_data$unit
            current_str <- improvement_data$current_formatted
            target_str <- improvement_data$target_formatted
            improvement_str <- improvement_data$improvement_formatted
          } else if (!is.null(improvement_data$current_display) && !is.null(improvement_data$unit)) {
            # Fallback to numeric display
            display_unit <- improvement_data$unit
            current_str <- sprintf("%.2f", improvement_data$current_display)
            target_str <- sprintf("%.2f", improvement_data$target_display)
            improvement_str <- sprintf("%+.2f", improvement_data$improvement_display)
          } else {
            # Fallback for non-velocity parameters like body weight
            display_unit <- unit
            current_str <- sprintf("%.2f", improvement_data$current)
            target_str <- sprintf("%.2f", improvement_data$target)
            improvement_str <- sprintf("%+.2f", improvement_data$improvement)
          }
          
          report_text <- c(report_text,
            sprintf("%d. %s", improvement_data$priority, label),
            sprintf("   Current: %s %s | Target: %s %s", 
                    current_str, display_unit,
                    target_str, display_unit),
            sprintf("   Improvement: %s %s", improvement_str, display_unit),
            sprintf("   Rationale: %s", improvement_data$rationale),
            ""
          )
        }
      }
      
    } else {
      # Sufficient case
      report_text <- c(report_text,
        "",
        "🎯 Your physiological parameters are SUFFICIENT!",
        "No improvements needed to sustain your test pace over 500m."
      )
    }
    
    return(paste(report_text, collapse = "\n"))
  })
  
  
  # Download handler for pace sufficiency analysis report
  output$download_analysis <- downloadHandler(
    filename = function() {
      paste0("K1_Pace_Sufficiency_Analysis_", Sys.Date(), ".txt")
    },
    content = function(file) {
      
      req(analysis_results())
      
      # Get analysis data
      test_pace <- analysis_results()$test_pace
      race_avg_velocity <- analysis_results()$race_avg_velocity
      pace_deficit <- analysis_results()$pace_deficit
      sufficiency_ratio <- analysis_results()$sufficiency_ratio
      verdict <- analysis_results()$verdict
      limiting_factors <- analysis_results()$limiting_factors
      target_improvements <- analysis_results()$target_improvements
      baseline <- analysis_results()$baseline
      
      # Status symbol
      status_symbol <- switch(verdict$status,
                             "SUFFICIENT" = "✅",
                             "MARGINAL" = "⚠️",
                             "INSUFFICIENT" = "❌")
      
      # Create comprehensive report
      report_lines <- c(
        "===================================================",
        "K1 500M KAYAK PACE SUFFICIENCY ANALYSIS REPORT",
        "===================================================",
        "",
        paste("Generated:", Sys.time()),
        "",
        "PACE COMPARISON:",
        sprintf("• Test Pace Target: %.2f m/s (%.1f km/h)", test_pace, test_pace * 3.6),
        sprintf("• Race Average Pace: %.2f m/s (%.1f km/h)", race_avg_velocity, race_avg_velocity * 3.6),
        sprintf("• Achievement Ratio: %.1f%% of target", sufficiency_ratio),
        if (pace_deficit > 0) sprintf("• Pace Deficit: %.2f m/s (%.1f km/h slower)", pace_deficit, pace_deficit * 3.6) else NULL,
        "",
        sprintf("%s VERDICT: %s", status_symbol, verdict$status),
        sprintf("%s", verdict$message),
        "",
        "CURRENT PARAMETER VALUES:",
        sprintf("• Maximum Velocity: %.2f m/s", baseline$inputs$max_velocity),
        sprintf("• Test Pace: %.2f m/s", baseline$inputs$max_distance_pace),
        sprintf("• Critical Speed: %.2f m/s", baseline$inputs$critical_speed),
        sprintf("• Body Weight: %.1f kg", baseline$inputs$body_weight),
        sprintf("• Max Distance at Test Pace: %.0f m", baseline$inputs$max_distance),
        if(!is.na(baseline$inputs$pace_400m)) sprintf("• 400m Training Pace: %.2f m/s", baseline$inputs$pace_400m) else NULL,
        "",
        "RACE PERFORMANCE:",
        sprintf("• Race Time: %.1f seconds", baseline$time),
        sprintf("• Average Power: %.0f watts", baseline$power),
        sprintf("• Average Velocity: %.2f m/s", baseline$velocity)
      )
      
      # Add limiting factors if any
      if (length(limiting_factors) > 0) {
        report_lines <- c(report_lines,
          "",
          "LIMITING FACTORS:"
        )
        
        factor_labels <- list(
          critical_speed = "Critical Speed (Aerobic Base)",
          max_velocity = "Maximum Velocity (Sprint Capacity)", 
          test_pace = "Test Pace Power Sustainability",
          w_prime = "Anaerobic Capacity (W')"
        )
        
        priority <- 1
        for (factor_name in names(limiting_factors)) {
          factor_data <- limiting_factors[[factor_name]]
          label <- factor_labels[[factor_name]] %||% factor_name
          
          report_lines <- c(report_lines,
            sprintf("%d. %s", priority, label),
            sprintf("   %s", factor_data$factor)
          )
          
          if (!is.null(factor_data$current) && !is.null(factor_data$needed)) {
            report_lines <- c(report_lines,
              sprintf("   Current: %.2f | Needed: %.2f | Deficit: %.2f", 
                      factor_data$current, factor_data$needed, factor_data$deficit)
            )
          }
          
          priority <- priority + 1
        }
      }
      
      # Add target improvements if any
      if (length(target_improvements) > 1) {  # More than just message
        report_lines <- c(report_lines,
          "",
          "RECOMMENDED IMPROVEMENTS:"
        )
        
        param_labels <- list(
          critical_speed = "Critical Speed",
          max_velocity = "Maximum Velocity",
          body_weight = "Body Weight"
        )
        
        param_units <- list(
          critical_speed = "m/s",
          max_velocity = "m/s", 
          body_weight = "kg"
        )
        
        for (param_name in names(target_improvements)) {
          if (param_name == "message") next
          
          improvement_data <- target_improvements[[param_name]]
          label <- param_labels[[param_name]] %||% param_name
          unit <- param_units[[param_name]] %||% ""
          
          # Use formatted strings for pretty display (mm:ss for pace)
          if (!is.null(improvement_data$current_formatted) && !is.null(improvement_data$unit)) {
            display_unit <- improvement_data$unit
            current_str <- improvement_data$current_formatted
            target_str <- improvement_data$target_formatted
            improvement_str <- improvement_data$improvement_formatted
          } else if (!is.null(improvement_data$current_display) && !is.null(improvement_data$unit)) {
            # Fallback to numeric display
            display_unit <- improvement_data$unit
            current_str <- sprintf("%.2f", improvement_data$current_display)
            target_str <- sprintf("%.2f", improvement_data$target_display)
            improvement_str <- sprintf("%+.2f", improvement_data$improvement_display)
          } else {
            # Fallback for non-velocity parameters like body weight
            display_unit <- unit
            current_str <- sprintf("%.2f", improvement_data$current)
            target_str <- sprintf("%.2f", improvement_data$target)
            improvement_str <- sprintf("%+.2f", improvement_data$improvement)
          }
          
          report_lines <- c(report_lines,
            sprintf("%d. %s", improvement_data$priority, label),
            sprintf("   Current: %s %s", current_str, display_unit),
            sprintf("   Target: %s %s", target_str, display_unit),
            sprintf("   Improvement: %s %s", improvement_str, display_unit),
            sprintf("   Rationale: %s", improvement_data$rationale),
            ""
          )
        }
      }
      
      # Add conclusion
      if (pace_deficit <= 0.05) {
        conclusion <- "🎯 Your physiological parameters are SUFFICIENT to sustain your test pace!"
      } else {
        conclusion <- sprintf("🎯 Focus on the improvements above to achieve your target pace of %.2f m/s", test_pace)
      }
      
      report_lines <- c(report_lines,
        "",
        "CONCLUSION:",
        conclusion,
        "",
        "KEY INSIGHTS:",
        "• Pace sufficiency analysis shows whether your physiology supports your target",
        "• Focus on limiting factors for maximum impact", 
        "• Re-run analysis after making improvements",
        "",
        "===================================================",
        "Report generated by K1 500m Kayak Race Simulator",
        "==================================================="
      )
      
      writeLines(report_lines, file)
    },
    contentType = "text/plain"
  )
}

# Run the application
shinyApp(ui = ui, server = server)