# Personalized K1 500m Kayak Race Simulation
# User inputs: max velocity, max distance at race pace, critical aerobic speed, body weight
# Converts velocity parameters to power outputs using drag equations

library(deSolve)
library(ggplot2)
library(gridExtra)

# Base kayak drag parameters (will be adjusted for individual)
BASE_DRAG_PARAMS <- list(
  water_density = 1000,        # kg/m³ - freshwater
  base_drag_coeff = 0.08,      # Base drag coefficient for reference athlete
  base_ref_area = 0.06,        # m² - Base reference area for 78kg athlete  
  base_weight = 78.5           # kg - Reference weight for calibration
)

# === USER INPUT FUNCTIONS ===

# Function to get user inputs with validation
get_user_inputs <- function() {
  cat("=== Personalized K1 500m Race Simulation ===\n")
  cat("Please enter your physiological parameters:\n\n")
  
  # Get user inputs with defaults
  max_velocity <- readline(prompt = "Maximum velocity (m/s) [default 5.6]: ")
  if (max_velocity == "") max_velocity <- 5.6 else max_velocity <- as.numeric(max_velocity)
  
  max_distance_pace <- readline(prompt = "Max distance pace (m/s) [default 5.2]: ")
  if (max_distance_pace == "") max_distance_pace <- 5.2 else max_distance_pace <- as.numeric(max_distance_pace)
  
  max_distance <- readline(prompt = "Maximum distance at specified pace (m) [default 200]: ")
  if (max_distance == "") max_distance <- 200 else max_distance <- as.numeric(max_distance)
  
  critical_speed <- readline(prompt = "Critical aerobic speed (m/s) [default 4.8]: ")
  if (critical_speed == "") critical_speed <- 4.8 else critical_speed <- as.numeric(critical_speed)
  
  body_weight <- readline(prompt = "Body weight (kg) [default 75]: ")
  if (body_weight == "") body_weight <- 75 else body_weight <- as.numeric(body_weight)
  
  pace_400m <- readline(prompt = "400m training piece pace (m/s) [optional, press Enter to skip]: ")
  if (pace_400m == "") pace_400m <- NA else pace_400m <- as.numeric(pace_400m)
  
  # Validation
  if (max_velocity <= critical_speed) {
    stop("Error: Maximum velocity must be greater than critical aerobic speed")
  }
  
  if (max_distance_pace <= critical_speed) {
    stop("Error: Max distance pace must be greater than critical aerobic speed")
  }
  
  if (max_distance_pace >= max_velocity) {
    stop("Error: Max distance pace must be less than maximum velocity")
  }
  
  if (max_distance < 100 || max_distance > 500) {
    warning("Max distance seems unusual (typical range: 100-500m)")
  }
  
  if (!is.na(pace_400m)) {
    if (pace_400m <= critical_speed) {
      warning("400m pace should be above critical speed for W' calculation")
    }
    if (pace_400m >= max_velocity) {
      warning("400m pace should be below maximum velocity")
    }
  }
  
  return(list(
    max_velocity = max_velocity,
    max_distance_pace = max_distance_pace,
    max_distance = max_distance, 
    critical_speed = critical_speed,
    body_weight = body_weight,
    pace_400m = pace_400m
  ))
}

# === VELOCITY-TO-POWER CONVERSION FUNCTIONS ===

# Calculate drag parameters based on body weight
calculate_drag_params <- function(body_weight) {
  # Adjust drag coefficient and reference area based on weight
  # Heavier athletes have more wetted surface area
  weight_ratio <- body_weight / BASE_DRAG_PARAMS$base_weight
  
  # Scale reference area with weight (approximately)
  adjusted_ref_area <- BASE_DRAG_PARAMS$base_ref_area * weight_ratio^(2/3)
  
  # Slightly adjust drag coefficient for body size
  adjusted_drag_coeff <- BASE_DRAG_PARAMS$base_drag_coeff * (1 + 0.1 * (weight_ratio - 1))
  
  return(list(
    drag_coefficient = adjusted_drag_coeff,
    reference_area = adjusted_ref_area,
    water_density = BASE_DRAG_PARAMS$water_density
  ))
}

# Convert velocity to power using drag equation: P = 0.5 * Cd * ρ * A * v³
velocity_to_power <- function(velocity, drag_params) {
  with(drag_params, {
    power <- 0.5 * drag_coefficient * water_density * reference_area * velocity^3
    return(power)
  })
}

# Calculate Anaerobic Work Capacity (W') from max distance at specified pace
# Uses 3-minute all-out test methodology adapted for single-point estimation
calculate_anaerobic_work_capacity <- function(max_velocity, critical_speed, max_distance_pace, max_distance, drag_params, pace_400m = NA) {
  # Power at different velocities
  max_power <- velocity_to_power(max_velocity, drag_params)
  critical_power <- velocity_to_power(critical_speed, drag_params)
  test_pace_power <- velocity_to_power(max_distance_pace, drag_params)
  
  # Time to cover max_distance at the specified pace
  time_at_test_pace <- max_distance / max_distance_pace
  
  # Model the test as if it were an all-out effort with exponential depletion
  # Based on 3-minute all-out test methodology: W' is work above CP for depletion phase
  
  if (test_pace_power <= critical_power) {
    warning("Test pace power is at or below critical power. W' calculation may be unreliable.")
    w_prime <- 15000  # Default 15kJ for sustainable efforts
  } else {
    # Conservative W' calculation based on demonstrated test performance
    # Avoid overestimating W' when test performance doesn't justify it
    
    # Base W' calculation: work performed above critical power during test
    excess_test_power <- test_pace_power - critical_power
    
    # CRITICAL FIX: Prevent W' from increasing as critical power decreases
    # Athletes with poor aerobic fitness shouldn't have unlimited anaerobic capacity
    
    # Cap excess power based on physiological limits
    # Use test pace power as the reference point, not the gap to critical power
    typical_critical_power <- test_pace_power * 0.7  # Typical critical power = 70% of test pace
    max_reasonable_excess <- test_pace_power * 0.4   # Max excess = 40% of test pace power
    
    # Limit excess power to prevent unrealistic W' for poor aerobic fitness
    if (excess_test_power > max_reasonable_excess) {
      # If critical power is too low, cap the effective excess power
      capped_excess_power <- max_reasonable_excess
      cat(sprintf("Note: Capping excess power at %.0fW (was %.0fW) for realistic W' calculation\n", 
          capped_excess_power, excess_test_power))
    } else {
      capped_excess_power <- excess_test_power
    }
    
    # Conservative approach: use sustained excess power as primary indicator
    # Account for fact that athlete likely started higher and declined
    
    # Estimate work done above critical power during the test
    # Assume exponential decline from higher initial power
    initial_boost_factor <- 1.1 + min((time_at_test_pace / 120), 0.3)  # Modest initial boost
    initial_boost_factor <- min(initial_boost_factor, max_power / test_pace_power)  # Cap at max power
    
    # W' calculation based on area under power curve above critical power
    # Use trapezoidal approximation: average of initial and final excess power × time
    estimated_initial_excess <- capped_excess_power * initial_boost_factor
    estimated_final_excess <- capped_excess_power * 0.7  # Assume decline to 70% of average
    avg_excess_power <- (estimated_initial_excess + estimated_final_excess) / 2
    
    # Base W' from demonstrated work capacity
    w_prime_base <- avg_excess_power * time_at_test_pace
    
    # Apply conservative scaling based on test characteristics
    # Multi-factor W' model incorporating maximum velocity potential
    
    # MAXIMUM VELOCITY FACTOR - Key insight: Sprint capability indicates anaerobic potential
    # Athletes with higher max velocity have better anaerobic power systems
    max_velocity_power <- velocity_to_power(max_velocity, drag_params)
    
    # Sprint reserve factor: how much sprint potential above test pace
    sprint_reserve_ratio <- max_velocity / max_distance_pace  # Velocity ratio
    typical_sprint_reserve <- 1.20  # Typical athlete has 20% sprint reserve
    sprint_factor <- (sprint_reserve_ratio / typical_sprint_reserve)^1.5  # Exponential scaling
    sprint_factor <- max(0.5, min(sprint_factor, 2.5))  # Wider bounds for more impact
    
    # Anaerobic range factor: total velocity spectrum from critical to max
    anaerobic_range <- max_velocity - critical_speed
    test_anaerobic_position <- (max_distance_pace - critical_speed) / anaerobic_range
    # Higher position in anaerobic range = better demonstrated anaerobic capacity
    range_factor <- 0.7 + test_anaerobic_position * 0.6  # More sensitive
    range_factor <- max(0.6, min(range_factor, 1.5))
    
    # Power-based sprint factor: Higher max power indicates better anaerobic systems
    power_sprint_factor <- (max_velocity_power / test_pace_power)^0.8
    power_sprint_factor <- max(0.7, min(power_sprint_factor, 2.0))
    
    # Combined maximum velocity factor with enhanced scaling
    max_velocity_factor <- (sprint_factor * 0.5 + range_factor * 0.3 + power_sprint_factor * 0.2)
    max_velocity_factor <- max(0.5, min(max_velocity_factor, 2.2))  # Allow more variation
    
    # Test quality factor: how impressive is the test relative to critical power?
    # Use capped excess power for quality assessment
    effective_power_ratio <- (critical_power + capped_excess_power) / critical_power
    if (effective_power_ratio > 1.5) {
      quality_boost <- 1.1 + (effective_power_ratio - 1.5) * 0.2  # Boost for high-intensity tests
    } else {
      quality_boost <- 0.8 + (effective_power_ratio - 1.0) * 0.2  # Conservative for low-intensity tests
    }
    quality_boost <- max(0.7, min(quality_boost, 1.4))  # Tighter range
    
    # Duration factor: longer tests suggest better anaerobic capacity
    if (time_at_test_pace > 45) {
      duration_factor <- 1.1 + (time_at_test_pace - 45) / 90  # Boost for longer tests
    } else {
      duration_factor <- 0.9 + (time_at_test_pace - 30) / 45  # Conservative for short tests
    }
    duration_factor <- max(0.7, min(duration_factor, 1.6))
    
    # Distance factor: longer distances at same pace suggest higher capacity
    if (max_distance > 250) {
      distance_factor <- 1.1 + (max_distance - 250) / 200
    } else {
      distance_factor <- 0.9 + (max_distance - 150) / 150
    }
    distance_factor <- max(0.8, min(distance_factor, 1.5))
    
    # 400m pace factor: Additional training data to inform W' capacity
    pace_400m_factor <- 1.0  # Default when no 400m data
    if (!is.null(pace_400m) && !is.na(pace_400m) && length(pace_400m) > 0) {
      # 400m is a classic anaerobic test distance - provides key insight into W' capacity
      pace_400m_power <- velocity_to_power(pace_400m, drag_params)
      
      # Compare 400m performance to test distance performance
      # Better 400m pace relative to test pace suggests higher anaerobic capacity
      pace_ratio <- pace_400m / max_distance_pace
      
      if (pace_ratio > 1.0) {
        # 400m pace faster than test pace - boost W' (athlete has untapped anaerobic potential)
        pace_400m_factor <- 1.0 + (pace_ratio - 1.0) * 0.8  # Moderate boost
      } else {
        # 400m pace slower than test pace - reduce W' (test may have been optimistic)
        pace_400m_factor <- 0.9 + (pace_ratio - 0.9) * 0.5  # Gentle reduction
      }
      
      # Additional check: 400m power relative to critical power
      power_400m_ratio <- pace_400m_power / critical_power
      if (power_400m_ratio > 1.8) {
        # High 400m power indicates strong anaerobic system
        pace_400m_factor <- pace_400m_factor * 1.1
      } else if (power_400m_ratio < 1.3) {
        # Lower 400m power suggests limited anaerobic capacity
        pace_400m_factor <- pace_400m_factor * 0.95
      }
      
      # Bound the factor
      pace_400m_factor <- max(0.7, min(pace_400m_factor, 1.4))
    }
    
    # Combined W' with multi-factor scaling including 400m pace data
    w_prime <- w_prime_base * quality_boost * duration_factor * distance_factor * max_velocity_factor * pace_400m_factor
    
    # Physiologically reasonable bounds
    w_prime <- max(w_prime, 6000)   # Minimum 6 kJ
    w_prime <- min(w_prime, 30000)  # Maximum 30 kJ (more conservative upper bound)
  }
  
  return(list(
    max_power = max_power,
    critical_power = critical_power,
    test_pace_power = test_pace_power,
    w_prime = w_prime,
    time_at_test_pace = time_at_test_pace,
    estimated_peak_power = if(exists("estimated_peak_power")) estimated_peak_power else test_pace_power
  ))
}

# === W' CALIBRATION AND VALIDATION ===

# Predict performance at other distances/intensities based on W' and Critical Power
predict_performance_from_w_prime <- function(power_profile, drag_params, test_distances = c(100, 150, 200, 250, 300)) {
  
  critical_power <- power_profile$critical_power
  w_prime <- power_profile$w_prime
  
  # Calculate critical velocity from critical power
  critical_velocity <- (critical_power / (0.5 * drag_params$drag_coefficient * drag_params$water_density * drag_params$reference_area))^(1/3)
  
  predictions <- data.frame(
    distance = test_distances,
    predicted_time = NA,
    predicted_velocity = NA,
    predicted_pace_kmh = NA,
    predicted_pace_minkm = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(test_distances)) {
    distance <- test_distances[i]
    
    # Use a simple search approach rather than Newton-Raphson
    # Test velocities from slightly above critical to reasonable maximum
    
    best_velocity <- critical_velocity
    best_error <- Inf
    
    # Search range: critical velocity to ~150% of critical velocity
    velocity_range <- seq(critical_velocity * 1.02, critical_velocity * 1.5, by = 0.01)
    
    for (v_test in velocity_range) {
      power_test <- velocity_to_power(v_test, drag_params)
      
      if (power_test <= critical_power) {
        # At or below critical power - can sustain indefinitely
        time_test <- distance / v_test
        distance_achieved <- v_test * time_test  # Always equals distance
        error <- 0
      } else {
        # Above critical power - limited by W'
        excess_power <- power_test - critical_power
        time_to_exhaustion <- w_prime / excess_power
        distance_achieved <- v_test * time_to_exhaustion
        error <- abs(distance_achieved - distance)
      }
      
      if (error < best_error) {
        best_error <- error
        best_velocity <- v_test
      }
    }
    
    # Calculate final time at best velocity
    power_final <- velocity_to_power(best_velocity, drag_params)
    if (power_final <= critical_power) {
      time_final <- distance / best_velocity
    } else {
      time_final <- w_prime / (power_final - critical_power)
    }
    
    predictions$predicted_time[i] <- time_final
    predictions$predicted_velocity[i] <- best_velocity
    predictions$predicted_pace_kmh[i] <- best_velocity * 3.6
    predictions$predicted_pace_minkm[i] <- 1000 / (best_velocity * 60)
  }
  
  return(predictions)
}

# Calibrate single test input against physiological model predictions
calibrate_test_input <- function(user_inputs, drag_params) {
  
  # Calculate W' from user's single test
  power_profile <- calculate_anaerobic_work_capacity(
    user_inputs$max_velocity, 
    user_inputs$critical_speed, 
    user_inputs$max_distance_pace, 
    user_inputs$max_distance, 
    drag_params,
    user_inputs$pace_400m
  )
  
  # Predict what athlete should achieve at other distances
  predictions <- predict_performance_from_w_prime(power_profile, drag_params)
  
  # User's actual test performance
  user_test <- list(
    distance = user_inputs$max_distance,
    actual_velocity = user_inputs$max_distance_pace,
    actual_time = user_inputs$max_distance / user_inputs$max_distance_pace,
    actual_pace_kmh = user_inputs$max_distance_pace * 3.6,
    actual_pace_minkm = 1000 / (user_inputs$max_distance_pace * 60)
  )
  
  # Find closest prediction to user's test distance
  distance_match_idx <- which.min(abs(predictions$distance - user_test$distance))
  closest_prediction <- predictions[distance_match_idx, ]
  
  # Calculate discrepancies
  velocity_error <- abs(user_test$actual_velocity - closest_prediction$predicted_velocity)
  time_error <- abs(user_test$actual_time - closest_prediction$predicted_time)
  velocity_error_pct <- (velocity_error / user_test$actual_velocity) * 100
  
  # Validation flags
  is_realistic <- velocity_error_pct < 15  # Within 15% is reasonable
  
  calibration_results <- list(
    user_test = user_test,
    predictions = predictions,
    closest_prediction = closest_prediction,
    velocity_error_pct = velocity_error_pct,
    time_error = time_error,
    is_realistic = is_realistic,
    power_profile = power_profile,
    validation_message = if (is_realistic) {
      "✓ Test result is physiologically consistent"
    } else if (velocity_error_pct > 25) {
      "⚠ Test result may be inconsistent with your other parameters"
    } else {
      "⚠ Test result shows minor inconsistency - check input values"
    }
  )
  
  return(calibration_results)
}

# === PERSONALIZED POWER CURVE GENERATION ===

# W'-informed personalized power strategy based on user's actual capabilities
personalized_power_strategy <- function(t, total_time, power_profile) {
  
  # Extract power parameters
  max_power <- power_profile$max_power
  critical_power <- power_profile$critical_power
  w_prime <- power_profile$w_prime
  test_pace_power <- power_profile$test_pace_power
  
  # Physiologically realistic start power (never exceed max power)
  start_power <- max_power * 0.92    # 92% of max for explosive start
  min_power <- critical_power * 0.88    # Don't fall too far below critical
  
  # W'-informed high phase duration and power
  # Larger W' capacity allows longer high power phase and higher sustainable power
  base_high_phase_duration <- 25  # Base duration in seconds
  typical_w_prime <- 22000        # Typical elite W' around 22kJ (updated from research)
  w_prime_factor <- w_prime / typical_w_prime
  
  # Scale duration: more W' = significantly longer high phase
  # Enhanced scaling to create meaningful differences
  high_phase_duration <- base_high_phase_duration * (0.5 + 1.0 * w_prime_factor)
  high_phase_duration <- min(max(high_phase_duration, 12), 50)  # Expanded range: 12-50s
  
  # W'-informed sustainable high phase power
  # Higher W' allows higher sustainable power above test pace
  w_prime_power_factor <- 0.8 + 0.4 * w_prime_factor  # Range: 0.8 to 1.2
  base_sustainable <- test_pace_power * 1.15 * w_prime_power_factor  # W'-scaled base
  max_sustainable <- max_power * 0.85                               # Physiological ceiling
  high_phase_power <- min(base_sustainable, max_sustainable)
  
  # Ensure sustainable power is above critical power
  high_phase_power <- max(high_phase_power, critical_power * 1.1)
  
  # Phase timings
  explosive_phase <- 8     # Explosive start phase (0-8s)
  high_phase_end <- explosive_phase + high_phase_duration
  
  if (t <= explosive_phase) {
    # Phase 1: Explosive start (0-8s)
    # Smooth transition from start power to high phase power
    progress <- t / explosive_phase
    # Slight decline from start power as immediate anaerobic reserves are used
    current_power <- start_power - (start_power - high_phase_power) * progress * 0.6
    
    return(current_power)
    
  } else if (t <= high_phase_end) {
    # Phase 2: High sustainable phase (8-30s)
    # Maintain high power with gradual decline as W' is consumed
    phase_duration <- high_phase_end - explosive_phase
    progress <- (t - explosive_phase) / phase_duration
    
    # Gradual decline in high phase as anaerobic capacity is consumed
    power_decline <- (high_phase_power - critical_power) * progress * 0.3
    current_power <- high_phase_power - power_decline
    
    return(current_power)
    
  } else {
    # Phase 3: Fatigue-dominated phase (30s-finish)
    # Smooth exponential decline toward critical power as fatigue accumulates
    phase_duration <- total_time - high_phase_end
    progress <- (t - high_phase_end) / phase_duration
    
    # Starting power at end of phase 2
    phase_start_power <- high_phase_power - (high_phase_power - critical_power) * 0.3
    
    # Exponential fatigue decline - research shows continuous decline
    # Use gentler decline rate for more realistic curve
    fatigue_rate <- 0.8
    fatigue_factor <- exp(-fatigue_rate * progress)
    
    power_range <- phase_start_power - min_power
    current_power <- min_power + (power_range * fatigue_factor)
    
    return(max(current_power, min_power))
  }
}

# === SIMULATION FUNCTIONS ===

# Enhanced race dynamics with personalized parameters
personalized_race_dynamics <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    v <- state[1]
    
    # Prevent unrealistic low velocities
    if (v < 0.5) v <- 0.5
    
    # Get dynamic power from personalized strategy
    P_t <- personalized_power_strategy(t, estimated_race_time, power_profile)
    
    # Hydrodynamic drag force: Fd = 0.5 * Cd * ρ * A * v²
    drag_force <- 0.5 * drag_params$drag_coefficient * drag_params$water_density * 
                  drag_params$reference_area * v^2
    
    # Differential equation: m * dv/dt = P(t)/v - Fd
    dvdt <- (P_t/v - drag_force) / m
    
    return(list(dvdt))
  })
}

# Main personalized simulation function
run_personalized_simulation <- function(user_inputs = NULL, estimated_time = 95) {
  
  # Get user inputs if not provided
  if (is.null(user_inputs)) {
    user_inputs <- get_user_inputs()
  }
  
  cat("\n=== Processing Your Parameters ===\n")
  
  # Calculate personalized drag parameters
  drag_params <- calculate_drag_params(user_inputs$body_weight)
  
  # Calculate power profile from velocity inputs
  power_profile <- calculate_anaerobic_work_capacity(
    user_inputs$max_velocity, 
    user_inputs$critical_speed,
    user_inputs$max_distance_pace,
    user_inputs$max_distance, 
    drag_params,
    user_inputs$pace_400m
  )
  
  # Run calibration analysis
  calibration_results <- calibrate_test_input(user_inputs, drag_params)
  
  # Display calculated parameters
  cat(sprintf("Your calculated parameters:\n"))
  cat(sprintf("- Maximum Power: %.0f watts\n", power_profile$max_power))
  cat(sprintf("- Test Pace Power: %.0f watts (at %.1f m/s)\n", power_profile$test_pace_power, user_inputs$max_distance_pace))
  cat(sprintf("- Critical Power: %.0f watts\n", power_profile$critical_power))
  cat(sprintf("- Anaerobic Work Capacity (W'): %.1f kJ\n", power_profile$w_prime/1000))
  cat(sprintf("- Power-to-Weight Ratio: %.1f W/kg\n", power_profile$max_power/user_inputs$body_weight))
  cat(sprintf("- Adjusted Drag Coefficient: %.4f\n", drag_params$drag_coefficient))
  
  # Display calibration results
  cat(sprintf("\n=== Test Input Calibration ===\n"))
  cat(sprintf("Your test: %.0fm at %.1f km/h (%.1f min/km)\n", 
      calibration_results$user_test$distance,
      calibration_results$user_test$actual_pace_kmh,
      calibration_results$user_test$actual_pace_minkm))
  cat(sprintf("%s\n", calibration_results$validation_message))
  
  if (!calibration_results$is_realistic) {
    cat(sprintf("Velocity difference: %.1f%% from predicted\n", calibration_results$velocity_error_pct))
  }
  
  cat(sprintf("\nBased on your W' capacity (%.1f kJ), you should also achieve:\n", power_profile$w_prime/1000))
  key_predictions <- calibration_results$predictions[calibration_results$predictions$distance %in% c(100, 150, 200, 250, 300), ]
  for (i in 1:nrow(key_predictions)) {
    pred <- key_predictions[i, ]
    cat(sprintf("- %3.0fm in %.1fs (%.1f km/h, %.1f min/km)\n", 
        pred$distance, pred$predicted_time, pred$predicted_pace_kmh, pred$predicted_pace_minkm))
  }
  
  # Set up simulation parameters
  parameters <- list(
    m = user_inputs$body_weight,
    drag_params = drag_params,
    power_profile = power_profile,
    estimated_race_time = estimated_time
  )
  
  # Initial conditions
  initial_velocity <- 2.0  # m/s - realistic start
  initial_state <- c(v = initial_velocity)
  
  # Time sequence
  times <- seq(0, estimated_time * 1.3, by = 0.1)
  
  # Solve ODE
  solution <- ode(y = initial_state, times = times, 
                  func = personalized_race_dynamics, parms = parameters,
                  method = "lsoda")
  
  # Process results
  results <- as.data.frame(solution)
  names(results) <- c("time", "velocity")
  
  # Calculate distance and other metrics
  dt <- diff(c(0, results$time))
  results$distance <- cumsum(results$velocity * dt)
  
  # Add power profile
  results$power <- sapply(results$time, function(t) 
    personalized_power_strategy(t, estimated_time, power_profile))
  
  # Calculate energy
  results$energy <- cumsum(results$power * dt)
  
  # Find 500m completion
  race_complete_idx <- which(results$distance >= 500)[1]
  if (!is.na(race_complete_idx)) {
    results <- results[1:race_complete_idx, ]
  }
  
  # Add user inputs for reference
  results$user_inputs <- list(user_inputs)
  results$power_profile <- list(power_profile)
  results$calibration_results <- list(calibration_results)
  
  return(results)
}

# === ANALYSIS AND PLOTTING ===

# Analyze personalized performance
analyze_personalized_performance <- function(results) {
  
  user_inputs <- results$user_inputs[[1]]
  power_profile <- results$power_profile[[1]]
  
  final_time <- tail(results$time, 1)
  avg_velocity <- mean(results$velocity)
  avg_power <- mean(results$power)
  total_energy <- tail(results$energy, 1)
  
  cat("\n=== Your Personalized K1 500m Race Results ===\n")
  cat(sprintf("Race Time: %.2f seconds\n", final_time))
  cat(sprintf("Average Velocity: %.2f m/s (%.2f km/h)\n", avg_velocity, avg_velocity * 3.6))
  cat(sprintf("Average Power: %.0f watts\n", avg_power))
  cat(sprintf("Power-to-Weight: %.1f W/kg\n", avg_power / user_inputs$body_weight))
  cat(sprintf("Total Energy: %.1f kJ\n", total_energy/1000))
  
  # Performance comparison
  world_record <- 95.15
  performance_pct <- (world_record / final_time) * 100
  cat(sprintf("Performance vs World Record: %.1f%%\n", performance_pct))
  
  if (final_time <= world_record) {
    cat("🏆 WORLD RECORD PACE! Outstanding performance!\n")
  } else if (performance_pct >= 90) {
    cat("🥇 Elite level performance!\n")  
  } else if (performance_pct >= 80) {
    cat("🥈 Competitive club level performance!\n")
  } else {
    cat("🥉 Good recreational performance - room for improvement!\n")
  }
  
  return(invisible(list(
    race_time = final_time,
    avg_power = avg_power,
    performance_pct = performance_pct
  )))
}

# Plot personalized results
plot_personalized_results <- function(results) {
  
  user_inputs <- results$user_inputs[[1]]
  power_profile <- results$power_profile[[1]]
  
  # Velocity profile
  p1 <- ggplot(results, aes(x = time, y = velocity)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_hline(yintercept = user_inputs$max_velocity, linetype = "dashed", 
               color = "red", alpha = 0.7) +
    geom_hline(yintercept = user_inputs$critical_speed, linetype = "dashed", 
               color = "green", alpha = 0.7) +
    labs(title = "Your Velocity Profile", 
         x = "Time (s)", y = "Velocity (m/s)",
         subtitle = sprintf("Max: %.1f m/s, Critical: %.1f m/s", 
                           user_inputs$max_velocity, user_inputs$critical_speed)) +
    theme_minimal()
  
  # Power profile
  p2 <- ggplot(results, aes(x = time, y = power)) +
    geom_line(color = "red", linewidth = 1.2) +
    geom_hline(yintercept = power_profile$max_power, linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = power_profile$critical_power, linetype = "dashed", alpha = 0.7) +
    labs(title = "Your Power Strategy", 
         x = "Time (s)", y = "Power (W)",
         subtitle = sprintf("Max: %.0fW, Critical: %.0fW", 
                           power_profile$max_power, power_profile$critical_power)) +
    theme_minimal()
  
  # Distance progress
  p3 <- ggplot(results, aes(x = time, y = distance)) +
    geom_line(color = "green", linewidth = 1.2) +
    geom_hline(yintercept = user_inputs$max_distance, linetype = "dotted", alpha = 0.5) +
    labs(title = "Distance Progress", 
         x = "Time (s)", y = "Distance (m)") +
    theme_minimal()
  
  # Power vs velocity
  p4 <- ggplot(results, aes(x = velocity, y = power)) +
    geom_line(color = "purple", linewidth = 1.2) +
    labs(title = "Power vs Velocity Relationship", 
         x = "Velocity (m/s)", y = "Power (W)") +
    theme_minimal()
  
  # Combine plots
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# === EXAMPLE USAGE ===

# Example with default parameters (for testing)
run_example_simulation <- function() {
  cat("Running example simulation with typical elite parameters...\n")
  
  example_inputs <- list(
    max_velocity = 5.6,         # m/s - elite sprint speed
    max_distance_pace = 5.2,    # m/s - pace for the max distance test
    max_distance = 200,         # m - distance sustainable at test pace  
    critical_speed = 4.8,       # m/s - aerobic threshold speed
    body_weight = 75            # kg
  )
  
  results <- run_personalized_simulation(example_inputs, estimated_time = 95)
  analysis <- analyze_personalized_performance(results)
  plot_personalized_results(results)
  
  return(results)
}

# === PACE SUFFICIENCY ANALYSIS ===

# Analyze if user's physiological parameters can sustain their test pace over 500m
analyze_pace_sufficiency <- function(user_inputs, simulation_results, speed_unit = "ms") {
  
  cat("=== Running Pace Sufficiency Analysis ===\n")
  cat("Analyzing if your physiological parameters can sustain your test pace over 500m...\n\n")
  
  # Get key values
  test_pace <- user_inputs$max_distance_pace  # User's target test pace
  race_avg_velocity <- mean(simulation_results$velocity)  # Actual race average velocity
  race_time <- tail(simulation_results$time, 1)
  power_profile <- simulation_results$power_profile[[1]]
  
  # Calculate pace deficit (positive = insufficient)
  pace_deficit <- test_pace - race_avg_velocity
  sufficiency_ratio <- (race_avg_velocity / test_pace) * 100
  
  # Analyze limiting factors
  limiting_factors <- identify_limiting_factors(user_inputs, power_profile, pace_deficit)
  
  # Generate verdict
  verdict <- generate_pace_verdict(pace_deficit, sufficiency_ratio)
  
  # Calculate target improvements needed
  target_improvements <- calculate_target_improvements(user_inputs, power_profile, pace_deficit, speed_unit)
  
  return(list(
    test_pace = test_pace,
    race_avg_velocity = race_avg_velocity,
    pace_deficit = pace_deficit,
    sufficiency_ratio = sufficiency_ratio,
    verdict = verdict,
    limiting_factors = limiting_factors,
    target_improvements = target_improvements,
    baseline = list(
      time = race_time,
      power = mean(simulation_results$power),
      velocity = race_avg_velocity,
      inputs = user_inputs
    )
  ))
}

# Identify which physiological parameters are limiting pace sustainability
identify_limiting_factors <- function(user_inputs, power_profile, pace_deficit) {
  
  limiting_factors <- list()
  
  # Calculate theoretical sustainable pace based on each parameter
  test_pace_power <- power_profile$test_pace_power
  critical_power <- power_profile$critical_power
  max_power <- power_profile$max_power
  w_prime <- power_profile$w_prime
  
  # 1. Check if test pace power exceeds sustainable power for 500m
  # For 500m (~100s), sustainable power is between critical power and test pace power
  sustainable_500m_power <- critical_power + (w_prime / 100)  # Rough estimate
  
  if (test_pace_power > sustainable_500m_power * 1.1) {  # 10% tolerance
    limiting_factors$test_pace <- list(
      factor = "Test pace power too high",
      current = test_pace_power,
      sustainable = sustainable_500m_power,
      deficit = test_pace_power - sustainable_500m_power
    )
  }
  
  # 2. Check critical speed limitation
  critical_speed_ratio <- user_inputs$critical_speed / user_inputs$max_distance_pace
  if (critical_speed_ratio < 0.75) {  # Critical speed should be at least 75% of test pace
    limiting_factors$critical_speed <- list(
      factor = "Critical speed insufficient",
      current = user_inputs$critical_speed,
      needed = user_inputs$max_distance_pace * 0.8,  # Target 80%
      deficit = (user_inputs$max_distance_pace * 0.8) - user_inputs$critical_speed
    )
  }
  
  # 3. Check maximum velocity limitation
  max_velocity_ratio <- user_inputs$max_velocity / user_inputs$max_distance_pace
  if (max_velocity_ratio < 1.15) {  # Max velocity should be at least 15% higher than test pace
    limiting_factors$max_velocity <- list(
      factor = "Maximum velocity insufficient",
      current = user_inputs$max_velocity,
      needed = user_inputs$max_distance_pace * 1.2,  # Target 20% higher
      deficit = (user_inputs$max_distance_pace * 1.2) - user_inputs$max_velocity
    )
  }
  
  # 4. Check W' (anaerobic capacity) limitation
  w_prime_per_kg <- w_prime / user_inputs$body_weight
  if (w_prime_per_kg < 120) {  # Typical minimum for sustained high intensity
    limiting_factors$w_prime <- list(
      factor = "Anaerobic capacity (W') insufficient",
      current = w_prime_per_kg,
      needed = 150,  # Target W'/kg
      deficit = 150 - w_prime_per_kg
    )
  }
  
  return(limiting_factors)
}

# Generate pace sufficiency verdict
generate_pace_verdict <- function(pace_deficit, sufficiency_ratio) {
  
  if (pace_deficit <= -0.05 || sufficiency_ratio >= 99) {  # Actually exceeds or very close
    verdict <- list(
      status = "SUFFICIENT",
      message = "Your physiological parameters can sustain your test pace over 500m",
      color = "success"
    )
  } else if (pace_deficit <= 0.25 && sufficiency_ratio >= 93) {  # Within reasonable range
    verdict <- list(
      status = "MARGINAL",
      message = "Your parameters are close but may struggle to sustain full test pace",
      color = "warning"
    )
  } else {  # Significant deficit
    verdict <- list(
      status = "INSUFFICIENT",
      message = "Your current parameters cannot sustain your test pace over 500m",
      color = "danger"
    )
  }
  
  verdict$ratio <- sufficiency_ratio
  return(verdict)
}

# Calculate specific target improvements needed
calculate_target_improvements <- function(user_inputs, power_profile, pace_deficit, speed_unit = "ms") {
  
  if (pace_deficit <= -0.05) {
    return(list(message = "No improvements needed - current parameters are sufficient"))
  }
  
  improvements <- list()
  
  # Unit conversion functions
  ms_to_kmh <- function(ms) ms * 3.6
  ms_to_pace_decimal <- function(ms) 1000 / (ms * 60)  # m/s to decimal min/km
  
  # Convert decimal minutes to mm:ss format
  decimal_to_mmss <- function(decimal_min) {
    minutes <- floor(decimal_min)
    seconds <- round((decimal_min - minutes) * 60)
    if (seconds == 60) {
      minutes <- minutes + 1
      seconds <- 0
    }
    sprintf("%d:%02d", minutes, seconds)
  }
  
  # Function to format velocity in user's preferred unit
  format_velocity <- function(ms_value, unit_type) {
    switch(unit_type,
           "kmh" = list(value = ms_to_kmh(ms_value), unit = "km/h", formatted = sprintf("%.2f", ms_to_kmh(ms_value))),
           "pace" = {
             decimal_pace <- ms_to_pace_decimal(ms_value)
             mmss_pace <- decimal_to_mmss(decimal_pace)
             list(value = decimal_pace, unit = "min/km", formatted = mmss_pace)
           },
           "ms" = list(value = ms_value, unit = "m/s", formatted = sprintf("%.2f", ms_value))
    )
  }
  
  # Estimate improvements needed to close pace deficit
  pace_improvement_needed <- pace_deficit * 1.1  # Add 10% buffer
  
  # Critical speed improvement (most impactful for 500m)
  critical_speed_target <- user_inputs$critical_speed + pace_improvement_needed * 0.6
  current_cs <- format_velocity(user_inputs$critical_speed, speed_unit)
  target_cs <- format_velocity(critical_speed_target, speed_unit)
  
  improvements$critical_speed <- list(
    current = user_inputs$critical_speed,  # Keep in m/s for calculations
    target = critical_speed_target,        # Keep in m/s for calculations
    improvement = critical_speed_target - user_inputs$critical_speed,  # Keep in m/s
    current_display = current_cs$value,    # Numeric value for calculations
    target_display = target_cs$value,      # Numeric value for calculations
    improvement_display = target_cs$value - current_cs$value,  # Numeric difference
    current_formatted = current_cs$formatted,    # Pretty formatted string
    target_formatted = target_cs$formatted,      # Pretty formatted string
    improvement_formatted = if(speed_unit == "pace") {
      decimal_to_mmss(abs(target_cs$value - current_cs$value))
    } else {
      sprintf("%+.2f", target_cs$value - current_cs$value)
    },
    unit = current_cs$unit,
    priority = 1,
    rationale = "Improve aerobic base for better pace sustainability"
  )
  
  # Maximum velocity improvement (helps with initial pace)
  max_velocity_target <- user_inputs$max_velocity + pace_improvement_needed * 0.4
  current_mv <- format_velocity(user_inputs$max_velocity, speed_unit)
  target_mv <- format_velocity(max_velocity_target, speed_unit)
  
  improvements$max_velocity <- list(
    current = user_inputs$max_velocity,    # Keep in m/s for calculations
    target = max_velocity_target,          # Keep in m/s for calculations
    improvement = max_velocity_target - user_inputs$max_velocity,  # Keep in m/s
    current_display = current_mv$value,    # Numeric value for calculations
    target_display = target_mv$value,      # Numeric value for calculations
    improvement_display = target_mv$value - current_mv$value,  # Numeric difference
    current_formatted = current_mv$formatted,    # Pretty formatted string
    target_formatted = target_mv$formatted,      # Pretty formatted string
    improvement_formatted = if(speed_unit == "pace") {
      decimal_to_mmss(abs(target_mv$value - current_mv$value))
    } else {
      sprintf("%+.2f", target_mv$value - current_mv$value)
    },
    unit = current_mv$unit,
    priority = 2,
    rationale = "Increase sprint capacity for stronger race start"
  )
  
  # Body weight reduction (if overweight)
  if (user_inputs$body_weight > 80) {  # Typical elite kayaker weight
    weight_target <- max(75, user_inputs$body_weight - 3)
    improvements$body_weight <- list(
      current = user_inputs$body_weight,
      target = weight_target,
      improvement = weight_target - user_inputs$body_weight,  # Negative = reduction
      priority = 3,
      rationale = "Reduce body weight to improve power-to-weight ratio"
    )
  }
  
  return(improvements)
}

# Generate human-readable pace sufficiency report
generate_pace_sufficiency_report <- function(analysis_results) {
  
  cat("=== PACE SUFFICIENCY ANALYSIS REPORT ===\n\n")
  
  # Summary comparison
  cat(sprintf("Test Pace (Target): %.2f m/s (%.1f km/h)\n", 
      analysis_results$test_pace, analysis_results$test_pace * 3.6))
  cat(sprintf("Race Average Pace: %.2f m/s (%.1f km/h)\n", 
      analysis_results$race_avg_velocity, analysis_results$race_avg_velocity * 3.6))
  cat(sprintf("Pace Achievement: %.1f%% of target\n\n", analysis_results$sufficiency_ratio))
  
  # Verdict
  verdict <- analysis_results$verdict
  status_symbol <- switch(verdict$status,
                         "SUFFICIENT" = "✅",
                         "MARGINAL" = "⚠️",
                         "INSUFFICIENT" = "❌")
  
  cat(sprintf("%s %s\n", status_symbol, verdict$status))
  cat(sprintf("%s\n\n", verdict$message))
  
  # Pace deficit analysis
  if (analysis_results$pace_deficit > 0.05) {
    cat(sprintf("Pace Deficit: %.2f m/s (%.1f km/h slower than target)\n", 
        analysis_results$pace_deficit, analysis_results$pace_deficit * 3.6))
    
    # Show limiting factors
    limiting_factors <- analysis_results$limiting_factors
    if (length(limiting_factors) > 0) {
      cat("\nLIMITING FACTORS:\n")
      
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
        
        cat(sprintf("%d. %s\n", priority, label))
        cat(sprintf("   %s\n", factor_data$factor))
        
        if (!is.null(factor_data$current) && !is.null(factor_data$needed)) {
          cat(sprintf("   Current: %.2f | Needed: %.2f\n", 
              factor_data$current, factor_data$needed))
        }
        cat("\n")
        priority <- priority + 1
      }
    }
  }
  
  # Target improvements
  improvements <- analysis_results$target_improvements
  if (length(improvements) > 1) {  # More than just the "no improvements needed" message
    cat("RECOMMENDED IMPROVEMENTS:\n")
    
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
    
    for (param_name in names(improvements)) {
      if (param_name == "message") next
      
      improvement_data <- improvements[[param_name]]
      label <- param_labels[[param_name]] %||% param_name
      unit <- param_units[[param_name]] %||% ""
      
      cat(sprintf("%d. %s\n", improvement_data$priority, label))
      
      # Use formatted strings if available (with user's preferred units), otherwise fall back
      if (!is.null(improvement_data$current_formatted) && !is.null(improvement_data$unit)) {
        # Use pretty formatted strings for pace (mm:ss) or regular formatting for others
        cat(sprintf("   Current: %s %s\n", improvement_data$current_formatted, improvement_data$unit))
        cat(sprintf("   Target: %s %s\n", improvement_data$target_formatted, improvement_data$unit))
        cat(sprintf("   Improvement needed: %s %s\n", improvement_data$improvement_formatted, improvement_data$unit))
      } else if (!is.null(improvement_data$current_display) && !is.null(improvement_data$unit)) {
        # Fallback to numeric display values
        cat(sprintf("   Current: %.2f %s\n", improvement_data$current_display, improvement_data$unit))
        cat(sprintf("   Target: %.2f %s\n", improvement_data$target_display, improvement_data$unit))
        cat(sprintf("   Improvement needed: %+.2f %s\n", improvement_data$improvement_display, improvement_data$unit))
      } else {
        # Fallback for non-velocity parameters like body weight
        cat(sprintf("   Current: %.2f %s\n", improvement_data$current, unit))
        cat(sprintf("   Target: %.2f %s\n", improvement_data$target, unit))
        cat(sprintf("   Improvement needed: %+.2f %s\n", improvement_data$improvement, unit))
      }
      
      cat(sprintf("   Rationale: %s\n", improvement_data$rationale))
      cat("\n")
    }
  }
  
  # Summary
  if (analysis_results$pace_deficit <= 0.05) {
    cat("🎯 CONCLUSION: Your current physiological parameters are sufficient to sustain your test pace over 500m!\n")
  } else {
    cat(sprintf("🎯 CONCLUSION: Focus on the above improvements to achieve your target pace of %.2f m/s\n", 
        analysis_results$test_pace))
  }
}

# === EXAMPLE SIMULATION ===

# Run example simulation
cat("=== Personalized K1 500m Simulation System ===\n")
cat("This system converts your velocity parameters to power outputs\n")
cat("Running example with typical elite parameters...\n\n")

example_results <- run_example_simulation()