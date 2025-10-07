# K1 500m Kayak Race Simulation

A personalized physiological simulation and pace sufficiency analysis tool for K1 500m kayak racing.

## Features

- **Personalized Race Simulation**: Input your physiological parameters to simulate your race performance
- **Pace Sufficiency Analysis**: Determines if your max velocity, critical speed, and W' can sustain your test pace over 500m
- **Multiple Input Units**: Supports km/h, min/km (mm:ss format), and m/s for speed inputs
- **Interactive Visualizations**: Real-time power and velocity curves during your simulated race
- **Detailed Reports**: Download comprehensive analysis reports with improvement recommendations

## Usage

### Online Version
The app is deployed and accessible at: [Your Render URL will go here]

### Input Parameters
- **Maximum Velocity**: Your peak sprint speed
- **Test Pace**: The pace you can maintain for your test distance
- **Critical Speed**: Your aerobic threshold speed
- **Body Weight**: Used for power-to-weight calculations
- **Test Distance**: Distance you can maintain at test pace
- **400m Training Pace** (Optional): Additional calibration data

### Analysis Output
- **Verdict**: SUFFICIENT / MARGINAL / INSUFFICIENT for sustaining test pace over 500m
- **Limiting Factors**: Identifies which physiological parameters constrain performance
- **Improvement Recommendations**: Specific targets for performance enhancement

## Technical Details

Built with R Shiny, featuring:
- Real-time physiological modeling with differential equations
- Power-based race simulation using critical power model
- Dynamic pace sufficiency analysis
- Responsive web interface with multiple unit formats

## Local Development

```bash
# Run locally
Rscript app.R

# Or with Docker
docker build -t k1-simulator .
docker run -p 3838:3838 k1-simulator
```

## Deployment

The app is containerized and ready for deployment on platforms like:
- Render.com (current deployment)
- Heroku
- Google Cloud Run
- Any Docker-compatible platform# Trigger deploy
