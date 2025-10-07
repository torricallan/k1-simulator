# Use the official Rocker Shiny image as base
# This includes R, Shiny Server, and common system dependencies
FROM rocker/shiny:latest

# Maintainer information
LABEL maintainer="K1 Simulation App"
LABEL description="K1 500m Kayak Race Simulation Shiny App"

# Install system dependencies needed for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'plotly', 'DT', 'deSolve', 'ggplot2', 'gridExtra'), repos='https://cran.rstudio.com/')"

# Remove default Shiny app and create app directory
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/

# Copy application files
COPY app.R .
COPY k1_shiny_app.R .
COPY personalized_k1_simulation.R .

# Set permissions
RUN chmod -R 755 /srv/shiny-server/

# Expose port 3838 (standard Shiny port)
EXPOSE 3838

# For Render deployment, we'll also expose 5000 and use a custom start command
EXPOSE 5000

# Create a custom start script that can run on different ports
RUN echo '#!/bin/bash\nPORT=${PORT:-3838}\nR -e "shiny::runApp(appDir=\"/srv/shiny-server\", host=\"0.0.0.0\", port=as.numeric(Sys.getenv(\"PORT\", \"$PORT\")))"' > /start.sh
RUN chmod +x /start.sh

# Use the custom start script
CMD ["/start.sh"]