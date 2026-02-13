# choose a Dockerhub base image with R, Shiny Server, and tidyverse packages
FROM rocker/shiny-verse:latest

# 1. System deps commonly needed by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev git curl \
    pkg-config make g++ \
    libudunits2-dev \
    libgdal-dev gdal-bin \
    libgeos-dev \
    libproj-dev proj-bin proj-data \
    libsqlite3-dev \
  && rm -rf /var/lib/apt/lists/*

# 2. R packages for VS Code integration + app runtime deps
RUN R -q -e 'install.packages(c(\
  "rstudioapi","languageserver",\
  "shiny","ggplot2","ggiraph","bslib","dplyr","stringr"\
  ), repos="https://cloud.r-project.org")'

# 3. Install Node.js LTS from NodeSource
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g npm@latest
    
# 4. Install Claude Code, Codex CLI, and Gemini CLI
RUN curl -fsSL https://claude.ai/install.sh | bash

# Install using npm
RUN  npm install -g @openai/codex

# Install Gemini CLI
RUN npm install -g @google/gemini-cli

# 5. Give shiny passwordless sudo for updating Claude Code from VS Code
RUN echo 'shiny ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers && \
    rm -rf /var/lib/apt/lists/*

# Expose Shiny server port
EXPOSE 3838
