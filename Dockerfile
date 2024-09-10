FROM rstudio/r-base:4.2.0-focal

# Update and install necessary packages in one layer
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    jq librsvg2-2 libpq-dev libssl-dev libv8-dev \
    libcurl4-openssl-dev libsasl2-dev odbc-postgresql \
    gdal-bin libgdal-dev libxml2-dev libglpk-dev \
    wget gzip && \
    rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV CPLUS_INCLUDE_PATH=/usr/include/gdal \
    C_INCLUDE_PATH=/usr/include/gdal \
    RENV_VERSION=1.0.7

COPY renv.lock /renv.lock

# Install renv and restore packages in one layer
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))" && \
    R -e 'renv::restore()'

# Copy assets, static files, and scripts in one layer
COPY www/ /app/www
COPY *.R /app/

# Expose the Shiny app port
EXPOSE 3838

# Set the working directory
WORKDIR /app

# Start the Shiny app
CMD ["Rscript", "app.R"]