FROM rstudio/r-base:4.2.0-focal

# Update and install necessary packages in one layer
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    jq librsvg2-2 libpq-dev libssl-dev libv8-dev \
    libcurl4-openssl-dev libsasl2-dev odbc-postgresql \
    gdal-bin libgdal-dev libxml2-dev libglpk-dev \
    wget gzip && \
    rm -rf /var/lib/apt/lists/*

# Configure ODBC for PostgreSQL
RUN echo "[postgresql]\nDriver = /usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so" >> /etc/odbcinst.ini

# Set environment variables
ENV CPLUS_INCLUDE_PATH=/usr/include/gdal \
    C_INCLUDE_PATH=/usr/include/gdal \
    RENV_VERSION=1.0.7

# Install renv and restore packages in one layer
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" && \
    R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')" && \
    R -e 'renv::restore()'

# Download, unzip, and rename the SQLite database in one layer
RUN mkdir -p /app/data && \
    wget -O /app/data/employees.db.gz https://raw.githubusercontent.com/fracpete/employees-db-sqlite/master/employees_db-full-1.0.6.db.gz && \
    gunzip /app/data/employees.db.gz

# Copy assets, static files, and scripts in one layer
COPY www/ /app/www
COPY data/ /app/data
COPY *.R /app/

# Expose the Shiny app port
EXPOSE 3838

# Set the working directory
WORKDIR /app

# Start the Shiny app
CMD ["Rscript", "main.R"]
