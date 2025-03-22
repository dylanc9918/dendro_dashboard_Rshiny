# Base R Shiny image
FROM rocker/shiny:4.4.0


RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libmysqlclient21  \
    iputils-ping

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

## app folder
COPY /shiny_app ./app



ENV R_CONFIG_ACTIVE=production

RUN R -e "install.packages(c('plyr', 'dplR', 'writexl', 'readxl', 'ggplot2', 'openxlsx', 'tidyr', 'dplyr', 'tidyverse', 'cowplot', 'ggExtra', 'PNWColors', 'DBI', 'shiny', 'RColorBrewer', 'plotly', 'leaflet', 'RODBC', 'leaflet.extras', 'htmlwidgets', 'shinyWidgets', 'shinyjs', 'sp', 'rlang', 'profvis', 'pool', 'odbc', 'data.table', 'treeclim', 'shinydashboard', 'gt', 'httr', 'future', 'furrr', 'promises', 'future.apply', 'RcppSimdJson', 'crew', 'progressr', 'TRADER', 'RMariaDB', 'DT', 'shinyFeedback'))"



# expose port
EXPOSE 3800


# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3800)"]