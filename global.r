library(plyr)
library(dplR)
library(writexl)
library(readxl)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(dplyr)
library(tidyverse)
library(cowplot)
library(ggExtra)
library(PNWColors)
library(DBI)
library(shiny)
library(RColorBrewer)
library(plotly)
library(leaflet)
library(RODBC)
library(leaflet.extras)
library(leaflet.providers)
library(htmlwidgets)
library(shinyWidgets)
library(shinyjs)
library(sp)
library(rlang)
library(profvis)
library(pool)
library(odbc)
library(data.table)
library(treeclim)
library(shinydashboard)
library(gt)
library(httr)
library(future)
library(furrr)
library(promises)
library(future.apply)
library(RcppSimdJson)
library(crew)
library(progressr)
library(TRADER)
library(RMariaDB)
library(DT)
library(shinyFeedback)
library(parallel)


cbind.fill <- function(...) {
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function(x) {
        rbind(x, matrix(, n - nrow(x), ncol(x)))
    }))
}

config <- config::get(file = "shiny_app/config.yml")


# options(future.debug = TRUE)
plan(multisession)

# # Source module files
source("modules/tree_site_map_module.R")
source("modules/tree_detrend_module.R")
source("modules/tree_corr_module.R")


## Database Connections ########################################################
con_dendro <- dbPool(
    drv = RMariaDB::MariaDB(),
    user = config$site_info$db_user,
    password = config$site_info$db_password,
    dbname = config$site_info$db_name,
    host = config$site_info$db_host,
    port = config$site_info$db_port
)



raw_rwi <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = config$raw_data_default$db_name,
    host = config$raw_data_default$db_host,
    user = config$raw_data_default$db_user,
    password = config$raw_data_default$db_password,
    port = config$raw_data_default$db_port
)



MDBPATH <- "C:\\Users\\Admin\\OneDrive\\Documents\\work\\Hydat.mdb"

## Establish connection
con_proxy <- odbcConnectAccess2007(MDBPATH)

## API URL
api_url <- "https://api.weather.gc.ca/collections"
#####################################################################
detrend_methods <- reactiveValues(series_name = NULL)


## Reactive Functions ##########################################################
# pull all dendrosite data for site names and codes
dendro_data <- reactive({
    query <- paste0("SELECT * FROM ", config$site_info$tbl_name)
    df <- dbGetQuery(con_dendro, query)
    setDF(df)
})

# info about the selected site
site_info <- reactiveVal(NULL)


table_data <- reactive({
    query <- sprintf(
        "SELECT * FROM rw_data WHERE site_code = '%s'",
        site_info()$id
    )
    con <- localCheckout(raw_rwi)
    dbGetQuery(con, query)
})

#######################################################
make_async_api_request <- function(url) {
    futures <-
        lapply(url, function(url) {
            print(nbrOfFreeWorkers())
            future({
                response <- GET(url)
            })
        })
}

future_climate <- reactiveVal(NULL)

last_year <- reactive({
    row_value <- as.integer(last_year <- tail(rownames(table_data()), n = 1))

    last_year_value <- .subset2(table_data(), "year")[row_value]
})


url_list_create_data <- reactive({
    req(last_year())
    url_list <- list()
    df <- table_data()
    last_year <- last_year()


    proxy_select <- "MONTHLY_MEAN_DISCHARGE"
    limit <- 50000
    total_data_call <- 1000000
    if (proxy_select == "MONTHLY_MEAN_LEVEL" | proxy_select == "MONTHLY_MEAN_DISCHARGE") {
        base_url <- "https://api.weather.gc.ca/collections/hydrometric-monthly-mean/items?f=csv"

        proxy_url <- paste0(base_url, "&limit=", limit, "&properties=", proxy_select, ",DATE,STATION_NAME")

        date_cutoff <- paste0("&datetime=../", last_year, "-01-01")

        remove_null <- paste0("properties.", proxy_select, "%3A*")

        full_url <- paste0(proxy_url, date_cutoff, "&q=", remove_null)


        for (offset in seq(0, total_data_call, by = limit)) {
            offset_str <- formatC(offset, format = "f", digits = 0)
            url <- paste0(full_url, "&offset=", offset_str)
            url_list <- append(url_list, url)
        }
    } else {
        base_url <- "https://api.weather.gc.ca/collections/climate-monthly/items?f=csv"

        proxy_url <- paste0(base_url, "&limit=", limit, "&properties=", proxy_select, ",LOCAL_YEAR,LOCAL_MONTH,STATION_NAME")

        date_cutoff <- paste0("&datetime=../", last_year, "-01-01")

        remove_null <- paste0("properties.", proxy_select, "%3A*")

        full_url <- paste0(proxy_url, date_cutoff, "&q=", remove_null)


        for (offset in seq(0, total_data_call, by = limit)) {
            offset_str <- formatC(offset, format = "f", digits = 0)
            url <- paste0(full_url, "&offset=", offset_str)
            url_list <- append(url_list, url)
        }
    }
    url_list
})



## close the connections once the app is closed
onStop(function() {
    poolClose(con_dendro)
    poolClose(raw_rwi)
})
