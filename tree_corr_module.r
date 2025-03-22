tree_corr_ui <- function(id) {
    ns <- NS(id)
    dashboardPage(
        dashboardHeader(title = "Climate Data Dashboard"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Climate Data",
                    tabName = "climate_data", icon = icon("cloud"), startExpanded = TRUE,
                    selectInput(
                        ns("climate_df"),
                        NULL,
                        choices = c(
                            "Water Flow" = "MONTHLY_MEAN_DISCHARGE",
                            "Water Level" = "MONTHLY_MEAN_LEVEL",
                            "Total Snowfall" = "TOTAL_SNOWFALL",
                            "Total Precipitation" = "TOTAL_PRECIPITATION",
                            "Cooling Degree Days" = "COOLING_DEGREE_DAYS",
                            "Heating Degree Days" = "HEATING_DEGREE_DAYS",
                            "Mean Temperature" = "MEAN_TEMPERATURE",
                            "Normal Sunshine" = "NORMAL_SUNSHINE"
                        ),
                        selected = "MONTHLY_MEAN_DISCHARGE"
                    )
                ),
                menuItem("Station Filtering",
                    tabname = "station_filter", icon = icon("filter"), startExpanded = FALSE,
                    checkboxInput(ns("valid_station"),
                        "Show sites with 30 years of consistent data",
                        value = TRUE
                    ),
                    checkboxInput(ns("month_const"),
                        "Show sites with data for all months",
                        value = TRUE
                    )
                ),
                menuItem("Season Filtering",
                    tabname = "season_filter", startExpanded = TRUE,
                    radioButtons(ns("season"), NULL,
                        choices = c(
                            "Annual",
                            "Spring",
                            "Summer",
                            "Warm Season",
                            "Water Year"
                        ),
                        selected = "Annual"
                    )
                ),
                menuItem("Time Offset",
                    tabname = "year_offset", startExpanded = TRUE,
                    icon = icon("calendar-days"),
                    radioButtons(ns("Offset"), NULL,
                        choices = c(
                            "None",
                            "One Year Back",
                            "Two Year Back"
                        ),
                        selected = "None"
                    )
                )
            )
        ),
        dashboardBody(
            fluidPage(
                hr(),
                fluidRow(
                    column(
                        9,
                        box(leafletOutput(ns("proxy_map")),
                            title = "Climate Proxy Map", width = NULL
                        )
                    ),
                    column(
                        3,
                        valueBoxOutput(ns("obs"), width = NULL),
                        valueBoxOutput(ns("t_stat"), width = NULL),
                        valueBoxOutput(ns("correlation"), width = NULL),
                        valueBoxOutput(ns("p_value"), width = NULL)
                    )
                ),
                tabBox(
                    tabPanel("Time Series", plotlyOutput(ns("proxy_plot"))),
                    tabPanel("Scatter Plot", plotlyOutput(ns("proxy_scatter"))),
                    width = 12
                ),
                tabBox(
                    tabPanel(
                        "Static Reconstructions",
                        plotOutput(ns("climate_recon_static"))
                    ),
                    tabPanel("Dynamic Reconstructions", plotOutput(ns("climate_recon_dynamic"))),
                    width = 12
                )
            )
        )
    )
}

tree_corr_server <- function(id, site_info, table_data, detrend_methods, future_climate) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns






        #### Reactive Functions#######################################################
        ##################################################################

        ## create a list of URLs to make an API call for that fit within the selected timze zone

        url_list_create_data <- reactive({
            req(last_year())
            url_list <- list()
            df <- table_data()
            last_year <- df$year[last_year()]


            proxy_select <- input$climate_df
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






        # chronology dataframe of detrended_df() object
        chron_df <- reactive({
            chron_df <- detrend_df() %>%
                chron(prewhiten = input$pre_white) %>%
                rownames_to_column("YEAR") %>%
                mutate(YEAR = as.integer(as.character(YEAR)))

            if (input$show_subsample_annotation) {
                chron_df <- chron_df %>%
                    filter(YEAR >= sss_yr_cutoff())
            }
            chron_df
        })


        # get the last year in tree site data
        last_year <- reactive({
            as.integer(last_year <- tail(rownames(table_data()), n = 1))
        })



        #### Hold the information about the markers on the map ( color, station name, etc)
        marker_properties <- reactiveValues(
            station = NULL
        )


        # future_climate <- reactive({
        #     make_async_api_request(url_list_create_data())
        # })


        # pulling the climate that has been requested in parrallel
        climate_select <- reactive({
            dt <- future_climate()

            resolve(dt)


            df <- lapply(dt, get_data)

            filter_list <- Filter(Negate(is.null), df)
            dt <- rbindlist(filter_list, use.names = T)

            if (input$climate_df == "MONTHLY_MEAN_LEVEL" | input$climate_df == "MONTHLY_MEAN_DISCHARGE") {
                dt <- dt %>%
                    separate(DATE, into = c("LOCAL_YEAR", "LOCAL_MONTH"), sep = "-") %>%
                    mutate(
                        LOCAL_YEAR = as.integer(LOCAL_YEAR),
                        LOCAL_MONTH = as.integer(LOCAL_MONTH)
                    )
            }
            df <- dt %>%
                select(y, x, STATION_NAME, LOCAL_YEAR, LOCAL_MONTH, input$climate_df) %>%
                rename(
                    MONTHLY_MEAN = input$climate_df, MONTH = LOCAL_MONTH, YEAR = LOCAL_YEAR,
                    LATITUDE = y, LONGITUDE = x
                ) %>%
                filter(!is.na(MONTHLY_MEAN)) %>%
                mutate(MONTHLY_MEAN = as.numeric(MONTHLY_MEAN))

            if (input$month_const) {
                df <- df %>%
                    group_by(STATION_NAME, YEAR) %>%
                    filter(all(1:12 %in% MONTH)) %>%
                    ungroup()
            }


            if (input$valid_station) {
                detrend_data <- detrend_df() %>%
                    rownames_to_column("year") %>%
                    summarise(start_year = min(year), end_year = max(year))

                df_filter <- df %>%
                    group_by(YEAR, STATION_NAME) %>%
                    summarize(
                        MONTHLY_MEAN = mean(MONTHLY_MEAN),
                        na.rm = TRUE, .groups = "drop"
                    ) %>%
                    arrange(STATION_NAME, YEAR) %>%
                    group_by(STATION_NAME, grp = cumsum(c(1, diff(YEAR) != 1))) %>%
                    filter(n() >= 30) %>%
                    ungroup() %>%
                    group_by(STATION_NAME) %>%
                    summarize(
                        overlap_years = sum(YEAR >= detrend_data$start_year &
                            YEAR <= detrend_data$end_year), .groups = "drop"
                    ) %>%
                    filter(overlap_years >= 30)


                df <- df %>%
                    filter(STATION_NAME %in% df_filter$STATION_NAME)
            }

            df_dist <- df %>%
                distinct(STATION_NAME, .keep_all = TRUE)


            marker_properties$station <- lapply(
                unique(df_dist$STATION_NAME),
                function(station) station
            )

            df
        })%>%
        bindCache(future_climate())

        observeEvent(rwl(), {
            # Get current names from rwl()
            current_names <- names(rwl())


            if (!identical(series_list$current_names, current_names)) {
                series_list$current_names <- current_names
            }
        })


        detrend_df <- reactive({
            series_list <- series_list$current_names
            combined_df <- data.frame()

            for (series_name in series_list) {
                detrend_method <- detrend_methods[[series_name]]
                detrend_df <- detrend.series(
                    y = rwl()[, series_name],
                    y.name = series_name,
                    method = detrend_method,
                    return.info = T,
                    make.plot = F
                )


                combined_df <- cbind.fill(combined_df, detrend_df$series)
            }
            rownames(combined_df) <- rownames(rwl())
            as.data.frame(combined_df)
        })



        ### take the table data and convert it to a proper rwl object
        rwl <- reactive({
            rwl <- table_data() %>%
                pivot_wider(names_from = series, values_from = value) %>%
                mutate(year = as.integer(year)) %>%
                arrange(year) %>%
                column_to_rownames(var = "year") %>%
                dplyr::select(-site_code) %>%
                as.rwl()

            if (input$strip_eps) {
                rwl <- rwl %>% strip.rwl()
            }
            return(rwl)
        })


        offset_year <- reactive({
            if (input$Offset == "None") {
                month_offset <- 0
            } else if (input$Offset == "One Year Back") {
                month_offset <- 12
            } else if (input$Offset == "Two Year Back") {
                month_offset <- 24
            }
        })




        proxy_df <- reactive({
            df <- climate_select()

            offset_year <- offset_year()


            ## This will offset the data by the appropiate year based on the user selection
            if (offset_year != 0) {
                df <- df %>%
                    mutate(
                        MONTH = lag(MONTH, n = offset_year),
                        MONTHLY_MEAN = lag(MONTHLY_MEAN, n = offset_year)
                    )



                year_offset <- unique(df[!complete.cases(df), ]$YEAR)


                df <- df %>%
                    filter(YEAR != year_offset)
            }


            ## This will filter the data based on the season selected by the user
            if (input$season != "Annual") {
                month_range <- list(
                    Spring = 3:5,
                    Summer = 6:8,
                    "Warm Season" = 4:9,
                    "Water Year" = -10:9
                )
                df <- df %>%
                    filter(MONTH %in% month_range[[input$season]])
            } else {
                df <- df
            }

            df
        })


        ### Finding the release years (abnormal spike in growth for the tree)
        release_years <- reactive({
            rwl <- rwl()

            resp <- absoluteIncrease(rwl, m1 = 15, m2 = 15, buffer = 10, length = 5)
        })



        # stations with correlation data
        stations <- reactive({
            corr_list <- find_best_correlation(proxy_df(), chron_df())
        }) %>%
            bindCache(proxy_df())

        # top 5 stations by correlation data
        best_stations <- reactive({
            best_station <- stations()$station[1:5]

            # function to set marker color based on station
            set_marker_color <- function(station) {
                best_station <<- best_station
                if (station %in% best_station) {
                    return("green")
                } else {
                    return("red")
                }
            }

            # code to set marker color for each station
            marker_properties$color <- sapply(marker_properties$station, set_marker_color)

            best_station
        })



        df_cor_mat <- reactive({
            creat_pal(stations())
        })



        unfilt_dist <- reactive({
            unfilt_dist <- unfilt_proxy() %>%
                distinct(STATION_NUMBER, .keep_all = T)

            marker_properties$station <- lapply(
                unfilt_dist$STATION_NUMBER,
                function(station) station
            )

            unfilt_dist
        }) %>%
            bindCache(unfilt_proxy())






        ############ Reactive Values#############################################################

        # tracking which markers have been selected
        proxy_markers <- reactiveValues(id = NULL)


        # create a reactive value for the selected climate proxy
        # Create reactive values
        rv_markers <- reactiveValues()

        series_list <- reactiveValues()



        # Use observe to update reactive values within a reactive context
        observe({
            site_info_data <- site_info()
            rv_markers$id <- site_info_data$id
            rv_markers$lat <- site_info_data$lat
            rv_markers$lng <- site_info_data$lng
        })



        #### Functions##################################################################################################################

        # function to get the data from the API using future workers
        make_async_api_request <- function(url) {
            futures <-
                lapply(url, function(url) {
                    print(nbrOfFreeWorkers())
                    future({
                        response <- GET(url)
                    })
                })
        }



        # function to retrieve future objects
        get_data <- function(response) {
            response <- value(response)
            if (status_code(response) == 200) {
                # Ensure the response content is a character string
                response_content <- content(response, "text", encoding = "UTF-8")
                df <- fread(input = response_content)
                return(df)
            } else {
                print("Failed to retrieve data")
                return(NULL)
            }
            df
        }





        # function that will find the correlations for all climate proxies and store
        find_best_correlation <- function(df_proxy, df_rwi) {
            # Placeholder for best correlation result
            df_proxy <- setDT(df_proxy)
            df_rwi <- setDT(df_rwi)

            df_proxy$MONTHLY_MEAN <- as.numeric(df_proxy$MONTHLY_MEAN)

            precalculated_means <- df_proxy[, .(mean = mean(MONTHLY_MEAN)), by = .(STATION_NAME, YEAR)]
            unique_stations <- unique(df_proxy$STATION_NAME)

            # Initialize the result list outside of the loop to avoid growing an object inside it
            results_list <- vector("list", length(unique_stations))

            names(results_list) <- unique_stations

            for (station in unique_stations) {
                proxy_data_for_station <- precalculated_means[STATION_NAME == station]

                # Join with RWI data by YEAR
                combined_data <- df_rwi[proxy_data_for_station, on = "YEAR"]

                if (nrow(combined_data) == 0) next

                # Calculate correlation
                current_correlation <- cor(combined_data$mean,
                    combined_data$std, # assuming 'std' is in df_rwi
                    use = "complete.obs",
                    method = "pearson"
                )

                if (is.na(current_correlation)) next

                results_list[[station]] <- data.table(station = station, corr = current_correlation)
            }


            df_cor <- rbindlist(results_list)


            df_cor[order(df_cor$corr, decreasing = T), ]
        }


        # proxy sites that were selected and converts to a time series for climate reconstruction based on averages across the selected sites
        proxy_select <- reactive({
            proxy_df <- climate_select() %>%
                group_by(STATION_NAME, YEAR)

            if (length(proxy_markers$id) > 0) {
                proxy_df <- proxy_df[proxy_df$STATION_NAME %in% proxy_markers$id, ]
            }

            proxy_df <- proxy_df %>%
                group_by(YEAR, MONTH) %>%
                summarize(MEAN = mean(MONTHLY_MEAN, na.rm = T), .groups = "drop")

            proxy_df <- proxy_df %>%
                rename(year = YEAR, month = MONTH) %>%
                mutate(group = cumsum(c(1, diff(year)) > 1)) %>%
                add_count(group) %>%
                filter(n == max(n)) %>%
                select(-group, -n)

            data.frame(proxy_df)
        })






        ######### OUTPUTS##############################################################################################################



        # create the leaflet map
        output$proxy_map <- renderLeaflet({
            climate_df <- climate_select()

            climate_df <- climate_df %>%
                distinct(STATION_NAME, .keep_all = T)

            dendr_df <- dendro_data() %>%
                select(site_name, code)


            proxy_map <- leaflet() %>%
                addTiles("https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png") %>%
                addCircleMarkers(
                    data = climate_df,
                    lng = ~LONGITUDE,
                    lat = ~LATITUDE,
                    fillColor = "red",
                    fillOpacity = 1,
                    radius = 5,
                    color = "black",
                    weight = 1,
                    label = ~ paste0("Station Name:", STATION_NAME),
                    group = "Base"
                ) %>%
                addAwesomeMarkers(
                    icon = awesomeIcons(
                        icon = "tree",
                        library = "fa",
                        markerColor = "lightblue"
                    ),
                    layerId = rv_markers$id,
                    lat = rv_markers$lat,
                    lng = rv_markers$lng,
                    label = paste(
                        "Site Name:",
                        dendr_df[dendr_df$code == rv_markers$id, "site_name"]
                    ),
                    markerOptions(zIndexOffset = 1000)
                ) %>%
                addDrawToolbar(
                    polylineOptions = FALSE, polygonOptions = FALSE,
                    circleOptions = FALSE, rectangleOptions = TRUE,
                    circleMarkerOptions = FALSE,
                    markerOptions = FALSE,
                    targetGroup = "draw",
                    singleFeature = TRUE
                    # ,
                    # editOptions = editToolbarOptions(
                    #     edit = FALSE,
                    #     selectedPathOptions = selectedPathOptions()
                    # )
                ) %>%
                addLayersControl(
                    baseGroups = c("Base", "Top 5 Correlation", "Correlation Matrix"),
                    options = layersControlOptions(
                        collapsed = FALSE,
                        autoZIndex = TRUE
                    )
                ) %>%
                addEasyButton(easyButton(
                    icon = "fa-eye-slash",
                    title = "Unselect All Markers",
                    onClick = JS("function(btn, map) {

            Shiny.setInputValue('tree_app-unselect_btn', Math.random());
          }")
                )) %>%
                onRender(
                    "function(el, x) {
                var map = this;

                map.on('layeradd', function(e) {
                  if (e.layer && e.layer.options && e.layer.options.group === 'selected') {
                    var selectedGroup = map.layerManager.getLayerGroup('selected');
                    if (selectedGroup) {
                      selectedGroup.bringToFront();
                    }
                  }
                });


                map.on('baselayerchange', function (e) {
            var selectedGroup = map.layerManager.getLayerGroup('selected');
                    if (selectedGroup) {
                      selectedGroup.bringToFront();
                    };

                });
            }"
                )

            proxy_map
        })

        
        observe({
            dendr_df <- dendro_data() %>%
                select(site_name, code)


            climate_df <- climate_select()

            climate_df <- climate_df %>%
                distinct(STATION_NAME, .keep_all = T)

            
            if (!is.null(proxy_markers$id)){user_selected <- climate_df[climate_df$STATION_NAME %in% proxy_markers$id]
            } else {
                user_selected <- NULL}


            cor_df <- df_cor_mat()


            pal <- cor_df$pal

            df <- cor_df$df


            best_stations <- best_stations()


            color_df <- data.frame(station = climate_df$STATION_NAME, color = marker_properties$color)

            df_merge <- merge(climate_df, color_df, by.x = "STATION_NAME", by.y = "station")


            leafletProxy("proxy_map") %>%
                clearGroup("Correlation Matrix") %>%
                clearGroup("Top 5 Correlation") %>%
                clearGroup("selected") %>%
                addCircleMarkers(
                    data = df,
                    lng = ~LONGITUDE,
                    lat = ~LATITUDE,
                    fillColor = ~color,
                    fillOpacity = 1,
                    radius = 5,
                    color = "black",
                    weight = 1,
                    label = ~ paste0("Station Number:", station),
                    group = "Correlation Matrix"
                ) %>%
                addCircleMarkers(
                    data = df_merge,
                    lng = ~LONGITUDE,
                    lat = ~LATITUDE,
                    fillColor = ~color,
                    fillOpacity = 1,
                    radius = 5,
                    color = "black",
                    weight = 1,
                    label = ~ paste0("Station Number:", STATION_NAME),
                    group = "Top 5 Correlation"
                ) %>%
                onRender(
                    "function(el, x) {
                var map = this;

                map.on('layeradd', function(e) {
                  if (e.layer && e.layer.options && e.layer.options.group === 'selected') {
                    var selectedGroup = map.layerManager.getLayerGroup('selected');
                    if (selectedGroup) {
                      selectedGroup.bringToFront();
                    }
                  }
                });


                map.on('baselayerchange', function (e) {
            var selectedGroup = map.layerManager.getLayerGroup('selected');
                    if (selectedGroup) {
                      selectedGroup.bringToFront();
                    };

                });
            }"
                )


                if(!is.null(user_selected)){
                    leafletProxy("proxy_map") %>%
                        addCircleMarkers(
                            data = user_selected,
                            lng = ~LONGITUDE,
                            lat = ~LATITUDE,
                            fillColor = "black",
                            fillOpacity = 1,
                            radius = 5,
                            color = "black",
                            weight = 1,
                            label = ~ paste0("Station Number:", STATION_NAME),
                            group = "selected"
                        )
                }
        })






        # create line graph of time series for climate proxy and RWI
        output$proxy_plot <- renderPlotly({
            detrend_df <- detrend_df()

            df_pivot <- chron_df()

            # release_years <- release_years()
            # all_releases_year_levels <- levels(release_years$years_list_total$AllReleasesYear)

            # all_releases_year_levels <- as.integer(all_releases_year_levels)

            df_proxy <- proxy_df()

            df_proxy <- if (length(proxy_markers$id) > 0) {
                df_proxy[df_proxy$STATION_NAME %in% proxy_markers$id, ]
            } else {
                df_proxy
            }


            df_filter <- df_proxy %>%
                group_by(YEAR) %>%
                summarize(mean = mean(MONTHLY_MEAN, na.rm = T))


            max_primary <- max(df_filter$mean, na.rm = TRUE)
            max_secondary <- max(df_pivot$std, na.rm = TRUE)
            scaling_factor <- max_primary / max_secondary
            df_pivot$mean <- df_pivot$std * scaling_factor

            df_pivot$hover_text <- paste(
                "Year:",
                df_pivot$YEAR, "<br>STD:", round(df_pivot$std, 2)
            )

            df_filter$hover_text <- paste(
                "Year:",
                df_filter$YEAR, "<br>Mean:", round(df_filter$mean, 2)
            )

            p <- plot_ly() %>%
                # Add line for RWI
                add_lines(
                    data = df_pivot, x = ~YEAR, y = ~mean, name = "RWI",
                    line = list(color = "blue"), text = ~hover_text, hoverinfo = "text"
                ) %>%
                add_lines(
                    data = df_filter, x = ~YEAR, y = ~mean, name = "Climate Proxy",
                    line = list(color = "red"), text = ~hover_text, hoverinfo = "text"
                ) %>%
                # Set axis titles
                layout(
                    yaxis = list(title = "Mean"),
                    xaxis = list(title = "Year")
                ) %>%
                # Optionally, configure the secondary axis if needed
                layout(yaxis2 = list(
                    title = "STD rwl", overlaying = "y", side = "right",
                    scaleanchor = "y", scaleratio = 1 / scaling_factor
                ))

            # for (i in 1:length(all_releases_year_levels)) {
            #     p <- p %>%
            #         add_trace(
            #             x = c(all_releases_year_levels[i], all_releases_year_levels[i]),
            #             y = c(0, max_primary),
            #             type = "scatter",
            #             mode = "lines",
            #             line = list(dash = "dash", color = "black"), showlegend = FALSE,
            #             text = paste("Year:", all_releases_year_levels[i]),
            #             hoverinfo = "text"
            #         )
            # }

            # p <- p %>%
            #     layout(
            #         updatemenus = list(
            #             list(
            #                 type = "buttons",
            #                 direction = "down",
            #                 x = 1.165,
            #                 y = .8,
            #                 buttons = list(
            #                      list(
            #                         method = "restyle",
            #                         args = list("visible", c(TRUE, TRUE, rep(FALSE, length(p$x$attrs) - 2))),
            #                         label = "Hide Release Years"
            #                     ),
            #                     list(
            #                         method = "restyle",
            #                         args = list("visible", rep(TRUE, length(p$x$attrs))),
            #                         label = "Show Release Years"
            #                     )
            #                 ),
            #                 active = 0
            #             )
            #         )
            #     )
            p
            
        })


        proxy_rwi_combo <- reactive({
            df_proxy <- proxy_df()

            df_proxy <-
                if (length(proxy_markers$id) > 0) {
                    df_proxy[df_proxy$STATION_NAME %in% proxy_markers$id, ]
                } else {
                    df_proxy
                }

            df_proxy <- df_proxy %>%
                group_by(YEAR) %>%
                summarize(mean = mean(MONTHLY_MEAN, na.rm = T))

            df_rwi <- chron_df()

            df_merge <- left_join(df_rwi, df_proxy, by = "YEAR")
        })

        # create scatter plot of time series for climate proxy and RWI
        output$proxy_scatter <- renderPlotly({
            df_merge <- proxy_rwi_combo()

            p <- df_merge %>%
                ggplot() +
                geom_point(aes(x = std, y = mean))

            p <- ggplotly(p)
        })





        # event that occurs when map is clicked
        observeEvent(input$proxy_map_marker_click, {
            click_data <- input$proxy_map_marker_click

            click_data$LATITUDE <- substr(click_data$lat, 1, 6) %>%
                as.numeric()

            click_data$LONGITUDE <- substr(click_data$lng, 1, 6) %>%
                as.numeric()


            df <- climate_select() %>%
                distinct(STATION_NAME, .keep_all = T) %>%
                mutate(LATITUDE = as.numeric(substr(LATITUDE, 1, 6))) %>%
                mutate(LONGITUDE = as.numeric(substr(LONGITUDE, 1, 6))) %>%
                filter(LATITUDE == click_data$LATITUDE & LONGITUDE == click_data$LONGITUDE) %>%
                select(STATION_NAME)



            if (!is.null(click_data)) {
                if (df$STATION_NAME %in% proxy_markers$id) {
                    # If the marker is already selected, unselect it
                    proxy_markers$id <- proxy_markers$id[proxy_markers$id != df$STATION_NAME]
                    station_index <- which(marker_properties$station == df$STATION_NAME)

                    color <- marker_properties$color[station_index]
                } else {
                    # If the marker is not selected, select it
                    proxy_markers$id <- c(proxy_markers$id, df$STATION_NAME)
                    color <- "black"
                }
                # Change the color of the clicked marker to the color underneath
                leafletProxy("proxy_map") %>%
                    addCircleMarkers(
                        lng = click_data$lng,
                        lat = click_data$lat,
                        fillColor = color,
                        radius = 5,
                        opacity = 1,
                        color = "black",
                        fillOpacity = 1,
                        weight = 1,
                        group = "selected",
                        options = markerOptions(zIndexOffset = 1000)
                    )
            }
        })



        # what happens when new tree site is selected is changed
        observeEvent(rv_markers$id,
            {
                proxy_markers$id <- NULL



                # Reset markers to unselected state
                leafletProxy("proxy_map") %>%
                clearShapes()%>%
                    clearGroup("selected")
            },
            ignoreInit = T
        )


#when a different climate proxy is chosen
        observeEvent(input$climate_df,
            {
                proxy_markers$id <- NULL

                # Reset markers to unselected state
                leafletProxy("proxy_map") %>%
                clearShapes()%>%
                    clearGroup("selected")
            },
            ignoreInit = T
        )

        # If unselect button is chosen then wipe all selections and reprint the map as original
        observeEvent(input$unselect_btn, {
            proxy_markers$id <- NULL




            df_merge <- climate_select() %>%
                distinct(STATION_NAME, .keep_all = T)

            df <- color_proxy()
            # Reset markers to unselected state
            leafletProxy("proxy_map") %>%
            clearGroup("selected")%>%
                clearShapes()
      
        })


        stat_test <- reactive({
            df_proxy <- proxy_df()

            df_proxy <- if (length(proxy_markers$id) > 0) {
                df_proxy[df_proxy$STATION_NAME %in% proxy_markers$id, ]
            } else {
                df_proxy
            }

            df_proxy <- df_proxy %>%
                group_by(YEAR) %>%
                summarize(mean = mean(MONTHLY_MEAN, na.rm = T))

            df_rwi <- chron_df()

            df_merge <- merge(df_rwi, df_proxy)
            test_result <- cor.test(df_merge$mean, df_merge$std, method = "pearson", use = "complete.obs")

            correlation_coefficient <- test_result$estimate
            p_value <- test_result$p.value
            t_statistic <- test_result$statistic
            n_observations <- sum(complete.cases(df_merge$mean, df_merge$std))

            return(list(correlation_coefficient, p_value, t_statistic, n_observations))
        })


        output$obs <- renderValueBox({
            req(proxy_markers$id)
            stat_test <- stat_test()
            valueBox(
                value = stat_test[[4]],
                subtitle = "Number of Observations"
            )
        })

        output$t_stat <- renderValueBox({
            req(proxy_markers$id)

            stat_test <- stat_test()
            valueBox(
                value = round(stat_test[[3]], 4),
                subtitle = "T-statistic"
            )
        })


        output$correlation <- renderValueBox({
            req(proxy_markers$id)

            stat_test <- stat_test()
            if (stat_test[[1]] > 0) {
                color <- "green"
            } else {
                color <- "red"
            }

            valueBox(
                value = round(stat_test[[1]], 4),
                subtitle = "Correlation Coefficient",
                color = color
            )
        })

        output$p_value <- renderValueBox({
            req(proxy_markers$id)

            stat_test <- stat_test()
            if (stat_test[[2]] < 0.05) {
                color <- "green"
            } else {
                color <- "red"
            }

            valueBox(
                value = round(stat_test[[2]], 4),
                subtitle = "P-value",
                color = color
            )
        })




        creat_pal <- function(stations) {
            color_gradient <- colorRampPalette(c("red", "white", "green"))

            df <- stations

            min_corr <- min(df$corr)
            max_corr <- max(df$corr)

            df$normalized_corr <- (df$corr - min_corr) / (max_corr - min_corr)

            n_colors <- 100
            palette <- color_gradient(n_colors)


            df$color <- palette[as.integer(df$normalized_corr * (n_colors - 1)) + 1]

            proxy_df <- proxy_df() %>%
                distinct(STATION_NAME, .keep_all = TRUE)

            df <- merge(df, proxy_df, by.x = "station", by.y = "STATION_NAME")

            pal <- colorNumeric(
                palette = palette,
                domain = df$corr
            )

            marker_properties$color <- setNames(df$color, df$station)

            return(list(df = df, pal = pal))
        }



        color_proxy <- reactive({
            station_numbers <- character()
            color <- character()

            points_df <- climate_select() %>%
                select(STATION_NAME, LATITUDE, LONGITUDE) %>%
                distinct()

            # Iterate over marker_properties to populate the vectors
            for (station_number in names(marker_properties$color)) {
                station_numbers <- c(station_numbers, station_number)
                color <- c(color, marker_properties$color[[station_number]])
            }

            df <- data.frame(STATION_NAME = station_numbers, color = color)
            df <- merge(points_df, df, by = "STATION_NAME")
        })

        # React to draw:created events to find selected points
        observeEvent(input$proxy_map_draw_new_feature, {
            shape <- input$proxy_map_draw_new_feature

            coords <- shape$geometry$coordinates[[1]]

            points_df <- climate_select() %>%
                select(STATION_NAME, LATITUDE, LONGITUDE) %>%
                distinct()

            # Determine which points are inside the polygon
            selected <- points_df[as.logical(point.in.polygon(
                points_df$LONGITUDE,
                points_df$LATITUDE,
                unlist(lapply(coords, function(c) c[1])),
                unlist(lapply(coords, function(c) c[2]))
            )), ]

            df <- color_proxy()




            proxy_markers$id <- NULL
            proxy_markers$id <- c(proxy_markers$id, selected$STATION_NAME)
            color <- "black"

            leafletProxy("proxy_map") %>%
                clearGroup("selected") %>%
                clearShapes() %>%
                addPolygons( # Add the new shape
                    lng = unlist(lapply(coords, function(c) c[1])),
                    lat = unlist(lapply(coords, function(c) c[2])),
                    color = "blue",
                    weight = 2,
                    opacity = 1,
                    fillOpacity = .2,
                    group = "polygon_select"
                ) %>%
                addCircleMarkers(
                    lng = selected$LONGITUDE,
                    lat = selected$LATITUDE,
                    fillColor = color,
                    layerId = selected$STATION_NAME,
                    radius = 5,
                    opacity = 1,
                    color = "black",
                    fillOpacity = 1,
                    weight = 1,
                    group = "selected",
                    options = markerOptions(zIndexOffset = 1000)
                )
        })



        # create a plot of the climate reconstruction

        output$climate_recon_static <- renderPlot({
            req(proxy_markers$id)
            proxy_select <- proxy_select()
            chron <- chron_df()
            rownames(chron) <- chron$YEAR
            chron$YEAR <- NULL

            month_range <- list(
                Annual = 1:12,
                Spring = 3:5,
                Summer = 6:8,
                "Warm Season" = 4:9,
                "Water Year" = -10:9
            )

            selection <- month_range[[input$season]]


            plot(dcc(
                chrono = chron, climate = proxy_select,
                selection = selection,
                method = "correlation",
                dynamic = "static"
            ))
        })

        output$climate_recon_dynamic <- renderPlot({
            req(proxy_markers$id)
            proxy_select <- proxy_select()
            chron <- chron_df()

            rownames(chron) <- chron$YEAR
            chron$YEAR <- NULL

            month_range <- list(
                Annual = 1:12,
                Spring = 3:5,
                Summer = 6:8,
                "Warm Season" = 4:9,
                "Water Year" = -10:9
            )

            selection <- month_range[[input$season]]



            plot(dcc(
                chrono = chron, climate = proxy_select,
                selection = selection,
                method = "correlation",
                dynamic = "moving"
            ))
        })
    })
}
