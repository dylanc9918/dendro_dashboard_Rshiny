tree_detrend_ui <- function(id) {
    ns <- NS(id)
    shinyjs::useShinyjs()
    dashboardPage(
        dashboardHeader(title = "Tree Detrend Module"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Select Data",
                    tabName = "select_data", icon = icon("table"), startExpanded = FALSE,
                    selectInput(ns("select_table"),
                        label = NULL,
                        choices = c(
                            "Ringwidth" = "rw_data",
                            "Earlywood" = "ew_data",
                            "Latewood" = "lw_data"
                        ),
                        selected = "Ringwidth"
                    )
                ),
                menuItem("Select Site",
                    tabName = "select_site", icon = icon("map-marker"), startExpanded = FALSE,
                    selectInput(ns("selected_site"),
                        label = NULL,
                        selected = NULL,
                        choices = NULL
                    )
                ),
                menuItem("Select Detrending Method",
                    tabName = "select_method", icon = icon("cogs"), startExpanded = TRUE,
                    selectInput(ns("detrend_method"),
                        label = NULL,
                        choices = c(
                            "Cubic Spline" = "Spline",
                            "Mod Negative Exponential" = "ModNegExp",
                            "Mean" = "Mean",
                            "Autoregressive model" = "Ar",
                            "Friedman super smoother" = "Friedman",
                            "Modified Hugershoff" = "ModHugershoff",
                            "Age dependent spline" = "AgeDepSpline"
                        ),
                        selected = "ModNegExp"
                    )
                ),
                menuItem("Signal Cleanup Options",
                    tabName = "options", icon = icon("sliders"), startExpanded = TRUE,
                    checkboxInput(ns("pre_white"), "Pre Whiten Chronology", value = TRUE),
                    checkboxInput(ns("strip_eps"), "Strip EPS", value = FALSE),
                    checkboxInput(ns("show_subsample_annotation"), "Show Sub-Sample Cutoff", value = FALSE)
                )
            )
        ),
        dashboardBody(
            fluidRow(
                tabBox(
                    tabPanel(
                        plotlyOutput(ns("detrended_plot")),
                        title = "Detrended Plot"
                    ),
                    tabPanel(
                        div(class = "plotly-plot", plotlyOutput(ns("raw_plot"))),
                        title = "Individual Detrended Series Plot"
                    ),
                    width = 9
                ),
                box(dataTableOutput(ns("detrend_report")),
                    width = 3,
                    title = "Detrend Report"
                )
            ),
            fluidRow(box(DTOutput(ns("series_detrend_table")), width = 9)),
            tags$script(src = "custom.js"),
            tags$script(paste0("DetrendSeriesChange('", ns(""), "')"))
        )
    )
}




tree_detrend_server <- function(id, site_info, detrend_methods) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns


        # reactive values
        warn_list <- reactiveValues()
        series_list <- reactiveValues()
        select_site <- reactiveVal(NULL)





        ## loading the selected site on the previous tab or current selection by user from drop down

        observeEvent(site_info()$id, {
            isolate({
                select_site(site_info()$id)

                table_choices <- total_data()

                updateSelectInput(session, "selected_site",
                    selected = isolate(select_site()), # Use isolate to avoid triggering reactivity
                    choices = sort(unique(table_choices$site_code))
                )
            })
        })

        observeEvent(input$selected_site,
            {
                select_site(input$selected_site)
            },
            ignoreInit = TRUE
        )



        # Observe any changes in the rwl() and update the stored sites to reactive value series_list
        observeEvent(rwl(),
            {
                # Get current names from rwl()
                current_names <- names(rwl())

                ## update the warn_list
                for (name in current_names) {
                    warn_list[[name]] <- NULL
                }

                for (name in names(warn_list)) {
                    warn_list[[name]] <- NULL
                }


                ## update the detrend methods
                for (series in current_names) {
                    detrend_methods[[series]] <- input$detrend_method
                }
                # Check if they are different from the stored ones
                # to minimize unnecessary updates
                if (!identical(series_list$current_names, current_names)) {
                    series_list$current_names <- current_names
                }
            },
            priority = 1
        )

        observe({
            series_list <- series_list$current_names
            rwl <- rwl()


            sapply(series_list, function(series_name) {
                detrend_method <- detrend_methods[[series_name]]

                detrend_warning <- tryCatch(
                    detrend.series(
                        y = rwl[, series_name],
                        y.name = series_name,
                        method = detrend_method,
                        return.info = T,
                        make.plot = F
                    ),
                    warning = function(w) {
                        warning <- w
                    }
                )
                warn_list[[series_name]] <- detrend_warning$message
            })
        })



        # if user changes the detrend method update the detrend method for all series to that method

        observeEvent(input$detrend_method, {
            for (series in series_list$current_names) {
                detrend_methods[[series]] <- input$detrend_method
            }
        })

        observeEvent(any(!sapply(warn_list, is.null)),
            {
                if (sum(!sapply(warn_list, is.null)) > 0) {
                    showModal(modalDialog(
                        title = "Warnings Detected",
                        paste(
                            "There were", sum(!sapply(warn_list, is.null)),
                            "warnings associated with your chosen detrend method see below"
                        ),
                        footer = modalButton("OK")
                    ))
                }
            },
            ignoreInit = TRUE
        )

        observe({
            lapply(series_list$current_names, function(series) {
                input_id <- paste0("detrend_method_", series)
                observeEvent(input[[input_id]],
                    {
                        detrend_methods[[series]] <- input[[input_id]]
                    },
                    ignoreInit = TRUE
                )
            })
        })





        ### REACTIVE functions ########################################

        table_data <- reactive({
            req(select_site(), input$select_table)
            query <- sprintf(
                "SELECT * FROM %s WHERE site_code = '%s'",
                input$select_table,
                select_site()
            )
            con <- localCheckout(raw_rwi)
            dbGetQuery(con, query)
        })





        # all the raw data from EW/RW/LW
        total_data <- reactive({
            req(select_site(), input$select_table)

            query <- sprintf("SELECT * FROM %s", input$select_table)
            con <- localCheckout(raw_rwi)
            dbGetQuery(con, query)
        })

        rwl <- reactive({
            table_data() %>%
                pivot_wider(names_from = series, values_from = value) %>%
                mutate(year = as.integer(year)) %>%
                arrange(year) %>%
                column_to_rownames(var = "year") %>%
                dplyr::select(-site_code) %>%
                as.rwl()
        })

        table_report <- reactive({
            rwl <- rwl()
            detrend_df <- detrend_df()

            for (type in method) {
                df_filter_rwl <- rwl %>%
                    detrend(method = type) %>%
                    rwi.stats() %>%
                    cbind(type)

                df_final[nrow(df_final) + 1, ] <- df_filter_rwl
            }

            detrend_df <- detrend_df %>%
                rwi.stats() %>%
                cbind(type = "Interactive Detrend")

            df_final <- rbind(df_final, detrend_df) %>%
                select(eps, snr, type) %>%
                rename(
                    "Detrend Method" = type,
                    "EPS" = eps,
                    "S/N" = snr
                ) %>%
                relocate(`Detrend Method`)

            df_final
        })


        detrend_df <- reactive({
            series_list <- series_list$current_names
            combined_df <- data.frame()
            rwl_df <- rwl()

            for (series_name in series_list) {
                detrend_method <- detrend_methods[[series_name]]

                detrend_df <- detrend.series(
                    y = rwl_df[, series_name],
                    y.name = series_name,
                    method = detrend_method,
                    return.info = TRUE,
                    make.plot = FALSE
                )

                combined_df <- cbind.fill(combined_df, detrend_df$series)

                colnames(combined_df)[ncol(combined_df)] <- series_name
            }

            rownames(combined_df) <- rownames(rwl())
            as.data.frame(combined_df)
        })



        ##################################################






        ### Reactive Vals and Global Variables###
        #################################################
        # default methods to choose from for detrending
        method <- c(
            "Spline",
            "ModNegExp",
            "Mean",
            "Ar",
            "Friedman",
            "ModHugershoff",
            "AgeDepSpline"
        )


        # create a dataframe to store the final results of the detrended methods
        df_final <- data.frame(matrix(ncol = 14, nrow = 0))
        colnames(df_final) <- c(
            "n.cores", "n.trees", "n", "n.tot",
            "n.wt", "n.bt", "rbar.tot", "rbar.wt", "rbar.bt",
            "c.eff", "rbar.eff", "eps", "snr", "type"
        )






        #############################################################################################################





        ### OUTPUTS####################################################################################################

        # Detrend Report for each method
        output$detrend_report <- renderDataTable({
            user_selection <- input$detrend_method

            df <- table_report() %>%
                datatable(
                    options = list(
                        dom = "t", paging = FALSE, searching = FALSE, info = FALSE,
                        ordering = FALSE
                    ),
                    rownames = FALSE
                ) %>%
                formatStyle(
                    "Detrend Method",
                    target = "row",
                    backgroundColor = styleEqual(user_selection, "lightblue")
                )
            df
        })

        ## Render a plotly graph of the raw rwl outputs for each series
        output$raw_plot <- renderPlotly({
            rwl_df <- rwl()

            rwl <- rwl_df %>%
                rownames_to_column(var = "year") %>%
                mutate(year = as.numeric(year)) %>%
                pivot_longer(!year, names_to = "series", values_to = "rwi")

            custom_js <-
                "function(e) {
  var plot = document.getElementById('tree_app-raw_plot');

  // Function to reset opacity for all traces
  function resetOpacity() {
    var update = {opacity: Array(plot.data.length).fill(1)};
    Plotly.restyle(plot, update);
  }

  // Event listener for clicking on a trace
  plot.on('plotly_click', function(data){
    var update = {opacity: Array(plot.data.length).fill(0.1)};
    update.opacity[data.points[0].curveNumber] = 1;
    Plotly.restyle(plot, update);
  });

  // Event listener for double-clicking on the plot area
  plot.on('plotly_doubleclick', function(){
    resetOpacity();

     Shiny.setInputValue('tree_app-series_plot_selection', null,
    { priority: 'event' })

  });
}
"


            detrend_df <- detrend_df() %>%
                rownames_to_column(var = "year") %>%
                pivot_longer(!year, names_to = "series", values_to = "rwi") %>%
                mutate(year = as.integer(year))



            ## Custom JavaScript for event handling
            plot_ly(detrend_df,
                x = ~year,
                y = ~rwi,
                type = "scatter",
                split = ~series,
                mode = "lines"
            ) %>%
                layout(
                    xaxis = list(title = "Year"),
                    yaxis = list(title = "RWI")
                ) %>%
                onRender(custom_js)
        })




        ## create a detrended plot of all series together
        output$detrended_plot <- renderPlotly({
            req(select_site())
            df_combined <- data.frame(matrix(
                ncol = 4,
                nrow = 0
            ))

            rwl <- rwl()

            for (type in method) {
                df_pivot <- rwl %>%
                    detrend(method = type, make.plot = F) %>%
                    chron(prewhiten = input$pre_white) %>%
                    cbind(type) %>%
                    rownames_to_column(var = "year") %>%
                    mutate(year = as.integer(year))

                df_combined <- rbind(df_combined, df_pivot)
            }

            custom_js <-
                "function(e) {
  var plot = document.getElementById('tree_app-detrended_plot');

  // Function to reset opacity for all traces
  function resetOpacity() {
    var update = {opacity: Array(plot.data.length).fill(1)};
    Plotly.restyle(plot, update);
  }

  // Event listener for clicking on a trace
  plot.on('plotly_click', function(data){
    var update = {opacity: Array(plot.data.length).fill(0.1)};
    update.opacity[data.points[0].curveNumber] = 1;
    Plotly.restyle(plot, update);
  });

  // Event listener for double-clicking on the plot area
  plot.on('plotly_doubleclick', function(){
    resetOpacity();
  });
}
"

            plot <- plot_ly(df_combined,
                x = ~year,
                y = ~std,
                type = "scatter",
                mode = "lines",
                color = ~type,
                alpha = 1
            ) %>%
                add_trace(
                    x = c(min(df_combined$year), max(df_combined$year)),
                    y = c(1, 1),
                    mode = "lines",
                    type = "scatter",
                    color = I("black"), # Set color to black
                    line = list(dash = "dash"),
                    colors = "black",
                    showlegend = FALSE # Remove trace from legend
                ) %>%
                layout(
                    xaxis = list(title = "Year"),
                    yaxis = list(title = "RWI")
                ) %>%
                onRender(custom_js)

            # Add vertical line for sss_yr_cutoff()
            if (input$show_subsample_annotation) {
                plot <- plot %>%
                    layout(shapes = list(
                        type = "rect",
                        fillcolor = "red", line = list(color = "red"),
                        opacity = 0.5,
                        y0 = 0, y1 = max(df_combined$std, na.rm = TRUE),
                        x0 = min(df_combined$year), x1 = sss_yr_cutoff()
                    ))
            }
            plot
        })











        cbind.fill <- function(...) {
            nm <- list(...)
            nm <- lapply(nm, as.matrix)
            n <- max(sapply(nm, nrow))
            do.call(cbind, lapply(nm, function(x) {
                rbind(x, matrix(, n - nrow(x), ncol(x)))
            }))
        }

        observeEvent(input$detrend_method_change, {
            method <- input$detrend_method_change$method
            series <- input$detrend_method_change$series

            detrend_methods[[series]] <- method
        })



        output$series_detrend_table <- renderDT({
            series_names <- series_list$current_names

            plot_selection <- input$series_plot_selection

            series_to_display <- if (!is.null(plot_selection)) {
                # Filter series names to the selected plot series
                series_names <- series_names[series_names == plot_selection]
            } else {
                # All series names
                series_names
            }

            df <- data.frame(
                Series = series_to_display,
                DetrendMethod = sapply(series_to_display, function(name) {
                    detrend_methods[[name]]
                }),
                WarningMessages = sapply(series_to_display, function(name) {
                    if (!is.null(warn_list[[name]])) {
                        warn_list[[name]]
                    } else {
                        "No Warnings"
                    }
                }),
                stringsAsFactors = FALSE
            )
            datatable(df, rownames = FALSE, escape = FALSE, selection = "none", extensions = "Buttons", options = list(
                columnDefs = list(
                    list(
                        targets = 1, # Index of the Detrend Method column
                        render = JS(
                            "function(data, type, row, meta) {",
                            "  if (type === 'display') {",
                            "    var options = ['Spline', 'ModNegExp', 'Mean', 'Ar', 'Friedman', 'ModHugershoff', 'AgeDepSpline'];",
                            "    var select = '<select class=\"detrend-method-dropdown\">';",
                            "    for (var i = 0; i < options.length; i++) {",
                            "      var selected = data === options[i] ? ' selected' : '';",
                            "      select += '<option value=\"' + options[i] + '\"' + selected + '>' + options[i] + '</option>';",
                            "    }",
                            "    select += '</select>';",
                            "    return select;",
                            "  }",
                            "  return data;",
                            "}"
                        )
                    )
                )
            ))
        })



        session$sendCustomMessage(type = "siteSelected", message = list())
    })
}
