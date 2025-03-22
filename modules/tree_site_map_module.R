tree_site_map_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        shinyFeedback::useShinyFeedback(),
        div(
            class = "scrollable-content",
            hr(),
            column(
                width = 7,
                box(leafletOutput(ns("site_map")),
                    title = span(icon("map"), "Tree Site Map"),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary"
                ),
                box(DTOutput(ns("rwl_summary")),
                    title = span(icon("list"), "RWL Summary"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    collapsible = TRUE,
                    collapsed = TRUE
                )
            ),
            column(
                width = 5,
                box(verbatimTextOutput(ns("rwl_report")),
                    title = span(icon("folder"), "RWL report of Selected Site"),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    collapsed = TRUE
                ),
                box(plotOutput(ns("series_plot"), , height = "800px"),
                    title = span(icon("chart-line"), "Series Plot"),
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    collapsed = TRUE
                )
            )
        )
    )
}

# Server function for the tree site map module
tree_site_map_server <- function(id, site_info, table_data, url_list_create_data, future_climate) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        rv_markers <- reactiveValues(id = NULL, lat = NULL, lng = NULL)
        rwl <- reactiveVal(NULL)


        dendro_df <- reactive({
            dendro_data()
        })

        site_avail <- reactive({
            query <- "SELECT DISTINCT(site_code) FROM rw_data"

            con <- localCheckout(raw_rwi)
            dbGetQuery(con, query)
        })


        output$site_map <- renderLeaflet({
            dendro_df <- dendro_df()[dendro_df()$code %in% site_avail()$site_code, ]

            dendro <- dendro_df %>%
                leaflet() %>%
                addAwesomeMarkers(
                    icon = awesomeIcons(
                        icon = "tree",
                        library = "fa", markerColor = "lightgreen"
                    ),
                    lng = ~long,
                    lat = ~lat,
                    layerId = ~code,
                    popup = ~ paste("Site Name:", site_name),
                    group = ~species_name,
                    label = ~site_name
                ) %>%
                addProviderTiles("Esri.NatGeoWorldMap") %>%
                onRender("
          function(el, x) {
            var map = this;

            // Create a control to display the info box
            var infoBox = L.control({position: 'topright'});
            infoBox.onAdd = function(map) {
              var div = L.DomUtil.create('div', 'info');
              div.innerHTML = '<h4>Selected Site Information</h4>' +
                              '<p><strong>Tree Code:</strong> <span id=\"tree-code\"></span></p>' +
                              '<p><strong>Site Name:</strong> <span id=\"site-name\"></span></p>' +
                              '<p><strong>Species Name:</strong> <span id=\"species-name\"></span></p>';
              return div;
            };
            infoBox.addTo(map);

     map.eachLayer(function(layer) {
              if (layer instanceof L.Marker) {
                layer.on('click', function(e) {
                console.log(e);
                  var site_info = e.target.options;

                    document.getElementById('tree-code').innerText = site_info.layerId;
                    document.getElementById('site-name').innerText = site_info.label;
                    document.getElementById('species-name').innerText = site_info.group;

                });
              }
            });
          }
        ")


            dendro
        })



        observeEvent(input$site_map_marker_click, {
            click_data <- input$site_map_marker_click

            site_info(input$site_map_marker_click)



            dendro_df <- dendro_df()

            url_list_create_data <- url_list_create_data()

            click_df <- dendro_df[dendro_df$code == click_data$id, ]



            ### Making asynch API calls for site that is selected
            if (!is.null(click_data)) {
                future_climate(make_async_api_request(url_list_create_data))
            }




            if (!is.null(click_data)) {
                leafletProxy("site_map") %>%
                    addAwesomeMarkers(
                        lng = click_data$lng,
                        lat = click_data$lat,
                        icon = awesomeIcons(
                            icon = "tree",
                            library = "fa", markerColor = "lightblue"
                        ),
                        layerId = click_data$id,
                        options = markerOptions(zIndexOffset = 1000), ,
                        label = paste(
                            "Site Name:",
                            click_df$site_name
                        )
                    )

                if (!is.null(rv_markers$id) && rv_markers$id != click_data$id) {
                    leafletProxy("site_map") %>%
                        addAwesomeMarkers(
                            icon = awesomeIcons(
                                icon = "tree",
                                library = "fa", markerColor = "lightgreen"
                            ),
                            layerId = rv_markers$id,
                            lat = rv_markers$lat,
                            lng = rv_markers$lng,
                            label = paste(
                                "Site Name:",
                                rv_markers$id, "site_name"
                            )
                        )
                }

                rv_markers$id <- click_data$id
                rv_markers$lat <- click_data$lat
                rv_markers$lng <- click_data$lng
            }




            rwl_df <- table_data() %>%
                pivot_wider(names_from = series, values_from = value) %>%
                mutate(year = as.integer(year)) %>%
                arrange(year) %>%
                column_to_rownames(var = "year") %>%
                dplyr::select(-site_code) %>%
                as.rwl()

            rwl(rwl_df)
        })

        output$series_plot <- renderPlot({
            req(site_info()$id, nrow(table_data()) > 0)
            rwl_df <- rwl()
            if (is.null(rwl_df)) {
                return(NULL)
            }
            plot(rwl_df, plot.type = "spag")
        })

        output$rwl_report <- renderPrint({
            req(site_info()$id, nrow(table_data()) > 0)
            rwl_df <- rwl()
            if (is.null(rwl_df)) {
                return(NULL)
            } else {
                rwl.report(rwl_df)
            }
        })

        output$rwl_summary <- renderDT({
            req(site_info()$id, nrow(table_data()) > 0)
            rwl_df <- rwl()
            if (is.null(rwl_df)) {
                return(NULL)
            }
            summary(rwl_df)
        })
    })
}
