server <- function(input, output, session) {
    # Hide tabs initially
    hideTab(inputId = "main_tabs", target = "Detrending")
    hideTab(inputId = "main_tabs", target = "Correlation")



    tree_site_map_server("tree_app", site_info, table_data, url_list_create_data, future_climate)

    observeEvent(site_info(), {
        showTab(inputId = "main_tabs", target = "Detrending")
        showTab(inputId = "main_tabs", target = "Correlation")
    })

    rv <- reactiveValues(
        detrending_loaded = FALSE,
        correlation_loaded = FALSE
    )



    observeEvent(input$main_tabs, {
        if (input$main_tabs == "Detrending" && !rv$detrending_loaded) {
            tree_detrend_server("tree_app", site_info, detrend_methods)
            rv$detrending_loaded <- TRUE
        } else if (input$main_tabs == "Correlation" && !rv$correlation_loaded) {
            tree_corr_server("tree_app", site_info, table_data, detrend_methods, future_climate)
            rv$correlation_loaded <- TRUE
        }
    })
}
