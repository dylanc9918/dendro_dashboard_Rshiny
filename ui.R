ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  titlePanel("Tree Dashboard"),
  setBackgroundColor("#efefef"),
  tags$head(
    tags$link(rel = "tree icon", href = "forest-tree-icon.png"),
    tags$style(HTML("
.scrollable-content {
                overflow-y: auto;
                overflow-x: hidden;
                height: 90vh;
            }
            hr {border-top: 1px solid #000000;}

  #loadmessage {
  position:fixed; z-index:8; top:50%; left:50%; padding:10px;
  text-align:center; font-weight:bold; color:#000000; background-color:#CCFF66;
  }

  .loader {
  position:fixed; z-index:8; border:16px solid #999999;
  border-top: 16px solid #8B0000; border-radius: 50%;
  width: 120px; height: 120px; top:45%; left:45%;
  animation: spin 2s linear infinite;
  }

  .prevent_click{
  position:fixed;
  z-index:9;
  width:100%;
  height:100vh;
  background-color: transpare'nt;
  }

   .clicked {
        background-color: grey !important;
      }

  @keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
  }")),
    tags$script(HTML("
      $(document).on('click', '.btn', function() {
        $(this).addClass('clicked');
        var btn = $(this);
        setTimeout(function() {
          btn.removeClass('clicked');
        }, 100);
      });
    "))
  ),
  # display load spinner when shiny is busy
  conditionalPanel(
    condition = "$(\'html\').hasClass(\'shiny-busy\')",
    tags$div(class = "loader"),
    tags$div(class = "prevent_click")
  ),
  tabsetPanel(
    id = "main_tabs",
    useShinydashboard(),
    tabPanel(
      "Site Map",
      tree_site_map_ui("tree_app")
    ),
    tabPanel(
      "Detrending",
      tree_detrend_ui(id = "tree_app")
    ),
    tabPanel(
      "Correlation",
      tree_corr_ui(id = "tree_app")
    )
  )
)
