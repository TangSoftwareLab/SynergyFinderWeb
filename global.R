# Load required packages
dependency <- c(
  "shiny", "shinyjs", "shinytoastr", "shinyWidgets", "shinydashboard",
  "shinyBS", "DT", "synergyfinder", "htmltools", #"kableExtra",
  "dashboardthemes", "ggplot2", "scales", "gplots", "lattice", "plotly", "grid",
  "xtable", "shinybusy", "writexl", "dplyr", "shinycssloaders", "purrr",
  "RPostgreSQL", "RPostgres", "DBI", "config", "colourpicker", "shiny.router",
  "bslib")
sapply(
  dependency,
  library,
  character.only = T
)

# Load local R scripts
sapply(
  list.files("./R", pattern = ".*\\.R$", recursive = TRUE, full.names = TRUE),
  source,
  .GlobalEnv
)

# Path to temporarily store pdf reports.
reportspath <- "tem_folder/"
cellosauruspath <- "cellosaurus.xml"

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

# Router

home <- tabPanel("HOME", homeUI("home"))
dashboard <- tabPanel("DASHBOARD", dashboardUI("dashboard"))
userGuide <- tabPanel("USER GUIDE", dashboardPage(
  # dash board header
  dashboardHeader(disable = TRUE),
  # sidebar content
  dashboardSidebar(
    width = "200px",
    # Menu elements
    sidebarMenu(
      id = "menuUserGuide",
      menuItem(
        "Upload Data",
        tabName = "inputDataUserGuideTab",
        selected = T
      ),
      menuItem(
        "Dose Response Map",
        tabName = "doseResponseUserGuideTab"
      ),
      menuItem(
        "Synergy Map",
        tabName = "synergyUserGuideTab"
      ),
      menuItem(
        "Sensitivity Map",
        tabName = "sensitivityUserGuideTab"
      ),
      menuItem(
        "DownloadReports",
        tabName = "reportUserGuideTab"
      ),
      menuItem(
        "VideoGuide",
        tabName = "videoGuideTab"
      )
    )         
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "inputDataUserGuideTab",
        tags$div(
          class = "userGuide",
          withMathJax(
            includeMarkdown(
              "./doc/user_guide_upload_data_tab.Rmd"
            )
          )
        )
      ),
      tabItem(
        tabName = "doseResponseUserGuideTab",
        tags$div(
          class = "userGuide",
          withMathJax(
            includeMarkdown(
              "./doc/user_guide_dose_response_tab.Rmd"
            )
          )
        )
      ),
      tabItem(
        tabName = "synergyUserGuideTab",
        tags$div(
          class = "userGuide",
          withMathJax(
            includeMarkdown(
              "./doc/user_guide_synergy_score_tab.Rmd"
            )
          )
        )
      ),
      tabItem(
        tabName = "sensitivityUserGuideTab",
        tags$div(
          class = "userGuide",
          withMathJax(
            includeMarkdown(
              "./doc/user_guide_sensitivity_score_tab.Rmd"
            )
          )
        )
      ),
      tabItem(
        tabName = "reportUserGuideTab",
        tags$div(
          class = "userGuide",
          withMathJax(
            includeMarkdown(
              "./doc/user_guide_report_tab.Rmd"
            )
          )
        )
      ),
      tabItem(
        tabName = "videoGuideTab",
        tags$div(
          class = "userGuide",
          tags$h1("A Brief Introduction to SynergyFinder Plus"),
          tags$iframe(
            width="560",
            height="315",
            src="https://www.youtube.com/embed/2SFPLaSXj54",
            frameborder="0",
            allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen=NA
          ),
          tags$h1("A Tour for SynergyFinder Plus Web Application"),
          tags$iframe(
            width="560",
            height="315",
            src="https://www.youtube.com/embed/KVnVvdCCUCo",
            frameborder="0",
            allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen=NA
          )
        )
      )
    )
  )
)
)
faq <- tabPanel("FAQ", faqUI("faq"))
cite <- tabPanel("HOW TO CITE", howToCiteUI("howToCite"))
aboutUs <- tabPanel("ABOUT US", aboutUsUI("aboutUs"))
contact <- tabPanel("CONTACT", contactUsUI("contactUs"))
router <- make_router(
  route("/", home),
  route("dashboard", dashboard),
  route("userGuide", userGuide),
  route("faq", faq),
  route("cite", cite),
  route("aboutUs", aboutUs),
  route("contact", contact)
)