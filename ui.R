ui <- shinyUI(
  fluidPage(
    # custom css for buttons and slider input               
    tags$head( 
      includeCSS("./styles/style.css"),
      # includeCSS("./styles/tooltip-curved.css"),
      tags$script(src = "feedback_source.js"),
      tags$script(src = "feedback.js"), #also to top button
      tags$script(src = "https://d3js.org/d3.v4.min.js"),
      tags$script(src = "https://d3js.org/d3-scale-chromatic.v1.min.js"),
      tags$script(src = "https://unpkg.com/d3-3d/build/d3-3d.min.js"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;400;700&display=swap"
      )
    ),
    setBackgroundImage("background.jpg"),
    navbarPage(
      title = tags$img(src="/images/logo.gif", height="30", width="150"),
      windowTitle = "SynergyFinder",
      id = "topNavBar",
      # HOME -------------------------------------------------------------------
      tabPanel("HOME", homeUI("home")),
      # DASHBOARD --------------------------------------------------------------
      tabPanel("DASHBOARD", dashboardUI("dashboard")),
      # USER GUIDE -------------------------------------------------------------
      tabPanel(
        "USER GUIDE",
        dashboardPage(
          # dash board header
          dashboardHeader(disable = TRUE),
          # sidebar content
          dashboardSidebar(
            width = "200px",
            # Menu elements
            sidebarMenu(
              id = "menuUserGuide",
              menuItem("Upload Data", tabName = "inputDataUserGuideTab"),
              menuItem("Dose Response Map", tabName = "doseResponseUserGuideTab"),
              menuItem("Synergy Map", tabName = "synergyUserGuideTab"),
              menuItem("Sensitivity Score", tabName = "sensitivityUserGuideTab"),
              menuItem("DownloadReports", tabName = "reportUserGuideTab")
            )         
          ),
          dashboardBody(
            tabItems(
              tabItem(
                tabName = "inputDataUserGuideTab",
                    tags$div(
                      class = "userGuide",
                      withMathJax(includeMarkdown("./doc/user_guide_upload_data_tab.Rmd"))
                      # inclRmd("./doc/user_guide_upload_data.Rmd")
                    )
              ),
              tabItem(
                tabName = "doseResponseUserGuideTab",
                tags$div(
                  class = "userGuide",
                  withMathJax(includeMarkdown("./doc/userGuide.Rmd"))
                )
              ),
              tabItem(
                tabName = "sensitivityUserGuideTab",
                tags$div(
                  class = "userGuide",
                  withMathJax(includeMarkdown("./doc/userGuide.Rmd"))
                )
              ),
              tabItem(
                tabName = "reportUserGuideTab",
                tags$div(
                  class = "userGuide",
                  withMathJax(includeMarkdown("./doc/userGuide.Rmd"))
                )
              )
            )
          )
        )
      ),
      # USER CITATION ----------------------------------------------------------
      tabPanel("HOW TO CITE", howToCiteUI("howToCite")),
      # ABOUT US ---------------------------------------------------------------
      tabPanel("ABOUT US", aboutUsUI("aboutUs")),
      # CONTACT ----------------------------------------------------------------
      tabPanel("CONTACT", contactUsUI("contactUs"))
    )
  )
)
