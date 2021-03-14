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
        fluidRow(
          style = "margin-top: 15px;",
          column(
            width = 8, offset = 2,
            tags$div(
              class = "textCard", id = "userGuide",
              withMathJax(includeMarkdown("./doc/userGuide.Rmd"))
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
