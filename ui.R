ui <- shinyUI(
  fluidPage(
    # custom css for buttons and sliderinput               
    tags$head( 
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "allcss.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "tooltip-curved.css"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "drop.css"),
      # tags$script(src = "./UI/home/fisheye.js"),
      tags$script(src = "feedback_source.js"),
      tags$script(src = "feedback.js"), #also to top button
      # tags$script(src = "tour.js"),
      tags$script(src="https://d3js.org/d3.v4.min.js"),
      tags$script(src="https://d3js.org/d3-scale-chromatic.v1.min.js"),
      tags$script(src="https://unpkg.com/d3-3d/build/d3-3d.min.js"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;400;700&display=swap")
    ),
    setBackgroundImage("background.jpg"),
    navbarPage(title = img(src="magnifier_i.gif", height="30", width="150"),
               id = "topNavBar",
               # HOME ------------------------------------------------------------
               tabPanel("HOME", homeUI,
                        img(src="magnifier.gif")),
               # DASHBOARD ------------------------------------------------------------
               tabPanel("DASHBOARD", dashboardUI),
               # ABOUT US ------------------------------------------------------------
               tabPanel("ABOUT US", aboutUs),
               # CONTACT ------------------------------------------------------------
               tabPanel("CONTACT", contactUs),
               # USER GUIDE ------------------------------------------------------------
               tabPanel("USER GUIDE")
    )
    #HTML('<footer style = "position:absolute!important;width:100%!important;bottom:0!important;left:50%!important">&copy; <script>document.write((new Date).getFullYear());</script>, FIMM</footer>')
  )
)
