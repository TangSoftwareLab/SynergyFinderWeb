ui <- #shinyUI(
  fluidPage(
    # custom css for buttons and slider input               
    tags$head( 
      includeCSS("./styles/style.css"),
      # includeCSS("./styles/tooltip-curved.css"),
      # tags$script(src = "feedback_source.js"),
      singleton(tags$script(src = "correctBaselineSetting.js")),
      # tags$script(src = "feedback.js"), #also to top button
      tags$script(src = "https://d3js.org/d3.v4.min.js"),
      tags$script(src = "https://d3js.org/d3-scale-chromatic.v1.min.js"),
      tags$script(src = "https://unpkg.com/d3-3d/build/d3-3d.min.js"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;400;700&display=swap"
      )
    ),
    # setBackgroundImage("background.jpg"),
    navbarPage(
      title = tags$img(src="/images/logo.gif", height="30", width="150"),
      windowTitle = "SynergyFinder",
      id = "topNavBar",
      # HOME -------------------------------------------------------------------
      nav_item(a(class="item1",href = route_link("/"), "HOME"), value = "HOME"),
      # DASHBOARD --------------------------------------------------------------
      nav_item(a(class="item2",href = route_link("dashboard"), "DASHBOARD"), value = "DASHBOARD"),
      # USER GUIDE -------------------------------------------------------------
      nav_item(a(class="item3",href = route_link("userGuide"), "USER GUIDE"), value = "USER GUIDE"),
      nav_item(a(class="item4",href = route_link("faq"), "FAQ"), value = "FAQ", useShinyjs()),
      # USER CITATION ----------------------------------------------------------
      nav_item(a(class="item5",href = route_link("cite"), "HOW TO CITE"), value = "HOW TO CITE"),
      # ABOUT US ---------------------------------------------------------------
      nav_item(a(class="item6",href = route_link("aboutUs"), "ABOUT US"), value = "ABOUT US"),
      # CONTACT ----------------------------------------------------------------
      nav_item(a(class="item7",href = route_link("contact"), "CONTACT"), value = "CONTACT")
    ),
    router$ui
  )
#)
