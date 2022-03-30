library(shiny)
library(shiny.router)

page_1 <- tabPanel("Page 1", value = "page_1",
                   "This is Page 1")

page_2 <- tabPanel("Page 2", value = "page_2",
                   "This is Page 2")

router <- make_router(
  route("/", page_1),
  route("page2", page_2)
)

#+++++++++++++
# ui
#+++++++++++++

ui <- navbarPage("Dashboard",
                 tabPanel(a(href = route_link("/"), "Page 1"), router$ui),
                 tabPanel(a(href = route_link("page2"), "Page 2"))
)

#+++++++++++++
# server
#+++++++++++++

server <- function(input, output, session)
{
  router$server(input, output, session)
}

shinyApp(ui, server)

library(shiny)
library(bslib)

ui <- page_navbar(
  nav("First tab"),
  nav("Second tab"),
  nav_item(a(href="http://stackoverflow.com", "stackoverflow")))

shinyApp(ui, server = function(...){})