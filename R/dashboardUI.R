# Module UI function
dashboardUI <- function(id) {
  ns <- NS(id)
  dashboardPage(
    # dash board header
    dashboardHeader(disable = TRUE),
    # sidebar content
    dashboardSidebar(
      width = "200px",
      # Menu elements
      sidebarMenu(
        id = "menu",
        menuItem(
          "Upload Data",
          tabName = "inputDataTab",
          selected = T
        ),
        menuItemOutput("doseResponseMenu"),
        menuItemOutput("synergyMenu"),
        menuItemOutput("sensitivityMenu"),
        menuItemOutput("reportMenu")
      )         
    ),
    dashboardBody(
      useToastr(),
      shinyjs::useShinyjs(),
      bsAlert("noPDdata"),
      uiOutput(outputId = 'errorTable'),
      fluidRow(
        column(
          width = 6,
          uiOutput(outputId = "plot_block")
        ),
        column(
          width = 6,
          selectInput(
            inputId = "correct_baseline", label = "Correct baseline",
            choices = list(
              # "",
              "Non: No correction" = "non",
              "Part: Correct negative inhibition values" = "part",
              "All: Correct whole matrix" = "all"),
            selected = "non"
          )
        )
      ),
      tabItems(
        inputDataTabUI(id = "inputDataTab"),
        doseResponseTabUI(id = "doseResponseTab"),
        synergyTabUI(id = "synergyTab"),
        sensitivityTabUI(id = "sensitivityTab"),
        reportTabUI(id = "reportTab")
      )
    )
  )
}
  