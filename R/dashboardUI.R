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
        menuItem("Upload Data", tabName = "inputDataTab"),
        menuItemOutput("doseResponseMenu"),
        menuItemOutput("synergyMenu"),
        menuItemOutput("sensitivityMenu"),
        menuItemOutput("reportMenu")
      )         
    ),
    dashboardBody(
      # uiOutput(outputId='fillInput'),
      useToastr(),
      shinyjs::useShinyjs(),
      bsAlert("noPDdata"),
      # uiOutput(outputId = 'exData'),
      uiOutput(outputId = 'errorTable'),
      fluidRow(
        column(
          width = 4,
          uiOutput(outputId = "plot_block")
        ),
        column(
          width = 4,
          selectInput(
            inputId = "correct_baseline", label = "Correct baseline",
            choices = list("", "Non" = "non", "Part" = "part", "All" = "all"),
            selected = ""
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
  