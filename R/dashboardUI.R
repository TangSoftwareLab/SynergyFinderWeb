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
        id = "menu1",
        menuItem("Upload Data", tabName = "inputdataTab"),
        menuItem("Dose Response Map", tabName = "doseResponseTab"),
        menuItem("Synergy Map", tabName = "synergyTab"),
        menuItem("Download Report", tabName = "reportTab")  
      )         
    ),
    dashboardBody(
      # uiOutput(outputId='fillInput'),
      useToastr(),
      shinyjs::useShinyjs(),
      bsAlert("noPDdata"),
      uiOutput(outputId='exData'),
      uiOutput(outputId='errorTable'),
      tabItems(
        inputDataTabUI("inputdataTab"),
        doseResponseTabUI("doseResponseTab"),
        synergyTabUI("synergyTab"),
        reportTabUI("reportTab")
      )
    )
  )
}
  