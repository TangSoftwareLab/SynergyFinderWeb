homeUI <- function(id) {
  ns <- NS(id)
  tags$div(
    id = "home",
    fluidPage(
      fluidRow(
        class = "row",
        column(
          width = 5,
          offset = 1,
          align = "center",
          # tags$h1("SynergyFinder"),
          tags$img(src = "images/title.png", height = 100),
          tags$br(),
          p("an interactive tool for analyzing drug combination dose-response data"),
          tags$br(),
          actionButton(
            inputId = "getStart",
            label = "Analyze",
            type="primary",
            class = "btn-lg shadow lift mr-1",
            style="color: #fff"
          ),
          actionButton(
            inputId = "toGuide",
            label = "User Guide",
            class = "btn-primary-soft lif"
          ),
        ),
        column(
          width = 6,
          align = "left",
          # 3D plot:
          includeHTML("./www/images/homePlot.svg")
        )
      ),
      tags$div(
        class = "footer",
        tags$div(
          class = "footer_contents",
          HTML(
            "&copy; ",
            "Copyright <script>document.write((new Date).getFullYear());</script>, ",
            '<a style="color:#0277bd;" target="_blank" ',
            'href="https://www.helsinki.fi/en/researchgroups/network-pharmacology-for-precision-medicine">Netphar</a>, ',
            'Faculty of Medicine, University of Helsinki. All Rights Reserved'
          )
        )
      )
    )
  )
}
 
