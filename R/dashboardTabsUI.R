inputDataTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    # INPUT DDATA
    tabName = id,
    # tags$h1("Upload Your Dataset"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          selectInput(
            "inputDatatype",
            label = "1. Choose data format:",
            width = '100%',
            choices = list("Table" = "Table", "Matrix" = "Matrix"),
            selected = "Table"
          )
        ),
        column(
          width = 3,
          div(
            id = "annotfileid",
            uiOutput('resettableInput'),
            HTML('<div id = "spanpop" class="tooltip-item"></div>')
          ),
          bsAlert("alertannotfile")
        ),
        column(
          width = 3,
          selectInput(
            "selectInhVia",
            label = "3. Phenotypic Response:",
            width = '100%',
            choices = list(
              "",
              "Inhibition" = "inhibition",
              "Viability" = "viability"
            ),
            selected = ""
          )
        )# ,
        # column(
        #   width = 3,
        #   tags$div(
        #     title="Move slider to visualize the drug combination dose-response data",
        #     id = "toursliderdr",
        #     switchButton(
        #       inputId = "Switch",
        #       label = "4. Visualize dose response data"
        #     )
        #   )
        # )
      ),
      fluidRow(
        column(
          width = 3,
          #tags$p("Download example data", style = "font-weight: 700;"),
          downloadButton(outputId = "loadExData_small", label = "example data")
        ),
        hr()
      ),
      DTOutput("inputData")
    )
  ) # tabItem - "inputTab"
}

doseResponseTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    #when slider for PlotDoseresponse is on
    conditionalPanel(
      # condition = "input.Switch == 1",
      condition = "input.selectInhVia != '' && !input.resettableInput",
      # dynamically create tabs with content
      box(
        id = "boxDose", solidHeader = FALSE, width = 12, collapsible = FALSE,
        #title="Dose Response Map",
        fluidRow(
          column(
            width = 10,
            uiOutput(outputId='tabs')
          ),
          column(
            width = 2,
            style = "box-shadow: 0 1px 4px 0 rgba(0, 0, 0, 0.2), 0 1px 1px 0 rgba(0, 0, 0, 0.19);",
            tags$h4("Adjust plot size"),
            sliderInput(
              inputId = "height", label = "Height",
              min = 0, max = 1000, value = 400, step = 1
            ),
            sliderInput(
              inputId = "width", label = "Width",
              min = 1, max = 13, value = 12, step = 1
            ),
            hr(),
            # tags$h4("Estimate outliers:"),
            # uiOutput(outputId = 'increase1'),
            # uiOutput(outputId = 'increase2'),
            # actionButton("excludeconc", "Impute"),
            # hr(),
            tags$div(
              title = "Calculate the synergy scores for drug combinations.",
              id = "tourcalcsyn",
              switchButton(inputId = "Switch2", label = "Calculate synergy")
            )
          )
        )
      )
    )
  )
}

synergyTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    # when slider for calcuate synergy is on
    conditionalPanel(
      condition = "input.Switch2 == 1",
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "correction", label = "Correct baseline",
            choices = list("Non" = "non", "Part" = "part", "All" = "all")
          )
        ),
        column(
          width = 4,
          tags$div(
            # id = "tourmodels",
            selectInput(
              inputId = "methods", label = "Reference model",
              choices = list(
                "ZIP" = "ZIP", "Bliss" = "Bliss",
                "Loewe" = "Loewe", "HSA" = "HSA")
              )
          )
        ),
        column(
          width = 4,
          tags$div(
            switchButton(
              inputId = "Switch4",
              label = "Visualize synergy scores"
            )
          )
        )
      )
    ),
    hr(),
    # when slider for plot synergy is on
    conditionalPanel(
      condition = "input.Switch4 == 1",
      fluidRow(
        box(
          id = "boxSyn", width = 12, collapsible = FALSE, solidHeader = FALSE, 
          fluidRow(uiOutput(outputId = 'tabs2')),
          fluidRow(
            height = 770,
            br(),
            column(
              width = 6,
              tags$div(
                title = "Brush and double-click to zoom",
                plotOutput(
                  outputId = "plotsyn1", height = 600,
                  dblclick = "plot7_dblclick",
                  brush = brushOpts(
                    id = "plotincrease_brush7",
                    resetOnNew = !0
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  downloadButton(
                    outputId = "download2Dsyn",
                    label = "Download"
                  )
                ),
                tags$div(
                  # title = "Show most synergistic area. \n on \n off",
                  column(
                    width = 4,
                    radioButtons(
                      inputId = "synarea", label = h4("Most synergistic area:"),
                      choices = list("ON" = 1, "OFF" = 0),
                      selected = 1,
                      inline = !0
                    )
                  )
                ),
                tags$div(
                  # title = "Adjust 2D surface grid. \n grid on \n grid off",
                  column(
                    width = 4,
                    radioButtons(
                      inputId = "sizegridsyn", label = h4("Grid:"),
                      choices = list("ON" = 1, "OFF" = 0),
                      selected = 1,
                      inline = !0
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                plotlyOutput(outputId = "plotsyn2", height = 600)
              ),
              fluidRow(
                column(
                  width = 4, 
                  downloadButton(outputId = "download3Dsyn", label = "Download")
                ),
                tags$div(
                  # title="Adjust 3D surface grid. \n grid on \n grid off \n transparent grid",
                  column(
                    width = 5, offset = 3,
                    radioButtons(
                      inputId = "sizegridsyn2", label = h4("Grid:"),
                      choices = list("ON" = 1, "OFF" = 0, "transparent" = -1),
                      selected = 1, inline = !0
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

reportTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    # Save report
    conditionalPanel(
      condition = "input.Switch4 == 1",
      box(
        id = "boxSyn", width = 12, collapsible = TRUE, collapsed = TRUE,
        solidHeader = TRUE,
        # title = "Static (pdf) report",
        h6("In case of visualisation proplems use Adobe Reader"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "selectStatic2",
              label = "Dose response plot type",
              width = '97%',
              choices = list(
                "all" = "all",
                "heatmap" = "heatmap",
                "curve" = "curve"
              )
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "selectStatic",
              label = "Synergy plot type",
              width = '97%',
              choices = list(
                "none" = "none",
                "all" = "all",
                "3D" = "3D",
                "2D" = "2D"
              )
            )
          ),
          column(
            width = 3, offset = 1,
            radioButtons(
              inputId = "synareaRepStat",
              label = tags$h4("Most synergistic area:"),
              choices = list("ON" = 1, "OFF" = 0),
              selected = 1,
              inline = !0
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            uiOutput(outputId = 'selectinputconprints')
          )
        ),
        downloadButton("downloadData2", label = "Download")
      ),
      box(
        id = "boxSave2",
        title = "Dynamic (pdf) report",
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = !0,
        tags$h6("In case of visualisation proplems use Adobe Reader"),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "selectDynamic2",
              label = "Dose response plot type",
              width = '97%',
              choices = list(
                "all" = "all",
                "heatmap" = "heatmap",
                "curve" = "curve"
              )
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "selectDynamic",
              label = "Synergy plot type",
              width = '97%',
              choices = list("all" = "all", "3D" = "3D")
            )
          ),
          column(
            width = 3, offset = 1,
            radioButtons(
              inputId = "synareaRepDyn",
              label = h4("Most synergistic area:"),
              choices = list("ON" = 1, "OFF" = 0),
              selected = 1,
              inline = !0
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            uiOutput(outputId='selectinputconprintd')
            )
        ),
        downloadButton(outputId = "downloadData", label = "Download")
      ),
      box(
        id = "boxSave3",
        title = "Short (pdf) report (HeatMap + 2D Synergy plot)",
        width = 12,
        collapsible = T,
        collapsed = T,
        solidHeader = !0,
        tags$h6("In case of visualisation proplems use Adobe Reader"),
        fluidRow(
          column(
            width = 12,
            uiOutput(outputId='selectinputconprintcomb')
          )
        ),
        fluidRow(
          column(
            width = 4,
            downloadButton(outputId = "downloadData3", label = "Download")
          ),
          column(
            width = 4, offset = 4,
            radioButtons(
              inputId = "synareaRep",
              label = tags$h4("Most synergistic area:"),
              choices = list("ON" = 1, "OFF" = 0),
              selected = 1,
              inline = !0
            )
          )
        )
      ),
      box(
        id = "boxSave4",
        title = "Table for synergy scores",
        width = 12,
        collapsible = T,
        collapsed = F,
        fluidRow(
          column(
            width = 12, offset = 0,
            downloadButton(
              outputId = "downloadSynscores1",
              label = "Download summaried synergy scores(.xlsx)"
            ),
            downloadButton(
              outputId = "downloadSynscores2",
              label = "Download summaried synergy scores(.csv)"
            ),
            downloadButton(
              outputId = "downloadSynscores3",
              label = "Download summaried synergy scores(.txt)"
            )
          ),
          column(
            width = 12, offset = 0,
            downloadButton(
              outputId = "downloadSynscoresFull1",
              label = "Download synergy scores(.xlsx)"
            ),
            downloadButton(
              outputId = "downloadSynscoresFull2",
              label = "Download synergy scores(.csv)"
            ),
            downloadButton(
              outputId = "downloadSynscoresFull3",
              label = "Download synergy scores(.txt)"
            )
          )
        )
      )
    )
  )
}