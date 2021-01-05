# Module UI function
dashboardUI <- dashboardPage(#skin = "black",
      # dash board header
      dashboardHeader(disable = TRUE),
      # sidebar content
      dashboardSidebar(
        width = "200px",
        # Menu elements
        sidebarMenu(id="menu1",
          menuItem("Upload Data", tabName = "inputdataTab"),
          menuItem("Dose Response Map", tabName = "doseresponseTab"),
          menuItem("Synergy Map", tabName = "synergyTab"),
          menuItem("Download Report", tabName = "reportTab")  
        )         
      ),
      dashboardBody(
                    # uiOutput(outputId='fillInput'),
                    # tags$head(tags$style(HTML('.alert-info,.bg-aqua,.callout.callout-info,.label-info,.modal-info .modal-body,
                    #                            .skin-blue .main-header .logo,.skin-blue .main-header .logo:hover,
                    #                            .skin-blue .main-header .navbar{background-image:linear-gradient(#54b4eb,#2fa4e7,#1d9ce5)}'))),
                    useToastr(),
                    shinyjs::useShinyjs(),
                    bsAlert("noPDdata"),
                    uiOutput(outputId='exData'),
                    uiOutput(outputId='errorTable'),
                    tabItems(
                      # Tab content -------------------------------------------------------------
                      tabItem(
                        # INPUT DDATA
                        tabName = "inputdataTab",
                        # tags$h1("Upload Your Dataset"),
                        fluidPage(
                          fluidRow(
                            column(width = 3,
                              selectInput("inputDatatype", label = "1. Choose data format:", width = '100%',
                                          choices = list("Table" = "Table", "Matrix" = "Matrix"),
                                          selected = "Table")
                            ),
                            column(width = 3,
                                   div(id="annotfileid", uiOutput('resettableInput'),
                                       HTML('<div id = "spanpop" class="tooltip-item"></div>')),
                                   bsAlert("alertannotfile"),
                            ),
                            column(width = 3,
                              selectInput("selectInhVia", label = "3. Phenotypic Response:", width = '100%',
                                          choices = list("","Inhibition" = "inhibition", "Viability" = "viability"),
                                          selected = "")
                            ),
                            column(width = 3,
                                   tags$div(title="Move slider to visualize the drug combination dose-response data",
                                            id = "toursliderdr",
                                            switchButton(inputId = "Switch",
                                                         label = "4. Visualize dose response data"))
                            ),
                          ),
                          fluidRow(
                            column(width = 3,
                                   #tags$p("Download example data", style = "font-weight: 700;"),
                                   downloadButton(outputId = "loadExData_small", label = "example data")
                            ),
                            hr()
                          ),
                          DTOutput("inputData")
                        )
                      ), # tabItem - "inputTab"
                      tabItem(tabName = "doseresponseTab",
                              #when slider for PlotDoseresponse is on
                              conditionalPanel(
                                condition = "input.Switch == 1",
                                  # dynamically create tabs with content
                                  box(width = 12, collapsible = FALSE,
                                      #title="Dose Response Map",
                                      id = "boxDose", solidHeader = FALSE,
                                      fluidRow(
                                        column(10,
                                               uiOutput(outputId='tabs')
                                        ),
                                        column(2,
                                               style = "box-shadow: 0 1px 4px 0 rgba(0, 0, 0, 0.2), 0 1px 1px 0 rgba(0, 0, 0, 0.19);",
                                               tags$h4("Adjust plot size"),
                                               sliderInput(inputId = "height",
                                                           label = "Height", min = 0,
                                                           max = 1000, value = 400,
                                                           step = 1),
                                               sliderInput(inputId = "width",
                                                           label = "Width", min = 1,
                                                           max = 13, value = 12,
                                                           step = 1),
                                               hr(),
                                               tags$h4("Estimate outliers:"),
                                               uiOutput(outputId='increase1'),
                                               uiOutput(outputId='increase2'),
                                               actionButton("excludeconc", "Impute"),
                                               hr(),
                                               tags$div(title="Calculate the synergy scores for drug combinations.",
                                                        id = "tourcalcsyn",
                                                        switchButton(inputId = "Switch2",
                                                                     label = "Calculate synergy")
                                               )
                                               )
                                        )
                                      )
                                )
                              ),
                      tabItem(tabName = "synergyTab",
                              # when slider for calcuate synergy is on
                              conditionalPanel(
                                 condition = "input.Switch2 == 1",
                                fluidRow(
                                  column(4,
                                         selectInput("correction", "Correct baseline",
                                                     choices = list("Non" = "non",
                                                                    "Part" = "part",
                                                                    "All" = "all"))
                                         ),
                                  column(4,
                                         div(selectInput("methods", "Reference model", 
                                                         choices = list("ZIP" = "ZIP", 
                                                                        "Bliss" = "Bliss", 
                                                                        "Loewe" = "Loewe", 
                                                                        "HSA" = "HSA")), 
                                             id = "tourmodels")
                                  ),
                                  column(4, 
                                         tags$div(title="Visualize the synergy scores for drug combinations as 2D or 3D interaction landscape over the dose-response matrix.",
                                                  id = "tourvizsyn",
                                                  switchButton(inputId = "Switch4",
                                                               label = "Visualize synergy scores"))
                                         )
                              )
                              ),
                              hr(),
                              # when slider for plot synergy is on
                              conditionalPanel(
                                condition = "input.Switch4 == 1",
                                fluidRow(
                                  box(width = 12, collapsible = FALSE, id = "boxSyn", solidHeader = FALSE, #class = "heysyn",
                                      #title = "Synergy Effect Analysis",
                                      fluidRow(uiOutput(outputId='tabs2')),
                                      fluidRow(br(), height = 770,
                                                # box(
                                                #   width = 12, status = "info", solidHeader = !0, collapsible = !0, height = 770,
                                                #   title = "Synergy Maps",
                                                  column(6,
                                                         tags$div(title="Brush and double-click to zoom",
                                                                  plotOutput("plotsyn1", height = 600, dblclick = "plot7_dblclick", brush = brushOpts(id = "plotincrease_brush7",resetOnNew = !0))),
                                                         fluidRow(
                                                           column(4, downloadButton("download2Dsyn", label = "Download")),
                                                           tags$div(title="Show most synergistic area. \n on \n off",
                                                                    column(4,
                                                                           radioButtons("synarea", label = h4("Most synergistic area:"),
                                                                                        choices = list("ON" = 1, "OFF" = 0),
                                                                                        selected = 1, inline = !0)
                                                                    )),
                                                           tags$div(title="Adjust 2D surface grid. \n grid on \n grid off",
                                                                    column(4,
                                                                           radioButtons("sizegridsyn", label = h4("Grid:"),
                                                                                        choices = list("ON" = 1, "OFF" = 0),
                                                                                        selected = 1, inline = !0)
                                                                    )))
                                                  ),
                                                  column(6,
                                                         fluidRow(
                                                           plotlyOutput("plotsyn2", height = 600)),
                                                         fluidRow(
                                                           column(4, downloadButton("download3Dsyn", label = "Download")),
                                                           tags$div(title="Adjust 3D surface grid. \n grid on \n grid off \n transparent grid",
                                                                    column(5,offset = 3,
                                                                           radioButtons("sizegridsyn2", label = h4("Grid:"),
                                                                                        choices = list("ON" = 1, "OFF" = 0, "transparent" = -1),
                                                                                        selected = 1, inline = !0)
                                                                    )))
                                                         
                                                  )
                                                # )
                                      )
                                  )
                                )
                              )
                      ),
                      tabItem(tabName = "reportTab",
                              # tags$head(tags$style(HTML(".small-box {height: 105px}"))),
                              # valueBox(actionButton("Save_report", "Save!"), "Save full report", icon = icon("save", lib = "glyphicon"), width = 13),  
                              # Save report
                              conditionalPanel(
                                condition = "input.Switch4 == 1",
                              box(width = 12, collapsible = TRUE, collapsed = T,
                                  id = "boxSyn", solidHeader = TRUE, #class = "heysyn",
                                  title = "Static (pdf) report",
                                  h6("In case of visualisation proplems use Adobe Reader"),
                                  fluidRow(
                                    column(width = 4,
                                           selectInput("selectStatic2", "Dose response plot type", 
                                                       choices = list("all" = "all", "heatmap" = "heatmap", "curve" = "curve"), 
                                                       width = '97%')
                                    ),
                                    column(width = 4,
                                           selectInput("selectStatic", "Synergy plot type", 
                                                       choices = list("none" = "none", "all" = "all", "3D" = "3D", "2D" = "2D"),
                                                       width = '97%')
                                    ),
                                    column(3, offset = 1,
                                           radioButtons("synareaRepStat", label = h4("Most synergistic area:"),
                                                        choices = list("ON" = 1, "OFF" = 0),
                                                        selected = 1, inline = !0)
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput(outputId='selectinputconprints')
                                    )
                                  ),
                                  downloadButton("downloadData2", label = "Download")
                              ),
                              
                              box(width = 12, collapsible = T, collapsed = T, 
                                  id = "boxSave2", solidHeader = !0, #status = "info",
                                  title = "Dynamic (pdf) report",
                                  h6("In case of visualisation proplems use Adobe Reader"),
                                  fluidRow(
                                    column(width = 4,
                                           selectInput("selectDynamic2", "Dose response plot type", choices = list("all" = "all", "heatmap" = "heatmap", "curve" = "curve"), width = '97%')
                                    ),
                                    column(width = 4,
                                           selectInput("selectDynamic", "Synergy plot type", choices = list("all" = "all", "3D" = "3D"), width = '97%')
                                    ),
                                    column(3, offset = 1,
                                           radioButtons("synareaRepDyn", label = h4("Most synergistic area:"), choices = list("ON" = 1, "OFF" = 0),
                                                        selected = 1, inline = !0)
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput(outputId='selectinputconprintd')
                                    )
                                  ),
                                  downloadButton("downloadData", label = "Download")
                              ),
                              box(width = 12, collapsible = T, collapsed = T, 
                                  id = "boxSave3", solidHeader = !0, #status = "info", 
                                  title = "Short (pdf) report (HeatMap + 2D Synergy plot)",
                                  h6("In case of visualisation proplems use Adobe Reader"),
                                  fluidRow( 
                                    column(12, uiOutput(outputId='selectinputconprintcomb'))),
                                  fluidRow(  
                                    column(4,downloadButton("downloadData3", label = "Download")),
                                    column(4, offset = 4,
                                           radioButtons("synareaRep", label = h4("Most synergistic area:"), choices = list("ON" = 1, "OFF" = 0),
                                                        selected = 1, inline = !0)
                                    )
                                  )
                              ),
                              box(width = 12, collapsible = T, collapsed = F, id = "boxSave4", title = "Table for synergy scores",
                                  fluidRow(column(12, offset = 0,
                                                  downloadButton("downloadSynscores1", label = "Download synergy scores(.xlsx)"),
                                                  downloadButton("downloadSynscores2", label = "Download synergy scores(.csv)"),
                                                  downloadButton("downloadSynscores3", label = "Download synergy scores(.txt)")#,
                                                  # HTML(paste0('<a download href="./synergy_scores.xlsx"><img src="xls.png" class = "marleft" alt="Smiley face" height="42" width="42"></a>
                                                  #              <a download href="./synergy_scores.csv"><img src="csv.png" class = "marleft" alt="Smiley face" height="42" width="42"></a>
                                                  #              <a download href="./synergy_scores.txt"><img src="txt.png" class = "marleft" alt="Smiley face" height="42" width="42"></a>'))
                                  )))
                              )
                      )
                    ),
                    # HTML('<img src="werecommend.png" class="browsers_" alt="HTML5 Icon" width="124" height="65">')
                   
      )
    )
