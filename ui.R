library(shiny)
library(shinytoastr)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(plotly)
source("MyButtons.R")
source("moduleChangeTheme.R")

ui <- dashboardPage(#skin = "black",
  # dash board header
  dashboardHeader(
                  title=img(src="magnifier_i_light blue.png", height="42", width="200")#,

                  # tags$li(class = "dropdown",
                  #         actionButton("videobt","User guide",icon("file-text"), 
                  #                      style="color: #fff; background-color: #00C0EF; border-color: #00C0EF")
                  # ),
                  # tags$li(class = "dropdown",
                  #         tags$a(href="http://www.ncbi.nlm.nih.gov/pubmed/26949479", target="_blank", 
                  #                tags$b("Reference")
                  #         )
                  # ),
                  # #dropdownMenuOutput("numusers_"),
                  # tags$li(class = "dropdown notifications-menu", 
                  #         HTML('<a href="#" class="dropdown-toggle" data-toggle="dropdown"> <i class="fa fa-user"></i><span class="label label-primary">1</span></a><ul class="dropdown-menu"> <li> <ul class="menu"> <li> <a href="mailto:jing.tang@helsinki.fi"><i class="fa fa-user text-warning"></i>Author: Jing Tang <i class="fa fa-envelope-o" aria-hidden="true"></i></a> <a href="#">Recent updates:</a> <a href="#"><i class="fa fa-info text-warning"></i>simplified structure of input file;</a> <a href="#"><i class="fa fa-info text-warning"></i>added most synergistic area <br> indentification;</a> <a href="#"><i class="fa fa-info text-warning"></i>added remove outlier feature;</a> <a href="#"><i class="fa fa-info text-warning"></i>added additional feature to <br>download 3D map as svg object;</a> <a href="#"><i class="fa fa-info text-warning"></i>reduced plots and reports <br> downloading time;</a> <a href="#"><i class="fa fa-info text-warning"></i>smooth update for choosing <br> new parameters;</a> <a href="#"><i class="fa fa fa-info text-warning"></i>fixed WebGL context problem <br> for old browsers;</a> <a href="#"><i class="fa fa-info text-warning"></i>fixed "cannot read property of <br> undefined" for additional traces of 3d surface;</a></li></ul></li></ul>')
                  # )
  ),

  
  # sidebar content
  dashboardSidebar(
              # custom css for buttons and sliderinput               
            tags$head( 
              tags$link(rel = "stylesheet", type = "text/css", href = "allcss.css"),
              tags$link(rel = "stylesheet", type = "text/css", href = "tooltip-curved.css"),
              tags$link(rel = "stylesheet", type = "text/css", href = "drop.css"),
              tags$script(src = "feedback_source.js"),
              tags$script(src = "feedback.js"), #also to top button
              tags$script(src = "tour.js")
            ),
             
             # Menu elements
            sidebarMenu(
              id="menu1",
              menuItem("Theme", tabName = "tabThemes"),
              menuItem("Intro", tabName = "introTab"),
              menuItem("User Guid", tabName = "userguidTab"),
              menuItem("Upload Data", tabName = "inputdataTab"),
              menuItem("Dose-response Map", tabName = "doseresponseTab"),
              menuItem("Synergy Map", tabName = "synergyTab"),
              menuItem("Download Report", tabName = "reportTab")  
             )         
  ),
  dashboardBody(
    ### changing theme
    # shinyDashboardThemes(
    #   theme = "grey_light"
    # ),
    # Custom theme ------------------------------------------------------------
    uiChangeThemeOutput(),
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
        tabName = "tabThemes",
        fluidRow(
          column(
            width = 12,
            
            # Theme drop-down ---------------------------------------------------------
            uiChangeThemeDropdown()
          )
        )
      ),
      tabItem(tabName = "introTab",
              div(
                br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                fluidRow(column(offset = 3, width = 6, 
                                div(
                                  HTML('<div class="primary_header"><h1>SynergyFinder web application</h1><h2>an interactive tool for analyzing drug combination dose-response data </h2></div>'
                                  ),  br(),
                                  HTML('<button type="button" id="buttonTour" class="btn btn-primary btn-lg">
          <span class="glyphicon glyphicon-play"></span>
           Start the tour
           </button>'), id = "startour")
                )), id = "wraptour")
      ),
      tabItem(
        tabName = "userguidTab",
        div(
          tags$p("How it works"),
          br(),
          # fluidRow(column(offset = 1, width = 10, HTML('<p align="center"><b>Video tutorial</b></p>'))),
          # fluidRow(column(offset = 3, width = 6, HTML('<img onmouseover="" style="cursor: pointer;" onclick="javascript:openvideo()" src="vtt.png"></img>'))), hr(), br(),
          # fluidRow(column(offset = 1, width = 10, HTML('<p align="center"><b>Technical documentation</b></p>'))),
          # fluidRow(column(offset = 3, width = 6, HTML('<img onmouseover="" style="cursor: pointer;" onclick="javascript:techdoc()" src="manual.png"></img>'))),
          # HTML('<object data="tech.pdf" type="application/pdf" width="100%" height="500"> alt : <a href="tech.pdf" download>tech.pdf</a></object>'),
          HTML('<iframe src="https://player.vimeo.com/video/192242384" width="870" height="600" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>')
        )
      ),
      tabItem(
        tabName = "inputdataTab",
        # tags$h1("Upload Your Dataset"),
        fluidPage(
          fluidRow(
            column(
              width = 4,
              tags$p("1. SynergyFinder accept three different file formats: CSV, XLSX, or TXT."),
              div(id="annotfileid", uiOutput('resettableInput'),
                  HTML('<div id = "spanpop" class="tooltip-item"></div>')),
              bsAlert("alertannotfile"),
              ),
            column(
              width = 4,
              # HTML('
              # <div id="tourfileformat" class="t-toggle-button">
              #   <label class="t-toggle-button__option js-checked">
              #     <input name="audience" value="designList" type="radio" checked="checked" id="paneltype_designer">
              #     <span>Table</span>
              #   </label>
              #   <label class="t-toggle-button__option">
              #     <input name="audience" value="devList" type="radio" id="panel-type_developer">
              #     <span>Matrix</span>
              #   </label>
              # </div>
              #      ')
              tags$p("2. The drug combination data must be arrange in one of 2 formats: Table or Matrix."),
              selectInput("inputDatatype", label = "Choose data format:", width = '100%',
                          choices = list("Table" = "Table", "Matrix" = "Matrix"),
                          selected = "Table")
            ),
            column(
              width = 4,
              tags$p("3. The measurement for drug response be: inhibition or viability"),
              selectInput("selectInhVia", label = "Choose readout:", width = '100%',
                          choices = list("","Inhibition" = "inhibition", "Viability" = "viability"),
                          selected = "")
            ),
          ),
          fluidRow(
            column(
              width = 4,
              tags$div(title="Move slider to visualize the drug combination dose-response data",
                       id = "toursliderdr",
                       switchButton(inputId = "Switch",
                                    label = "Visualize dose-response data"))
              ),
            column(
              width = 4,
            ),
            column(
              width = 4,
              tags$p("For more detail about input data, please check User Guid page.",
                     "Click the button below to download example files for testing purposes."),
              downloadButton(outputId = "loadExData_small", label = "example data"))
          ),
          hr()
        )
      ), # tabItem - "inputTab"
      tabItem(tabName = "doseresponseTab",
              selectInput("correction", "Correction", 
                          choices = list("Non" = "non", "Part" = "part", "All" = "all")),
              hr(),
              #when slider for PlotDoseresponse is on
              conditionalPanel(
                condition = "input.Switch == 1",
                # dynamically create tabs with content
                fluidRow(
                  box(width = 12, collapsible = F, id = "boxDose", solidHeader = !0,
                      column(10,
                             uiOutput(outputId='tabs')
                      ),
                      column(2, br(),
                             box(width = 12.5, status = "info", collapsible = !0,
                                 fluidRow(
                                   column(10, offset = 1,
                                          sliderInput(inputId = "height", label = "Height", min = 0, max = 1000, value = 500, step = 1))),
                                 fluidRow(
                                   column(10, offset = 1,
                                          sliderInput(inputId = "width", label = "Width", min = 1, max = 13, value = 12, step = 1)))
                             ), 
                             box(id = "removeoutliers", width = 12.5, title = "Estimate missing responses or outliers", status = "info", collapsible = !0,
                                 box(width = 12.5, title = "Choose concentration:",
                                     fluidRow(column(12, uiOutput(outputId='increase1'))),
                                     fluidRow(column(12, uiOutput(outputId='increase2'))),
                                     fluidRow(column(10, offset = 2, actionButton("excludeconc", "Approx. conc.")))
                                 )
                             ) 
                      )
                  )
                )
              ),
              tags$div(title="Calculate the synergy scores for drug combinations.", id = "tourcalcsyn",
                       switchButton(inputId = "Switch2",
                                    label = "Calculate synergy")),
            ),
      tabItem(tabName = "synergyTab",
              # when slider for calcuate synergy is on
              conditionalPanel(
                condition = "input.Switch2 == 1",
                
                div(selectInput("methods", "Method", choices = list("ZIP" = "ZIP", "Bliss" = "Bliss", "Loewe" = "Loewe", "HSA" = "HSA"), width = '40%'), id = "tourmodels"),

                tags$div(title="Visualize the synergy scores for drug combinations as 2D or 3D interaction landscape over the dose-response matrix.", id = "tourvizsyn",
                         switchButton(inputId = "Switch4",
                                      label = "Visualize synergy scores"))
              ),
              hr(),
              # when slider for plot synergy is on
              conditionalPanel(
                condition = "input.Switch4 == 1",
                fluidRow(
                  box(width = 12, collapsible = F, id = "boxDose", solidHeader = !0, class = "heysyn",
                      fluidRow(uiOutput(outputId='tabs2')),
                      fluidRow( br(),
                                box(
                                  width = 12, status = "info", solidHeader = !0, collapsible = !0, height = 770,
                                  title = "Synergy Maps",
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
                                )
                      )
                  )
                )
              )
      ),
      tabItem(tabName = "reportTab",
              # tags$head(tags$style(HTML(".small-box {height: 105px}"))),
              # valueBox(actionButton("Save_report", "Save!"), "Save full report", icon = icon("save", lib = "glyphicon"), width = 13),  
        # Save report
        box(width = 12, collapsible = T,collapsed = T, id = "boxSave", solidHeader = !0, status = "info", title = "Static (pdf) report",
            h6("In case of visualisation proplems use Adobe Reader"),
            fluidRow(
              column(width = 4,
                     selectInput("selectStatic2", "Dose response plot type", choices = list("all" = "all", "heatmap" = "heatmap", "curve" = "curve"), width = '97%')
              ),
              column(width = 4,
                     selectInput("selectStatic", "Synergy plot type", choices = list("none" = "none", "all" = "all", "3D" = "3D", "2D" = "2D"), width = '97%')
              ),
              column(3, offset = 1,
                     radioButtons("synareaRepStat", label = h4("Most synergistic area:"), choices = list("ON" = 1, "OFF" = 0),
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
        
        box(width = 12, collapsible = T, collapsed = T, id = "boxSave2", solidHeader = !0, status = "info", title = "Dynamic (pdf) report",
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
        box(width = 12, collapsible = T, collapsed = T, id = "boxSave3", solidHeader = !0, status = "info", 
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
        box(width = 12, collapsible = T, collapsed = F, id = "boxSave4", title = "Synergy scores:",
            fluidRow(column(12, offset = 0,
                            downloadButton("downloadSynscores1", label = "Download synergy scores(.xlsx)"),
                            downloadButton("downloadSynscores2", label = "Download synergy scores(.csv)"),
                            downloadButton("downloadSynscores3", label = "Download synergy scores(.txt)")#,
                            # HTML(paste0('<a download href="./synergy_scores.xlsx"><img src="xls.png" class = "marleft" alt="Smiley face" height="42" width="42"></a>
                            #              <a download href="./synergy_scores.csv"><img src="csv.png" class = "marleft" alt="Smiley face" height="42" width="42"></a>
                            #              <a download href="./synergy_scores.txt"><img src="txt.png" class = "marleft" alt="Smiley face" height="42" width="42"></a>'))
            )))
        )
    ),
    HTML('<img src="werecommend.png" class="browsers_" alt="HTML5 Icon" width="124" height="65">')
    #HTML('<footer style = "position:absolute!important;width:100%!important;bottom:0!important;left:50%!important">&copy; <script>document.write((new Date).getFullYear());</script>, FIMM</footer>')
  )
)