inputDataTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    # INPUT DDATA
    tabName = id,
    # tags$h1("Upload Your Dataset"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "inputDatatype",
            label = "1. Choose data format:",
            width = '100%',
            choices = list("Table" = "Table", "Matrix" = "Matrix"),
            selected = "Table"
          ),
          br(),
          downloadButton(outputId = "loadExData_small", label = "example data")
        ),
        column(
          width = 4,
          div(
            id = "annotfileid",
            uiOutput('resettableInput'),
            shinyBS::bsPopover(#session,
              id = "q1",
              title = "Input data structure:",
              content = paste0(
                tags$p("Table format:"),
                # tags$br(),
                tags$image(
                  src = 'images/exampleTab.png',
                  width = '400', height = '200'
                ),
                tags$br(),
                tags$br(),
                tags$p("Matrix format:"),
                # tags$br(),
                tags$image(
                  src = 'images/exampleMat.png',
                  width = '400', height = '200'
                ),
                tags$br(),
                tags$br(),
                tags$p(
                  "For more information about input file format please check the USER GUIDE."
                )
              ),
              placement = "bottom",
              trigger = "click"
            )
            # HTML('<div id = "spanpop" class="tooltip-item"></div>')
          ),
          shinyWidgets::materialSwitch(
            inputId = "annoSwitch",
            label = "Annotate Data",
            status = "primary",
            right = TRUE
          ),
          bsAlert("alertannotfile")
        ),
        column(
          width = 4,
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
        )
      ),
      hr(),
      fluidRow(
        DTOutput("inputData"),
        br(),
        tags$div(
          id = "drugAnnoResult",
          hr(),
          tags$h1("Drug Information"),
          br(),
          DTOutput(outputId = "drugAnno"),
          downloadButton(outputId = "downloadDrugAnno", label = "Download Drug Information"),
          br(),
          hr(),
          tags$h1("Drug Target Information"),
          br(),
          DTOutput(outputId = "drugAnnoTarget"),
          tags$p(
            "* Potent targets are defined as the targets displaying binding affinities <= 1,000 nM
            from the bioactivity databases, or targets recorded in the unary databases.
            The information comes from",
            tags$a("MICHA", href = 'https://micha-protocol.org/', target = '_blank'),
            ", which integrates the data from 6 databases: ",
            tags$a("DTC", href = "https://drugtargetcommons.fimm.fi/", target = "_blank"),
            ", ",
            tags$a("chEMBL", href = "https://www.ebi.ac.uk/chembl/", target = "_blank"),
            ", ",
            tags$a("BindingDB", href = "https://www.bindingdb.org/bind/index.jsp", target = "_blank"),
            ", ",
            tags$a("DrugBank", href = "https://go.drugbank.com/", target = "_blank"),
            ", ",
            tags$a("Guide to Pharmacology", href = "https://www.guidetopharmacology.org/", target = "_blank"),
            ", ",
            tags$a("DGIdb", href = "https://www.dgidb.org/", target = "_blank"),
            "."),
          downloadButton(outputId = "downloadTargetAnno", label = "Download Drug Target Information"),
          br()
        ),
        tags$div(
          id = "cellAnnoResult",
          hr(),
          tags$h1("Cell Line Information"),
          br(),
          DTOutput(outputId = "cellAnno"),
          downloadButton(outputId = "downloadCellAnno", label = "Download Cell Information")
        )
      )
    )
  ) # tabItem - "inputTab"
}

doseResponseTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    # fluidRow(
    #   column(
    #     width = 2,
    #     uiOutput(outputId = "DR_block_ui")
    #   )
    # ),
    hr(),
    box(
      id = "boxDoseResponseCurve",
      title = "Dose Response Curve",
      solidHeader = TRUE,
      width = 6,
      height = 400,
      collapsible = TRUE,
      fluidRow(
        plotOutput(outputId = "DRC_plot") %>% 
          withSpinner(color="#D2D2D2")
      ),
      tags$hr(),
      fluidRow(
        column(
          width = 6,
          uiOutput(outputId = "DRC_drug_ui"),
          shinyWidgets::materialSwitch(
            inputId = "DRC_grid",
            label = "Background grids",
            status = "primary",
            right = TRUE
          ),
          downloadButton(
            outputId = "downloadDRC",
            label = "Download Plot (SVG)",
            icon =shiny::icon("download")
          )
        ),
        column(
          width = 6,
          colourpicker::colourInput(
            inputId = "DRC_dot_color",
            label = "Color for dots",
            value = "#FF0000"
          ),
          colourpicker::colourInput(
            inputId = "DRC_curve_color",
            label = "Color for curve",
            value = "black"
          )
        )
      )
    ),
    uiOutput(outputId = "multi_drug_DR_plot"),
    box(
      id = "boxDoseResponseMatrix",
      title = "Dose Response Map (2 Drugs Combination)",
      solidHeader = TRUE,
      width = 6, 
      height = 400,
      collapsible = TRUE,
      fluidRow(
        plotlyOutput(outputId = "DR_plot") %>% 
          withSpinner(color="#D2D2D2")
      ),
      tags$hr(),
      fluidRow(
        uiOutput(outputId = "DR_2_drugs_ui")
      ),
      fluidRow(
        column(
          width = 6,
          sliderInput(
            inputId = "DR_text_size",
            label = "Texts size scale",
            value = 1,
            min = 0.1,
            max = 2
          ),
          selectInput(
            inputId = "DR_summary_statistic",
            label = "Summary Statistics",
            choice = c(
              "None" = NULL,
              "Mean" = "mean", "Median" = "median",
              "25% quantile" = "quantile_25",
              "75% quantile" = "quantile_75"
            ),
            selected = NULL
          ),
          sliderInput(
            inputId = "DR_heatmap_label_size",
            label = "Heatmap text label size",
            value = 1,
            min = 0.1,
            max = 2
          ),
          selectInput(
            inputId = "DR_rep_statistic",
            label = "Statistics for Observations",
            choices = c(
              "Non" = NULL,
              "95% confidence interval" = "ci",
              "Standard Deviation (SD)" = "sd",
              "Standard error of mean (SEM)" = "sem"),
            selected = NULL
          ),
          shinyWidgets::materialSwitch(
            inputId = "DR_grid",
            value = TRUE,
            label = "Grids on surface",
            status = "primary",
            right = TRUE
          )
        ),
        column(
          width = 6,
          shinyWidgets::radioGroupButtons(
            inputId = "DR_plot_type",
            label = "Plot type",
            choices = c("HeatMap" = "heatmap", "3D surface" = "3D"),
            status = "primary"
          ),
          colourpicker::colourInput(
            inputId = "DR_high_value_color",
            label = "High response value color",
            value = "#FF0000"
          ),
          colourpicker::colourInput(
            inputId = "DR_low_value_color",
            label = "Low response value color",
            value = "#00FF00"
          ),
          colourpicker::colourInput(
            inputId = "DR_heatmap_label_color",
            label = "Heatmap text label color",
            value = "#000000"
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
    fluidRow(
      # Synergy Score Plot
      uiOutput(outputId = "multi_drug_syn_plots"),
      box(
        id = "BoxSynergyScorePlot",
        title = "Synergy Map (2 Drugs Combination)",
        solidHeader = TRUE,
        width = 12, 
        collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            plotlyOutput(outputId = "syn_ZIP_plot") %>% 
              withSpinner(color="#D2D2D2"),
            plotlyOutput(outputId = "syn_Loewe_plot") %>% 
              withSpinner(color="#D2D2D2")
          ),
          column(
            width = 6,
            plotlyOutput(outputId = "syn_HSA_plot") %>% 
              withSpinner(color="#D2D2D2"),
            plotlyOutput(outputId = "syn_Bliss_plot") %>% 
              withSpinner(color="#D2D2D2")
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 6,
            uiOutput(outputId = "syn_2_drugs_ui"),
          ),
          column(
            width = 6,
            shinyWidgets::radioGroupButtons(
              inputId = "syn_plot_type",
              label = "Plot type",
              choices = c(
                "HeatMap" = "heatmap",
                "3D Surface" = "3D",
                "2D Contour" = "2D"),
              status = "primary",
              selected = "3D"
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = "syn_summary_statistic",
              label = "Summary Statistics",
              choice = c(
                "Mean" = "mean", "Median" = "median",
                "25% quantile" = "quantile_25",
                "75% quantile" = "quantile_75"
              ),
              selected = "mean"
            )
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "syn_text_size",
              label = "Texts size scale",
              value = 1,
              min = 0.1,
              max = 2
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "syn_high_value_color",
              label = "Synergy effect color",
              value = "#FF0000"
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "syn_low_value_color",
              label = "Antagnositic effect color",
              value = "#00FF00"
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            shinyWidgets::materialSwitch(
              inputId = "syn_grid",
              value = TRUE,
              label = "Show grids",
              status = "primary",
              right = TRUE
            ),
            selectInput(
              inputId = "syn_rep_statistic",
              label = "Statistics from Bootstrap",
              choices = c(
                "Non" = NULL,
                "95% confidence interval" = "ci",
                "Standard Deviation (SD)" = "sd",
                "Standard error of mean (SEM)" = "sem"),
              selected = NULL
            )
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "syn_heatmap_label_size",
              label = "Heatmap text label size",
              value = 1,
              min = 0.1,
              max = 2
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "syn_heatmap_label_color",
              label = "Heatmap text label color",
              value = "#000000"
            )
          )
        )
      ),
      # Bar Barometer plot
      # box(
      #   id = "boxBarPlot",
      #   title = "Bar and Barometer Plot",
      #   solidHeader = TRUE,
      #   width = 12, 
      #   collapsible = TRUE,
      fluidRow(
        column(
          width = 3,
          offset = 1,
          align = "center",
          br(),
          tags$h3("Selected Data Point"),
          hr(),
          DTOutput(outputId = "syn_barometer_values")
        ),
        column(
          width = 4,
          # offset = 4,
          plotOutput(outputId = "syn_barometer") %>% 
            withSpinner(color="#D2D2D2")
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput(outputId = "syn_bar_plot_ui")
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 3,
          sliderInput(
            inputId = "bb_panel_title_size",
            label = "Panel title size",
            value = 10,
            min = 0,
            max = 20
          )
        ),
        column(
          width = 3,
          sliderInput(
            inputId = "bb_axis_text_size",
            label = "Axis text size",
            value = 10,
            min = 0,
            max = 20
          )
        ),
        column(
          width = 3,
          sliderInput(
            inputId = "bb_highlight_label_size",
            label = "Highlited label size",
            value = 10,
            min = 0,
            max = 20
          )
        ),
        column(
          width = 3,
          colourpicker::colourInput(
            inputId = "bb_barometer_color",
            label = "Barometer color",
            value = "#CC3311"
          )
          
        )
      ),
      fluidRow(
        column(
          width = 3,
          colourpicker::colourInput(
            inputId = "bb_pos_value_color",
            label = "Positive bar color",
            value = "#FF0000"
          )
        ),
        column(
          width = 3,
          colourpicker::colourInput(
            inputId = "bb_neg_value_color",
            label = "Negative bar color",
            value = "#00FF00"
          )
        ),
        column(
          width = 3,
          colourpicker::colourInput(
            inputId = "bb_highlight_pos_color",
            label = "Highlighted positive bar color",
            value = "#BB0000"
          )
        ),
        column(
          width = 3,
          colourpicker::colourInput(
            inputId = "bb_highlight_neg_color",
            label = "Highlighted negative bar color",
            value = "#00BB00"
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "downloadBar",
            label = "Download Bar Plot (SVG)",
            icon =shiny::icon("download")
          )
        ),
        column(
          width = 3,
          downloadButton(
            outputId = "downloadBarometer",
            label = "Download Barometer (SVG)",
            icon =shiny::icon("download")
          )
        )
      )
      # )
    )
  )
}

sensitivityTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidRow(
      DTOutput("summaryTable"),
      hr(),
      box(
        id = "SSPlot",
        title = "S-S Plot",
        solidHeader = TRUE,
        width = 12, 
        collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            plotlyOutput(outputId = "ss_ZIP_plot") %>%
              withSpinner(color="#D2D2D2"),
            plotlyOutput(outputId = "ss_Loewe_plot") %>% 
              withSpinner(color="#D2D2D2")
          ),
          column(
            width = 6,
            plotlyOutput(outputId = "ss_HSA_plot") %>% 
              withSpinner(color="#D2D2D2"),
            plotlyOutput(outputId = "ss_Bliss_plot") %>% 
              withSpinner(color="#D2D2D2")
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "ss_point_color",
              label = "Point color",
              value = "#2166AC"
            )
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "ss_point_size",
              label = "Point size (mm)",
              value = 1,
              min = 1,
              max = 10,
              step = 0.5,
            )
          )
        ),
        column(
          width = 3,
          colourpicker::colourInput(
            inputId = "ss_label_color",
            label = "Text label color",
            value = "#2166AC"
          )
        ),
        column(
          width = 3,
          sliderInput(
            inputId = "ss_label_size",
            label = "Label size (pt)",
            value = 10,
            min = 1,
            max = 50,
            step = 1
          )
        ),
        fluidRow(
          column(
            width = 3,
            br(),
            shinyWidgets::materialSwitch(
              inputId = "ss_show_label",
              value = TRUE,
              label = "Show labels",
              status = "primary",
              right = TRUE
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
    fluidRow(
      column(
        width = 3,
        tags$h3("Select Blocks to Report"),
        uiOutput(outputId = "report_blocks_ui")
      ),
      column(
        width = 3,
        tags$h3("Download Reports for Plots"),
        downloadButton(
          outputId = "static_report",
          label = "Static PDF Report"
        ),
        br(),
        br(),
        downloadButton(
          outputId = "dynamic_report",
          label = "Dynamic HTML Report"
        ),
        br(),
        tags$p("Note: The dimention reducted multi-drug surface plot will not",
               "be included in 'static report'.")
      ),
      column(
        width = 3,
        tags$h3("Download Data Tables"),
        selectInput(
          inputId = "download_table_format",
          label = "Select output table format",
          choice = c("CSV", "XLSX", "TXT"),
          selected = "CSV"
        ),
        downloadButton(
          outputId = "download_summary_table",
          label = "Summary Table"
        ),
        br(),
        br(),
        downloadButton(
          outputId = "download_synergy_table",
          label = "Synergy Score Table"
        )
      ),
      column(
        width = 3,
        tags$h3("Download R Object for SynergyFinder Package"),
        downloadButton(
          outputId = "download_r_object",
          label = "R Object"
        )
      )
    )
  )
}