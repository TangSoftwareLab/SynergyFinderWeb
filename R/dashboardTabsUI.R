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
            inputId = "inputDatatype",
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
        ),
        column(
          width = 3,
          br(),
          downloadButton(outputId = "loadExData_small", label = "example data")
        )
      ),
      hr(),
      DTOutput("inputData")
    )
  ) # tabItem - "inputTab"
}

doseResponseTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidRow(
      column(
        width = 2,
        uiOutput(outputId = "DR_block_ui")
        )
      ),
    hr(),
    box(
      id = "boxDoseResponseCurve",
      title = "Dose Response Curve",
      solidHeader = TRUE,
      width = 5,
      height = 400,
      collapsible = TRUE,
      fluidRow(
        plotOutput(outputId = "DRC_plot")
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
          )
        ),
        column(
          width = 6,
          colourpicker::colourInput(
            inputId = "DRC_dot_color",
            label = "Color for dots",
            value = "#C24B40"
          ),
          colourpicker::colourInput(
            inputId = "DRC_curve_color",
            label = "Color for curve",
            value = "black"
          )
        )
      )
    ),
    box(
      id = "boxDoseResponseMatrix",
      title = "Dose Response Matrix",
      solidHeader = TRUE,
      width = 7, 
      height = 400,
      collapsible = TRUE,
      fluidRow(
          plotlyOutput(outputId = "DR_plot")
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
          sliderInput(
            inputId = "DR_heatmap_label_size",
            label = "Heatmap text label size",
            value = 1,
            min = 0.1,
            max = 2
          ),
          selectInput(
            inputId = "DR_rep_statistic",
            label = "Statistics for Replicates",
            choices = c("Non" = NULL, "95% confidence interval" = "ci",
                        "Standard error of mean" = "sem"),
            selected = NULL
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
          shinyWidgets::materialSwitch(
            inputId = "DR_grid",
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
            choices = c("HeatMap", "3D surface"),
            status = "primary"
          ),
          colourpicker::colourInput(
            inputId = "DR_high_value_color",
            label = "High response value color",
            value = "#C24B40"
          ),
          colourpicker::colourInput(
            inputId = "DR_low_value_color",
            label = "Low response value color",
            value = "#2166AC"
          ),
          colourpicker::colourInput(
            inputId = "DR_heatmap_label_color",
            label = "Heatmap text label color",
            value = "#000000"
          )
        )
      )
    ),
    uiOutput(outputId = "multi_drug_DR_plots")
  )
}

synergyTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidRow(
      # Synergy Score Plot
      box(
        id = "BoxSynergyScorePlot",
        title = "Synergy Scores",
        solidHeader = TRUE,
        width = 12, 
        collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            plotlyOutput(outputId = "syn_ZIP_plot"),
            plotlyOutput(outputId = "syn_Loewe_plot")
          ),
          column(
            width = 6,
            plotlyOutput(outputId = "syn_HSA_plot"),
            plotlyOutput(outputId = "syn_Bliss_plot")
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
              value = "#C24B40"
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "syn_low_value_color",
              label = "Antagnositic effect color",
              value = "#2166AC"
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            shinyWidgets::materialSwitch(
              inputId = "syn_grid",
              label = "Grids on surface",
              status = "primary",
              right = TRUE
            ),
            selectInput(
              inputId = "syn_rep_statistic",
              label = "Statistics for Replicates",
              choices = c("Non" = NULL, "95% confidence interval" = "ci",
                          "Standard error of mean" = "sem"),
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
      box(
        id = "boxBarPlot",
        title = "Bar and Barometer Plot",
        solidHeader = TRUE,
        width = 12, 
        collapsible = TRUE,
        fluidRow(
          column(
            width = 4,
            offset = 4,
            plotOutput(outputId = "syn_barometer")
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput(
              outputId = "syn_bar_plot",
              click = "syn_bar_plot_click",
              dblclick = "syn_bar_plot_dbclick"
            )
          )
        ),
        hr(),
        tags$h4("Bar Plot Setting"),
        fluidRow(
          column(
            width = 3,
            sliderInput(
              inputId = "bb_panel_title_size",
              label = "Panel title size",
              value = 5,
              min = 0,
              max = 20
            )
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "bb_axis_text_size",
              label = "Axis text size",
              value = 5,
              min = 0,
              max = 20
            )
          ),
          column(
            width = 3,
            sliderInput(
              inputId = "bb_highlight_label_size",
              label = "Highlited label size",
              value = 5,
              min = 0,
              max = 20
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "bb_pos_value_color",
              label = "Positive bar color",
              value = "#000000"
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "bb_neg_value_color",
              label = "Negative bar color",
              value = "#000000"
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "bb_highlight_pos_color",
              label = "Highlighted positive bar color",
              value = "#000000"
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "bb_highlight_neg_color",
              label = "Highlighted negative bar color",
              value = "#000000"
            )
          )
        )
      ),
      uiOutput(outputId = "multi_drug_syn_plots")
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
            plotlyOutput(outputId = "ss_ZIP_plot"),
            plotlyOutput(outputId = "ss_Loewe_plot")
          ),
          column(
            width = 6,
            plotlyOutput(outputId = "ss_HSA_plot"),
            plotlyOutput(outputId = "ss_Bliss_plot")
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
            numericInput(
              inputId = "ss_point_size",
              label = "Point size (mm)",
              value = 1,
              min = 1,
              max = 10,
              step = 0.5,
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            br(),
            shinyWidgets::materialSwitch(
              inputId = "ss_show_label",
              label = "Show labels",
              status = "primary",
              right = TRUE
            )
          ),
          column(
            width = 3,
            colourpicker::colourInput(
              inputId = "ss_label_color",
              label = "Antagnositic effect color",
              value = "#2166AC"
            )
          ),
          column(
            width = 3,
            numericInput(
              inputId = "ss_label_size",
              label = "Label size (pt)",
              value = 10,
              min = 1,
              max = 50,
              step = 1
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
        width = 6,
        selectInput(
          inputId = "DRReport",
          label = "Dose response plots",
          choices = c(
            "All",
            "HeatMap",
            "3D surface"),
          selected = NULL
        ),
        selectInput(
          inputId = "SynReport",
          label = "Synergy or sensitivity plots",
          choices = c(
            "All",
            "HeatMap",
            "2D contour",
            "3D surface"),
          selected = NULL
        ),
        tags$p("Note: The surface plot in static report might not be ploted ",
               "in the expected angle. It's better to download the plots in",
               " the web page or export dynamic report.")
      ),
      column(
        width = 6,
        uiOutput(outputId = "report_blocks_ui")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$h4("Download Reports for Plots"),
        downloadButton(
          outputId = "static_report",
          label = "Download Static PDF Report"
        ),
        downloadButton(
          outputId = "dynamic_report",
          label = "Download Dynamic RNoteBook"
        ),
        ),
      column(
        width = 4,
        tags$h4("Download Data Tables"),
        selectInput(
          inputId = "download_table_format",
          label = "Select output table format",
          choice = c("CSV", "XLSX", "TXT"),
          selected = "CSV"
        ),
        downloadButton(
          outputId = "download_summary_table",
          label = "Download Summary Table"
        ),
        downloadButton(
          outputId = "download_synergy_table",
          label = "Download Synergy Score Table"
        )
      ),
      column(
        width = 4,
        tags$h4("Download R Object for SynergyFinder Package"),
        downloadButton(
          outputId = "download_r_object",
          label = "Download R Object"
        )
      )
    )
  )
}
annotationTabUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidRow(
      tags$h4("Cell Line"),
      uiOutput(outputId = "cellAnno"),
      tags$h4("Drugs"),
      uiOutput(outputId = "drugAnno")
    )
  )
}