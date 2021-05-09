vals <- reactiveValues(users_ = 0)

server <- function(input, output, session){
  # # Home page ----------------------------------------------------------------
  # userCuide button
  observeEvent(input$toGuide, {
    updateNavbarPage(session, inputId = "topNavBar",
                     selected = "USER GUIDE")
  })
  # GetStart button
  observeEvent(input$getStart, {
    updateNavbarPage(session, inputId = "topNavBar",
                     selected = "DASHBOARD")
  })
  
  # # Define reactive variables ------------------------------------------------
  datannot <- reactiveValues(annot = NULL)
  dataReshaped <- reactiveValues(reshapeD = NULL)
  inputdatatype <- reactiveValues(type_ = "Table")
  switches <- reactiveValues(vizDR = 0, vizSyn = 0, report = 0, calSyn = 0)
  correct_baseline <- reactiveValues(correct_baseline = NULL)
  bb_plots <- reactiveValues(bar_plot = NULL, barometer = NULL)
  observeEvent(
    eventExpr = input$correct_baseline,
    handlerExpr = {
      if (input$correct_baseline == "") {
        correct_baseline$correct_baseline <- NULL
      } else {
        correct_baseline$correct_baseline <- input$correct_baseline
      }
    }
  )
  nDrug <- reactiveValues(n = 0)
  data_output <- reactive({
    isolate({dataReshaped$reshapeD})
  })
  bb_plot_param <- reactiveValues(
    conc_unit = NULL,
    drug_pair = NULL, 
    selected_panel = "response",
    selected_conc = NULL,
    selected_values = NULL)
  shinyjs::hide(selector = "a[data-value=\"doseResponseTab\"]")
  shinyjs::hide(selector = "a[data-value=\"syenrgyTab\"]")
  shinyjs::hide(selector = "a[data-value=\"downloadTab\"]")
  shinyjs::hide(selector = "a[data-value=\"sensitivityTab\"]")
  shinyjs::hide(id = "correct_baseline")
  shinyjs::hide(id = "annoSwitch")
  # Reset the input data file
  output$resettableInput <- renderUI({
    input$inputDatatype
    tagList(
      fileInput(
        inputId = 'annotfile',
        label = tags$p(
          '2. Select a file',
          bsButton("q1", label = "",
                   icon = icon(
                     "fa-question-circle", lib = "font-awesome",
                     class="fa fa-question-circle"
                   ),
                   style = "link",
                   size = "extra-small")
        ),
        accept = c('.csv', '.xlsx', '.txt'),
        
      ),
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
    )
  })
  
  # Check number of sessions
  # isolate(vals$users_ <- vals$users_ + 1)
  # session$onSessionEnded(function(){ 
  #   isolate(vals$users_ <- vals$users_ - 1)
  #   if(isolate(vals$users_ ) == 0){
  #     delCommand = paste0("rm -r ", getwd()); system(delCommand)
  #   }
  # })
  # 
  closeAll <- function() {
    shinyjs::hide(selector = "a[data-value=\"doseResponseTab\"]")
    shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
    shinyjs::hide(selector = "a[data-value=\"sensitivityTab\"]")
    shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
    shinyjs::hide(selector = "a[data-value=\"annotationTab\"]")
    shinyjs::hide(id = "correct_baseline")
    shinyjs::hide(id = "annoSwitch")
    bb_plots <- reactiveValues(bar_plot = NULL, barometer = NULL)
    bb_plot_param <- reactiveValues(
      conc_unit = NULL,
      drug_pair = NULL, 
      selected_panel = "response",
      selected_conc = NULL,
      selected_values = NULL)
    dataReshaped$reshapeD <- NULL
    datannot$annot <- NULL
    correct_baseline$correct_baseline <- NULL
    updateSwitchInput(session, inputId = "annoSwitch", value = FALSE)
    updateSelectInput(session, "selectInhVia", selected = "")
    updateSelectInput(session, inputId = "correct_baseline", selected = "")
    switches$vizDR <- 0
    switches$vizSyn <- 0
    switches$report <- 0
    switches$calSyn <- 0
    nDrug$n <- 0
    closeAlert(session, "alert1")
    closeAlert(session, "alertPD")
  }
  
  # # inputDataTab ------------------------------------------------------------
  
  # Download example data -----------------------------------------------------
  
  output$loadExData_small <- downloadHandler(
    filename = "ExampleData.zip",
    content = function(fname) {
      zip(zipfile=fname, files="./ExampleData/")
    },
    contentType = "application/zip"
  )
  
  # Input data type select box ------------------------------------------------
  observeEvent(
    eventExpr = input$inputDatatype,
    handlerExpr = {
      closeAll()
      # inputdatatype$type_ <- input$inputDatatype
    }
  )
  
  # File is loaded ------------------------------------------------------------
  observeEvent(
    eventExpr = input$annotfile,
    handlerExpr = {
      tryCatch({
        # If already loaded annotation table, drop all switches and vars
        if (!is.null(datannot$annot)) {
          closeAll()
        }
        # Check file extension
        ext <- toupper(tools::file_ext(input$annotfile$name))
        annot <- NULL
        
        if (!(ext %in% c("TXT", "CSV","XLSX"))) {
          datannot$annot <- NULL
          annot <- as.data.frame(c("Error"), stringsAsFactors = FALSE)
          colnames(annot) <- c("Error")
          createAlert(
            session, 
            anchorId = "alertannotfile",
            alertId = "alert1",
            title = "Error",
            content = "Only .csv, .txt and .xlsx are supported",
            append = FALSE
          )
        } else {
          if (input$inputDatatype == "Table") {
            if (ext == 'XLSX') {
              annot <- openxlsx::read.xlsx(
                input$annotfile$datapath
              )
            } else if (ext == "CSV") {
              annot <- read.csv(
                file = input$annotfile$datapath,
                header = TRUE,
                row.names = NULL,
                fill = TRUE
              )
            } else if (ext == "TXT") {
              annot <- read.table(
                file = input$annotfile$datapath,
                header = TRUE,
                sep = "\t",
                row.names = NULL,
                fill = TRUE
              )
            }
            
            # take care of NA's and empty rows/cols
            col_string <- sapply(annot, class) == "Character"
            annot[col_string] <- sapply(
              annot[col_string], 
              function(x) gsub("^\\s+|\\s+$", "", x)
            )
            annot <- annot[!apply(is.na(annot) | annot == "", 2, all), ] # rows with all NA
            annot <- annot[, !apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
            annot <- synergyfinder::.AdjustColumnName(annot)
            cols_num <- c(
              "block_id",
              grep("conc\\d+", colnames(annot), value = TRUE),
              "response"
            )
            annot[cols_num] <- sapply(annot[cols_num], as.numeric)
            datannot$annot <- annot
            datannot$type_ <- "Table"
          } else if (input$inputDatatype == "Matrix") {
            if (ext == 'XLSX') {
              annot <- openxlsx::read.xlsx(
                input$annotfile$datapath,
                colNames = FALSE
              )
            } else if (ext == "CSV") {
              annot <- read.csv(
                file = input$annotfile$datapath,
                header = FALSE,
                row.names = NULL,
                fill = TRUE
              )
            } else if (ext == "TXT") {
              annot <- read.table(
                file = input$annotfile$datapath,
                header = FALSE,
                sep = "\t",
                row.names = NULL,
                fill = TRUE
              )
            }
            
            # take care of NA's and empty rows/cols       
            annot <- sapply(annot, function (x) gsub("^\\s+|\\s+$", "", x))
            annot <- annot[!apply(is.na(annot) | annot == "", 1, all),] # rows with all NA
            annot <- annot[,!apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
            
            # Transform matrix to data table
            datannot$annot <- transformInputDataMatrix(data = annot)
          }
        }
      }, error = function(e) {
        datannot$annot <<- NULL 
        toastr_error(
          message = paste0(
            "Something wrong with your file that cannot be handled by application.",
            " Please check that <b>\"", input$inputDatatype,
            "\"</b> is a correct file format.",
            "For more information about input data see <b>USER GUIDE</b>.",
            "\n", e
          ),
          title = "Unhandled error occurred!",
          closeButton = TRUE,
          progressBar = TRUE,
          position = "top-right",
          preventDuplicates = TRUE,
          showDuration = 300,
          hideDuration = 1000,
          timeOut = 10000,
          extendedTimeOut = 1000,
          showEasing = "swing",
          hideEasing = "swing",
          showMethod = "fadeIn",
          hideMethod = "fadeOut"
        )
      }) # tryCatch
    }# handlerExpr
  ) # observeEvent
  
  # Render data table ---------------------------------------------------------
  observeEvent(
    eventExpr = {
      datannot$annot
    },
    handlerExpr = {
      # Show data table
      if (!is.null(datannot$annot)) {
        output$inputData <- renderDT(
          datannot$annot,
          options = list(
            scrollX = TRUE,
            scrollCollapse=TRUE,
            lengthChange = FALSE
          ),
          rownames= FALSE
        )
        shinyjs::show(id = "annoSwitch")
      } else {
        shinyjs::hide(id = "inputData")
        shinyis::hide(id = "annoSwitch")
        updateSwitchInput(
          session, inputId = "annoSwitch", value = FALSE
        )
      }
    }
  )
  
  # Annotation switch ---------------------------------------------------------
  observeEvent(
    eventExpr = {
      datannot$annot
      input$annoSwitch
    },
    handlerExpr = {
      if (!is.null(datannot$annot) & input$annoSwitch) {
        show_modal_spinner(spin = "fading-circle") 
        withCallingHandlers({
          # drugs' annotation
          drugs <- Reduce(
            f = c,
            x = datannot$annot[, 
                               grepl(
                                 "drug", 
                                 colnames(datannot$annot), 
                                 fixed = TRUE)
            ]
          )
          drugs <- na.omit(unique(drugs))
          drug_anno <- TidyComb::AnnotateDrug(drugs)
          output$drugAnno <- renderDT(
            drug_anno$drug,
            options = list(
              scrollX = TRUE,
              scrollCollapse=TRUE,
              lengthChange = FALSE
            ),
            rownames= FALSE
          )
          output$drugAnnoTarget <- renderDT(
            drug_anno$target,
            options = list(
              scrollX = TRUE,
              scrollCollapse=TRUE,
              lengthChange = FALSE
            ),
            rownames= FALSE
          )
          # cell lines' annotation
          cells <- grepl("cell", colnames(datannot$annot), fixed = TRUE)
          if (sum(cells) != 0){
            cells <- na.omit(unique(datannot$annot[, cells]))
            cell_anno <- TidyComb::AnnotateCell(cells, file = cellosauruspath)
            output$cellAnno <- renderDT(
              cell_anno,
              options = list(
                scrollX = TRUE,
                scrollCollapse=TRUE,
                lengthChange = FALSE
              ),
              rownames= FALSE
            )
          } else {
          }
        },
        message = function(m) {
          shinyjs::html(
            selector = ".modal-body div:eq(14)",
            html = m$message,
            add = FALSE)
        })
        
        remove_modal_spinner()
      }
    } # handlerExpr
  ) # observeEvent
  
  observeEvent(
    eventExpr = input$annoSwitch,
    handlerExpr = {
      if (input$annoSwitch) {
        shinyjs::show(id = "drugAnno")
        shinyjs::show(id = "cellAnno")
        shinyjs::show(id = "drugAnnoTarget")
      } else {
        shinyjs::hide(id = "drugAnno")
        shinyjs::hide(id = "cellAnno")
        shinyjs::hide(id = "drugAnnoTarget")
      }
    }
  )
  # selectInhVia --------------------------------------------------------------
  observeEvent(
    eventExpr = input$selectInhVia, 
    handlerExpr = {
      # clean settings
      shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
      shinyjs::hide(selector = "a[data-value=\"sensitivityTab\"]")
      shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
      shinyjs::hide(id = "correct_baseline")
      bb_plots <- reactiveValues(bar_plot = NULL, barometer = NULL)
      bb_plot_param <- reactiveValues(
        conc_unit = NULL,
        drug_pair = NULL,
        selected_panel = "response",
        selected_conc = NULL,
        selected_values = NULL)
      updateSelectInput(session, inputId = "correct_baseline", selected = "")
      correct_baseline$correct_baseline <- NULL
      switches$vizDR <- 0
      switches$vizSyn <- 0
      switches$report <- 0
      switches$calSyn <- 0
      nDrug$n <- 0
      closeAlert(session, "alert1")
      closeAlert(session, "alertPD")
      warning_message <- NULL
      if (input$selectInhVia != "") {
        if (!is.null(datannot$annot)) {
          closeAlert(session, "alertPD")
          dataReshaped$reshapeD <- tryCatch(
            withCallingHandlers({
            synergyfinder::ReshapeData(
              datannot$annot,
              data_type = input$selectInhVia,
              impute = TRUE
            )
          }, error = function(e) {
            closeAll()
            toastr_error(
              message = paste0(
                "Something wrong with your file that cannot be handled by",
                "application. Please check the input data formation and ",
                "content. For more information about input data see ",
                "<b>USER GUIDE</b>.\n",
                "R package message:\n",
                e
              ),
              title = "Unhandled error occurred!",
              closeButton = TRUE,
              progressBar = TRUE,
              position = "top-right",
              preventDuplicates = TRUE,
              showDuration = 300,
              hideDuration = 1000,
              timeOut = 10000,
              extendedTimeOut = 1000,
              showEasing = "swing",
              hideEasing = "swing",
              showMethod = "fadeIn",
              hideMethod = "fadeOut"
            )
          }, warning = function(w){
            warning_message <<- w$message
            invokeRestart("muffleWarning")
          }))
          
          if (!is.null(warning_message)){
          toastr_warning(
            message = warning_message,
            title = "Warning!",
            closeButton = TRUE,
            progressBar = TRUE,
            position = "top-right",
            preventDuplicates = TRUE,
            showDuration = 300,
            hideDuration = 1000,
            timeOut = 30000,
            extendedTimeOut = 1000,
            showEasing = "swing",
            hideEasing = "swing",
            showMethod = "fadeIn",
            hideMethod = "fadeOut"
          )
          }
         
          print(dataReshaped$reshapeD)
          if (!is.null(dataReshaped$reshapeD)) {
            nDrug$n <- sum(
              grepl(
                "drug",
                colnames(dataReshaped$reshapeD$drug_pairs),
                fixed = TRUE
              )
            )
            # visualize dose response
            switches$vizDR <- 1
            switches$calSyn <- 1
            output$doseResponseMenu <- renderMenu({
              menuItem("Dose Response Map", tabName = "doseResponseTab")
            })
          }
        } else {
          closeAll()
          updateSelectInput(
            session,
            "selectInhVia",
            selected = ""
          )
          createAlert(
            session,
            "noPDdata",
            "alertPD",
            title = "Error",
            content = paste0(
              "Please load required files first! or upload an example data"
            ),
            append = FALSE,
            dismiss = FALSE
          )
        }
      }
    } # handlerExpr
  ) # observeEvent
  
  # # Render plot block selector ----------------------------------------------
  observeEvent(
    eventExpr = {
      switches$vizDR
    },
    handlerExpr = {
      if (switches$vizDR == 1) {
        if (isolate(input$selectInhVia) != "" &
            !is.null(datannot$annot) &
            !is.null(dataReshaped$reshapeD)){
          closeAlert(session, "alertPD")
          drug_pairs <- dataReshaped$reshapeD$drug_pairs
          #find all drug pairs
          blocks <- 1:nrow(drug_pairs)
          names(blocks) <- sapply(
            1:nrow(drug_pairs),
            function(i) {
              paste0(
                as.character(i),
                ": ",
                paste(
                  drug_pairs[i, grepl("drug\\d+", colnames(drug_pairs))],
                  collapse = " - "
                )
              )
            }
          )
          output$plot_block <- renderUI({
            selectInput(
              inputId = "viz_block",
              label = "Block ID for plots",
              choices = blocks,
              selected = blocks[1]
            )
          }) # renderUI plot_block
          
          # render report block list
          output$report_blocks_ui <- renderUI({
            awesomeCheckboxGroup(
              inputId = "report_block",
              label = "", #"Select Blocks to Report",
              choices = blocks,
              selected = blocks
            )
          })
        }
      }
    } # handlerExpr
  ) # observeEvent
  
  # Show the selector only when the tabs doseResponseTab or synergyTab selected
  observeEvent(
    eventExpr = input$menu,
    handlerExpr = {
      if (input$menu %in% c("doseResponseTab", "synergyTab")) {
        shinyjs::show("plot_block")
        shinyjs::show("correct_baseline")
      } else {
        if (input$menu == "sensitivityTab") {
          shinyjs::show("correct_baseline")
        } else {
          shinyjs::hide("correct_baseline")
        }
        shinyjs::hide("plot_block")
      }
    }
  )
  
  # Render container for dimension reduction plot for multiple drug combo
  observeEvent(
    eventExpr = {
      switches$vizDR
      nDrug$n
    },
    handlerExpr = {
      if (switches$vizDR == 1) {
        if (nDrug$n > 2) { # Two drug combination
          output$multi_drug_DR_plot <- renderUI(
            box(
              id = "boxDoseResponseMultiDrug",
              title = "Dose Response Map (Multiple Drugs Combination)",
              solidHeader = TRUE,
              width = 6,
              height = 400,
              collapsible = TRUE,
              fluidRow(
                plotlyOutput(outputId = "multi_DR_plot") %>% 
                  withSpinner(color="#D2D2D2")
              ),
              tags$hr(),
              fluidRow(
                column(
                  width = 6,
                  br(),
                  shinyWidgets::materialSwitch(
                    inputId = "DR_multi_point",
                    label = "Show data points",
                    status = "primary",
                    right = TRUE
                  ),
                  br(),
                  tags$div(
                    style = "margin-top: -4px;",
                    colourpicker::colourInput(
                      inputId = "DR_multi_point_color",
                      label = "Color for data points",
                      value = "#DDA137"
                    )
                  )
                ),
                column(
                  width = 6,
                  colourpicker::colourInput(
                    inputId = "DR_multi_high_value_color",
                    label = "High response value color",
                    value = "#C24B40"
                  ),
                  colourpicker::colourInput(
                    inputId = "DR_multi_low_value_color",
                    label = "Low response value color",
                    value = "#2166AC"
                  )
                )
              )
            )
          )
        } else {
          output$multi_drug_DR_plots <- renderUI(
            tags$div()
          )
        }
      }
    }
  )
  
  # # Dose response tab -------------------------------------------------------
  # Render drug pair selectors ------------------------------------------------
  observeEvent(
    eventExpr = input$viz_block,
    handlerExpr = {
      # render UI DRC_drug_ui
      if (!is.null(input$viz_block)){
        drug_pairs <- dataReshaped$reshapeD$drug_pairs
        drugs <- 1:sum(grepl("drug\\d+", colnames(drug_pairs)))
        output$DRC_drug_ui <- renderUI({
          # render UI DRC_drug_ui
          names(drugs) <- drug_pairs[
            as.character(drug_pairs$block_id) == input$viz_block,
            grepl("drug\\d+", colnames(drug_pairs))]
          selectInput(
            inputId = "DRC_drug",
            label = "Drug for plot",
            choices = drugs,
            selected = drugs[1]
          )
        })
        
        # All possible 2-drug combos
        tmp <- combn(1:length(drugs), 2)
        two_drug_comb <- apply(tmp, 2, paste0, collapse = "-")
        names(two_drug_comb) <- apply(
          tmp, 
          2,
          function(x) paste(
            drug_pairs[
              as.character(drug_pairs$block_id) == input$viz_block,
              paste0("drug", x, sep = "")
            ],
            collapse = " - "
          )
        )
        
        # render UI DR_2_drugs_ui
        output$DR_2_drugs_ui <- renderUI({
          selectInput(
            inputId = "DR_2_drugs",
            label = "Drug pair for plot",
            choices = two_drug_comb,
            selected = two_drug_comb[1]
          )
        })
        
        # render UI syn_2_drugs_ui
        output$syn_2_drugs_ui <- renderUI({
          selectInput(
            inputId = "syn_2_drugs",
            label = "Drug pair for plot",
            choices = two_drug_comb,
            selected = two_drug_comb[1]
          )
        })
      }
    }
  )
  
  # Plot: Dose-response curve -------------------------------------------------
  observeEvent(
    eventExpr = {
      input$correct_baseline
      input$DRC_drug
      input$DRC_height
      input$DRC_width
      input$DRC_dot_color
      input$DRC_curve_color
      input$DRC_grid
      switches$vizDR
    },
    handlerExpr = {
      if (switches$vizDR == 1 & !is.null(input$viz_block)) {
        if (input$DRC_grid){
          output$DRC_plot <- renderPlot(
            PlotDoseResponseCurve(
              data = dataReshaped$reshapeD,
              plot_block = input$viz_block,
              drug_index = input$DRC_drug,
              plot_title = "",
              plot_subtitle = "",
              point_color = input$DRC_dot_color,
              curve_color = input$DRC_curve_color,
              plot_new = TRUE,
              record_plot = TRUE
            )
          )
          # print(dataReshaped$reshapeD)
        } else {
          output$DRC_plot <- renderPlot(
            PlotDoseResponseCurve(
              data = dataReshaped$reshapeD,
              plot_block = input$viz_block,
              drug_index = input$DRC_drug,
              grid = NULL,
              plot_title = "",
              plot_subtitle = "",
              point_color = input$DRC_dot_color,
              curve_color = input$DRC_curve_color,
              plot_new = TRUE,
              record_plot = TRUE
            )
          )
        }
      }
    }
  )
  
  output$downloadDRC <- downloadHandler(
    filename = function() {
      paste0(
        "block_",
        input$viz_block,
        "_",
        input$DRC_drug,
        "dose_response_curve.svg"
      )
    },
    content = function(file) {
      grDevices::svg(
        file, 
        width = 10, 
        height = 10,
      )
      replayPlot(
        PlotDoseResponseCurve(
          data = dataReshaped$reshapeD,
          plot_block = input$viz_block,
          drug_index = input$DRC_drug,
          grid = NULL,
          plot_title = "",
          plot_subtitle = "",
          point_color = input$DRC_dot_color,
          curve_color = input$DRC_curve_color,
          plot_new = TRUE,
          record_plot = TRUE
        )
      )
    }
  )
  
  # Plot: Dose-response map ---------------------------------------------------
  observeEvent(
    eventExpr = {
      input$correct_baseline
      input$DR_summary_statistic
      input$viz_block
      input$DR_high_value_color
      input$DR_low_value_color
      input$DR_plot_type
      input$DR_grid
      input$DR_2_drugs
      switches$vizDR
      input$DR_heatmap_label_size
      input$DR_heatmap_label_color
      input$DR_text_size
      input$DR_rep_statistic
    },
    handlerExpr = {
      if (switches$vizDR == 1 & !is.null(input$viz_block)) {
        param <- reactiveValues(drugs = c(1, 2))
        if (!is.null(input$DR_2_drugs)) {
          param$drugs <- unlist(strsplit(input$DR_2_drugs, "-"))
        }
        if(dataReshaped$reshapeD$drug_pairs[
          which(dataReshaped$reshapeD$drug_pairs$block_id == input$viz_block),
          "replicate"] & input$DR_plot_type == "heatmap"){
          shinyjs::show(id = "DR_rep_statistic")
          shinyjs::show(id = "DR_heatmap_label_size")
          shinyjs::show(id = "DR_heatmap_label_color")
          shinyjs::hide(id = "DR_grid")
          output$DR_plot <- renderPlotly(
            Plot2DrugHeatmap(
              data = dataReshaped$reshapeD,
              plot_block = input$viz_block,
              drugs = param$drugs,
              dynamic = TRUE,
              plot_title = "",
              statistic = input$DR_rep_statistic,
              summary_statistic = input$DR_summary_statistic,
              high_value_color = input$DR_high_value_color,
              low_value_color = input$DR_low_value_color,
              text_label_size_scale = input$DR_heatmap_label_size,
              text_label_color = input$DR_heatmap_label_color,
              title_text_size_scale = input$DR_text_size
            )
          )
        } else {
          if (input$DR_plot_type == "heatmap"){
            shinyjs::show(id = "DR_rep_statistic")
            shinyjs::show(id = "DR_heatmap_label_size")
            shinyjs::show(id = "DR_heatmap_label_color")
            shinyjs::hide(id = "DR_grid")
          } else {
            shinyjs::hide(id = "DR_rep_statistic")
            shinyjs::hide(id = "DR_heatmap_label_size")
            shinyjs::hide(id = "DR_heatmap_label_color")
            shinyjs::show(id = "DR_grid")
          }
          if (input$DR_plot_type == "heatmap"){
            output$DR_plot <- renderPlotly(
              Plot2DrugHeatmap(
                data = dataReshaped$reshapeD,
                plot_block = input$viz_block,
                drugs = param$drugs,
                dynamic = TRUE,
                statistic = NULL,
                plot_title = "",
                summary_statistic = input$DR_summary_statistic,
                high_value_color = input$DR_high_value_color,
                low_value_color = input$DR_low_value_color,
                text_label_size_scale = input$DR_heatmap_label_size,
                text_label_color = input$DR_heatmap_label_color,
                title_text_size_scale = input$DR_text_size
              )
            )
          } else {
            output$DR_plot <- renderPlotly({
              Plot2DrugSurface(
                data = dataReshaped$reshapeD,
                drugs = param$drugs,
                plot_block = input$viz_block,
                dynamic = TRUE,
                plot_title = "",
                summary_statistic = input$DR_summary_statistic,
                high_value_color = input$DR_high_value_color,
                low_value_color = input$DR_low_value_color,
                grid = input$DR_grid,
                text_size_scale = input$DR_text_size
              )
            })
          }
        }
        shinyjs::show(id = "correct_baseline")
      } else {
        shinyjs::hide(id = "correct_baseline")
      }
    }
  )
  
  # Plot: Multi-drug dose-response --------------------------------------------
  observeEvent(
    eventExpr = {
      input$correct_baseline
      input$viz_block
      input$DR_multi_high_value_color
      input$DR_multi_low_value_color
      input$DR_multi_point
      input$DR_multi_point_color
      switches$vizDR
      nDrug$n
    },
    handlerExpr = {
      if (switches$vizDR == 1 & !is.null(input$viz_block) & nDrug$n > 2) {
        output$multi_DR_plot <- renderPlotly(
          PlotMultiDrugSurface(
            data = dataReshaped$reshapeD,
            plot_block = input$viz_block,
            plot_value = "response",
            plot_title = "",
            high_value_color = input$DR_multi_high_value_color,
            low_value_color = input$DR_multi_low_value_color,
            show_data_points = input$DR_multi_point,
            point_color = input$DR_multi_point_color
          )
        )
      } else if (nDrug$n <= 2) {
        shinyjs::hide(id = "multi_DR_plot")
      }
    }
  )
  
  # Calculate synergy scores and sensitivity score --------------------------
  observeEvent(
    eventExpr = {
      input$correct_baseline
      switches$calSyn
    },
    handlerExpr = {
      if (!is.null(correct_baseline$correct_baseline) &
          switches$calSyn == 1 & !is.null(dataReshaped$reshapeD)) {
        show_modal_spinner(spin = "fading-circle") 
        withCallingHandlers({
          dataReshaped$reshapeD <- CalculateSynergy(
            dataReshaped$reshapeD,
            correct_baseline = correct_baseline$correct_baseline,
            method = c("ZIP", "HSA", "Loewe", "Bliss")
          )
          dataReshaped$reshapeD <- CalculateSensitivity(
            dataReshaped$reshapeD,
            correct_baseline = correct_baseline$correct_baseline
          )
        },
        message = function(m) {
          shinyjs::html(
            selector = ".modal-body div:eq(14)",
            html = m$message,
            add = FALSE)
        }
        )
        remove_modal_spinner()
        
        switches$vizSyn <- 1
        # switches$calSyn <- 0
      } else {
        shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
      }
    }
  )
  
  # # synergyTab --------------------------------------------------------------
  observeEvent(
    eventExpr = {
      switches$vizSyn
    },
    handlerExpr = {
      if (switches$vizSyn == 1) {
        output$synergyMenu <- renderMenu({
          menuItem("Synergy Score", tabName = "synergyTab")
        })
        output$sensitivityMenu <- renderMenu({
          menuItem("Sensitivity Score", tabName = "sensitivityTab")
        })
        output$reportMenu <- renderMenu({
          menuItem("Download Reports", tabName = "reportTab")
        })
        shinyjs::show(selector = "a[data-value=\"synergyTab\"]")
        shinyjs::show(selector = "a[data-value=\"sensitivityTab\"]")
        shinyjs::show(selector = "a[data-value=\"reportTab\"]")
      } else {
        shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
        shinyjs::hide(selector = "a[data-value=\"sensitivityTab\"]")
        shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
      }
    }
  )
  
  # Plot: Bar barometer plots -------------------------------------------------
  observeEvent(
    eventExpr = {
      switches$vizSyn
      dataReshaped$reshapeD
      input$correct_baseline
      input$viz_block
      input$bb_panel_title_size
      input$bb_pos_value_color
      input$bb_neg_value_color
      input$bb_axis_text_size
      input$bb_highlight_label_size
      input$bb_highlight_pos_color
      input$bb_highlight_neg_color
      input$bb_barometer_color
    },
    handlerExpr = {
      if ("synergy_scores" %in% names(dataReshaped$reshapeD) & 
          switches$vizSyn == 1 &
          !is.null(input$viz_block)) {
        data <- dataReshaped$reshapeD
        if (is.null(bb_plot_param$drug_pair)) {
          bb_plot_param$drug_pair <- dataReshaped$reshapeD$drug_pairs[
            dataReshaped$reshapeD$drug_pairs$block_id == input$viz_block,
            grepl(
              "drug",
              colnames(dataReshaped$reshapeD$drug_pairs),
              fixed = TRUE
            )
          ]
        }
        if (is.null(bb_plot_param$conc_unit)) {
          bb_plot_param$conc_unit <- dataReshaped$reshapeD$drug_pairs[
            dataReshaped$reshapeD$drug_pairs$block_id == input$viz_block,
            grepl(
              "conc_unit",
              colnames(dataReshaped$reshapeD$drug_pairs),
              fixed = TRUE
            )
          ]
        }
        bb_plots$bar_plot <- PlotMultiDrugBar(
          data,
          plot_block = input$viz_block,
          plot_value = c(
            "response", "ZIP_synergy", "HSA_synergy",
            "Bliss_synergy", "Loewe_synergy"
          ),
          highlight_row = bb_plot_param$selected_conc,
          sort_by = bb_plot_param$selected_panel,
          panel_title_size = input$bb_panel_title_size,
          axis_text_size = input$bb_axis_text_size,
          highlight_label_size = input$bb_highlight_label_size,
          highlight_pos_color = input$bb_highlight_pos_color,
          highlight_neg_color = input$bb_highlight_neg_color,
          pos_value_color = input$bb_pos_value_color,
          neg_value_color = input$bb_neg_value_color,
          data_table = TRUE
        )
        barPlotHeight <- reactive(length(unique(bb_plots$bar_plot$data_table$id)) * 10)
        output$syn_bar_plot_ui <- renderUI({
          plotOutput(
            outputId = "syn_bar_plot",
            click = "syn_bar_plot_click",
            dblclick = "syn_bar_plot_dbclick",
            height = paste0(
              as.character(barPlotHeight()), "px")
          ) %>% 
            withSpinner(color="#D2D2D2")
        })
        if (is.null(bb_plot_param$selected_values)) {
          bb_plot_param$selected_values <- bb_plots$bar_plot$data_table[
            bb_plots$bar_plot$data_table$id == 1,
          ]
        }
        if (is.null(bb_plot_param$selected_conc)) {
          bb_plot_param$selected_conc <- unlist(bb_plot_param$selected_values[,
            sort(grep("conc", colnames(bb_plot_param$selected_values), value = TRUE))
          ])
        }
        bb_plots$barometer <- PlotBarometer(
          data,
          plot_block = input$viz_block,
          plot_concs = bb_plot_param$selected_conc,
          color_bar_color = input$bb_barometer_color,
          needle_text_offset = 5,
          show_concs = FALSE
        )
        # Double click to sort
        observeEvent(input$syn_bar_plot_dbclick, {
          if (is.null(input$syn_bar_plot_dbclick$panelvar1)){
            bb_plots$bar_plot <- PlotMultiDrugBar(
              data,
              plot_block = input$viz_block,
              plot_value = c(
                "response", "ZIP_synergy", "HSA_synergy",
                "Bliss_synergy", "Loewe_synergy"
              ),
              highlight_row = bb_plot_param$selected_conc,
              sort_by = bb_plot_param$selected_panel,
              panel_title_size = input$bb_panel_title_size,
              axis_text_size = input$bb_axis_text_size,
              highlight_label_size = input$bb_highlight_label_size,
              highlight_pos_color = input$bb_highlight_pos_color,
              highlight_neg_color = input$bb_highlight_neg_color,
              pos_value_color = input$bb_pos_value_color,
              neg_value_color = input$bb_neg_value_color,
              data_table = TRUE
            )
            bb_plots$barometer <- PlotBarometer(
              data,
              plot_block = input$viz_block,
              plot_concs = bb_plot_param$selected_conc,
              color_bar_color = input$bb_barometer_color,
              needle_text_offset = 5,
              show_concs = FALSE
            )
          } else {
            d <- grepl(sub("\n.*", "", input$syn_bar_plot_dbclick$panelvar1),
                       bb_plot_param$drug_pair)
            if (sum(d) > 0) {
              bb_plot_param$selected_panel <- sub("drug", "conc", names(bb_plot_param$drug_pair)[d])
            } else {
              bb_plot_param$selected_panel <- switch(
                input$syn_bar_plot_dbclick$panelvar1,
                "Response\n(% inhibition)" = "response",
                "ZIP Synergy Score" = "ZIP_synergy",
                "HSA Synergy Score" = "HSA_synergy",
                "Loewe Synergy Score" = "Loewe_synergy",
                "Bliss Synergy Score" = "Bliss_synergy"
              )
            }
            bb_plots$bar_plot <- PlotMultiDrugBar(
              data,
              plot_block = input$viz_block,
              plot_value = c(
                "response", "ZIP_synergy", "HSA_synergy",
                "Bliss_synergy", "Loewe_synergy"
              ),
              highlight_row = bb_plot_param$selected_conc,
              sort_by = bb_plot_param$selected_panel,
              panel_title_size = input$bb_panel_title_size,
              axis_text_size = input$bb_axis_text_size,
              highlight_label_size = input$bb_highlight_label_size,
              highlight_pos_color = input$bb_highlight_pos_color,
              highlight_neg_color = input$bb_highlight_neg_color,
              pos_value_color = input$bb_pos_value_color,
              neg_value_color = input$bb_neg_value_color,
              data_table = TRUE
            )
            bb_plots$barometer <- PlotBarometer(
              data,
              plot_block = input$viz_block,
              plot_concs = bb_plot_param$selected_conc,
              color_bar_color = input$bb_barometer_color,
              needle_text_offset = 5,
              show_concs = FALSE
            )
          }
        })
        # Click to highlight
        observeEvent(input$syn_bar_plot_click, {
          bb_plot_param$selected_values <- bb_plots$bar_plot$data_table[
            bb_plots$bar_plot$data_table$id == round(input$syn_bar_plot_click$y),
          ]
          bb_plot_param$selected_conc <- unlist(bb_plot_param$selected_values[,
            sort(grep("conc", colnames(bb_plot_param$selected_values), value = TRUE))
          ])
          
          bb_plots$bar_plot <- PlotMultiDrugBar(
            data,
            plot_block = input$viz_block,
            plot_value = c(
              "response", "ZIP_synergy", "HSA_synergy",
              "Bliss_synergy", "Loewe_synergy"
            ),
            highlight_row = bb_plot_param$selected_conc,
            sort_by = bb_plot_param$selected_panel,
            panel_title_size = input$bb_panel_title_size,
            axis_text_size = input$bb_axis_text_size,
            highlight_label_size = input$bb_highlight_label_size,
            highlight_pos_color = input$bb_highlight_pos_color,
            highlight_neg_color = input$bb_highlight_neg_color,
            pos_value_color = input$bb_pos_value_color,
            neg_value_color = input$bb_neg_value_color,
            data_table = TRUE
          )
          bb_plots$barometer <- PlotBarometer(
            data,
            plot_block = input$viz_block,
            plot_concs = bb_plot_param$selected_conc,
            needle_text_offset = 5,
            color_bar_color = input$bb_barometer_color,
            show_concs = FALSE
          )
        })
        
        observe({
          output$syn_bar_plot <- renderPlot(
            {bb_plots$bar_plot$plot})
          output$syn_barometer <- renderPlot({bb_plots$barometer})
          headerCallback <- c(
            "function(thead, data, start, end, display){",
            "  $('th', thead).css('border-bottom', 'none');",
            "}"
          )
          output$syn_barometer_values <- renderDT(
            { 
              df <- bb_plot_param$selected_values %>% 
                dplyr::select(
                  dplyr::starts_with("conc"),
                  "ZIP Synergy Score:" = "ZIP_synergy",
                  "Loewe Synergy Score:" = "Loewe_synergy",
                  "Bliss Synergy Score:" = "Bliss_synergy",
                  "HSA Synergy Score:" = "HSA_synergy"
                  ) %>% 
                round(digits = 2)
              
              for (i in 1:ncol(bb_plot_param$drug_pair)) {
                df[, which(colnames(df) == paste0("conc", i))] <- paste(
                  as.character(df[, which(colnames(df) == paste0("conc", i))]),
                  bb_plot_param$conc_unit[, paste0("conc_unit", i)]
                )
                colnames(df)[which(colnames(df) == paste0("conc", i))] <- paste0(
                  bb_plot_param$drug_pair[1, paste0("drug", i)],
                  ":")
              }
              
              DT::datatable(
                data = t(df),
                class = "compact",
                rownames = TRUE,
                colnames = c(""),
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options = list(
                  dom = 't',
                  ordering = FALSE,
                  paging = FALSE,
                  searching = FALSE,
                  headerCallback = JS(headerCallback)
                )
              )
            }
          )
        })
      }
    }
  )
  
  output$downloadBar <- downloadHandler(
    filename = function() {
      paste0(
        "SynergyFinder_bar_plot_block_",
        input$viz_block,
        ".svg"
      )
    },
    content = function(file) {
      ggsave(file, plot = bb_plots$bar_plot$plot, device = "svg")
    }
  )
  
  output$downloadBarometer <- downloadHandler(
    filename = function() {
      paste0(
        "SynergyFinder_barometer_block_",
        input$viz_block,
        ".svg"
      )
    },
    content = function(file) {
      ggsave(file, plot = bb_plots$barometer, device = "svg")
    }
  )
  # # Plot: Synergy Map -------------------------------------------------------
  # Plot: Heatmap or 3D surface
  observeEvent(
    eventExpr = {
      input$correct_baseline
      dataReshaped$reshapeD
      input$viz_block
      input$syn_high_value_color
      input$syn_low_value_color
      input$syn_summary_statistic
      input$syn_plot_type
      input$syn_2_drugs
      switches$vizSyn
      input$syn_heatmap_label_size
      input$syn_heatmap_label_color
      input$syn_text_size
      input$syn_rep_statistic
    },
    handlerExpr = {
      if ("synergy_scores" %in% names(dataReshaped$reshapeD) &
          switches$vizSyn == 1 &
          !is.null(input$viz_block)) {
        drugs <- reactiveValues(drugs = c(1, 2))
        if (!is.null(input$syn_2_drugs)) {
          drugs$drugs <- unlist(strsplit(input$syn_2_drugs, "-"))
        }
        if (dataReshaped$reshapeD$drug_pairs[
          which(dataReshaped$reshapeD$drug_pairs$block_id == input$viz_block),
          "replicate"] & input$syn_plot_type == "heatmap"){
          shinyjs::show(id = "syn_rep_statistic")
        } else {
          shinyjs::hide(id = "syn_rep_statistic")
        }
        
        if (input$syn_plot_type == "heatmap") {
          shinyjs::show(id = "syn_heatmap_label_size")
          shinyjs::show(id = "syn_heatmap_label_color")
        } else {
          shinyjs::hide(id = "syn_heatmap_label_size")
          shinyjs::hide(id = "syn_heatmap_label_color")
        }
        
        if (input$syn_plot_type == "3D") {
          shinyjs::show(id = "syn_grid")
        } else {
          shinyjs::hide(id = "syn_grid")
        }
        # ZIP
        output$syn_ZIP_plot <- renderPlotly({
          p <- PlotSynergy(
            data = dataReshaped$reshapeD,
            block_ids = input$viz_block,
            drugs = drugs$drugs,
            type = input$syn_plot_type,
            method = "ZIP",
            dynamic = TRUE,
            plot_title = "ZIP",
            summary_statistic = input$syn_summary_statistic,
            high_value_color = input$syn_high_value_color,
            low_value_color = input$syn_low_value_color,
            text_size_scale = input$syn_text_size,
            heatmap_text_label_color = input$syn_heatmap_label_color,
            heatmap_text_label_size_scale = input$syn_heatmap_label_size,
            statistic = input$syn_rep_statistic,
            surface_grid = input$syn_grid,
            display = FALSE
          )
          p[[1]]
        })
        # Loewe
        output$syn_Loewe_plot <- renderPlotly({
          p <- PlotSynergy(
            data = dataReshaped$reshapeD,
            block_ids = input$viz_block,
            drugs = drugs$drugs,
            type = input$syn_plot_type,
            method = "Loewe",
            dynamic = TRUE,
            plot_title = "Loewe",
            summary_statistic = input$syn_summary_statistic,
            high_value_color = input$syn_high_value_color,
            low_value_color = input$syn_low_value_color,
            text_size_scale = input$syn_text_size,
            heatmap_text_label_color = input$syn_heatmap_label_color,
            heatmap_text_label_size_scale = input$syn_heatmap_label_size,
            statistic = input$syn_rep_statistic,
            surface_grid = input$syn_grid,
            display = FALSE
          )
          if (input$syn_plot_type == "2D"){
            ggplotly(p[[1]])
          } else {
            p[[1]]
          }
        })
        # Bliss
        output$syn_Bliss_plot <- renderPlotly({
          p <- PlotSynergy(
            data = dataReshaped$reshapeD,
            block_ids = input$viz_block,
            drugs = drugs$drugs,
            type = input$syn_plot_type,
            method = "Bliss",
            dynamic = TRUE,
            plot_title = "Bliss",
            summary_statistic = input$syn_summary_statistic,
            high_value_color = input$syn_high_value_color,
            low_value_color = input$syn_low_value_color,
            text_size_scale = input$syn_text_size,
            heatmap_text_label_color = input$syn_heatmap_label_color,
            heatmap_text_label_size_scale = input$syn_heatmap_label_size,
            statistic = input$syn_rep_statistic,
            surface_grid = input$syn_grid,
            display = FALSE
          )
          p[[1]]
        })
        # HSA
        output$syn_HSA_plot <- renderPlotly({
          p <- PlotSynergy(
            data = dataReshaped$reshapeD,
            block_ids = input$viz_block,
            drugs = drugs$drugs,
            type = input$syn_plot_type,
            method = "HSA",
            dynamic = TRUE,
            plot_title = "HSA",
            summary_statistic = input$syn_summary_statistic,
            high_value_color = input$syn_high_value_color,
            low_value_color = input$syn_low_value_color,
            text_size_scale = input$syn_text_size,
            heatmap_text_label_color = input$syn_heatmap_label_color,
            heatmap_text_label_size_scale = input$syn_heatmap_label_size,
            statistic = input$syn_rep_statistic,
            surface_grid = input$syn_grid,
            display = FALSE
          )
          p[[1]]
        })
      }
    }
  )
  
  # Plot: Multi-drug Synergy Map ----------------------------------------------
  observeEvent(
    eventExpr = {
      input$viz_block
      switches$vizSyn
      # nDrug$n
    },
    handlerExpr = {
      if (switches$vizSyn == 1 & !is.null(input$viz_block)) {
        if (nDrug$n> 2) { # Two drug combination
          output$multi_drug_syn_plots <- renderUI(
            tagList(
              box(
                id = "BoxMultiSynergyScorePlot",
                title = "Synergy Map (Dimention Reduction)",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                fluidRow(
                  column(
                    width = 6,
                    plotlyOutput(outputId = "syn_multi_ZIP_plot") %>% 
                      withSpinner(color="#D2D2D2"),
                    plotlyOutput(outputId = "syn_multi_Loewe_plot") %>% 
                      withSpinner(color="#D2D2D2")
                  ),
                  column(
                    width = 6,
                    plotlyOutput(outputId = "syn_multi_HSA_plot") %>% 
                      withSpinner(color="#D2D2D2"),
                    plotlyOutput(outputId = "syn_multi_Bliss_plot") %>% 
                      withSpinner(color="#D2D2D2")
                  )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 3,
                    shinyWidgets::materialSwitch(
                      inputId = "syn_multi_point",
                      label = "Show data points",
                      status = "primary",
                      right = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    colourpicker::colourInput(
                      inputId = "syn_multi_point_color",
                      label = "Color for data points",
                      value = "#DDA137"
                    )
                  ),
                  column(
                    width = 3,
                    colourpicker::colourInput(
                      inputId = "syn_multi_high_value_color",
                      label = "Synergy effect color",
                      value = "#C24B40"
                    )
                  ),
                  column(
                    width = 3,
                    colourpicker::colourInput(
                      inputId = "syn_multi_low_value_color",
                      label = "Antagnositic effect color",
                      value = "#2166AC"
                    )
                  )
                )
              )
            )
          )
        } else {
          output$multi_drug_syn_plots <- renderUI(
            tags$div()
          )
        }
      }
    }
  )
  
  observeEvent(
    eventExpr = {
      input$correct_baseline
      input$viz_block
      input$syn_multi_high_value_color
      input$syn_multi_low_value_color
      input$syn_multi_point
      input$syn_multi_point_color
      switches$vizSyn
      nDrug$n
    },
    handlerExpr = {
      if (switches$vizSyn == 1 & !is.null(input$viz_block) & nDrug$n > 2) {
        output$syn_multi_ZIP_plot <- renderPlotly(
          PlotMultiDrugSurface(
            data = dataReshaped$reshapeD,
            plot_block = input$viz_block,
            plot_value = "ZIP_synergy",
            plot_title = "ZIP",
            high_value_color = input$syn_multi_high_value_color,
            low_value_color = input$syn_multi_low_value_color,
            show_data_points = input$syn_multi_point,
            point_color = input$syn_multi_point_color
          )
        )
        output$syn_multi_Loewe_plot <- renderPlotly(
          PlotMultiDrugSurface(
            data = dataReshaped$reshapeD,
            plot_block = input$viz_block,
            plot_value = "Loewe_synergy",
            plot_title = "Loewe",
            high_value_color = input$syn_multi_high_value_color,
            low_value_color = input$syn_multi_low_value_color,
            show_data_points = input$syn_multi_point,
            point_color = input$syn_multi_point_color
          )
        )
        output$syn_multi_Bliss_plot <- renderPlotly(
          PlotMultiDrugSurface(
            data = dataReshaped$reshapeD,
            plot_block = input$viz_block,
            plot_value = "Bliss_synergy",
            plot_title = "Bliss",
            high_value_color = input$syn_multi_high_value_color,
            low_value_color = input$syn_multi_low_value_color,
            show_data_points = input$syn_multi_point,
            point_color = input$syn_multi_point_color
          )
        )
        output$syn_multi_HSA_plot <- renderPlotly(
          PlotMultiDrugSurface(
            data = dataReshaped$reshapeD,
            plot_block = input$viz_block,
            plot_value = "HSA_synergy",
            plot_title = "HSA",
            high_value_color = input$syn_multi_high_value_color,
            low_value_color = input$syn_multi_low_value_color,
            show_data_points = input$syn_multi_point,
            point_color = input$syn_multi_point_color
          )
        )
      }
    }
  )
  
  # # sensitivity Tab ---------------------------------------------------------
  observeEvent(
    eventExpr = {
      input$correct_baseline
      dataReshaped$reshapeD
      switches$vizSyn
      input$ss_point_color
      input$ss_point_size
      input$ss_show_label
      input$ss_label_color
      input$ss_label_size
    },
    handlerExpr = {
      if ("css" %in% colnames(dataReshaped$reshapeD$drug_pairs) &
          switches$vizSyn == 1) {
        # S-S plot table
        output$ss_ZIP_plot <- renderPlotly({
          PlotSensitiveSynergy(
            data = dataReshaped$reshapeD,
            plot_synergy = "ZIP",
            point_color = input$ss_point_color,
            point_size = input$ss_point_size,
            show_labels = input$ss_show_label,
            point_label_color = input$ss_label_color,
            label_size = input$ss_label_size,
            dynamic = TRUE
          )
        })
        output$ss_Loewe_plot <- renderPlotly({
          PlotSensitiveSynergy(
            data = dataReshaped$reshapeD,
            plot_synergy = "Loewe",
            point_color = input$ss_point_color,
            point_size = input$ss_point_size,
            show_labels = input$ss_show_label,
            point_label_color = input$ss_label_color,
            label_size = input$ss_label_size,
            dynamic = TRUE
          )
        })
        output$ss_Bliss_plot <- renderPlotly({
          PlotSensitiveSynergy(
            data = dataReshaped$reshapeD,
            plot_synergy = "Bliss",
            point_color = input$ss_point_color,
            point_size = input$ss_point_size,
            show_labels = input$ss_show_label,
            point_label_color = input$ss_label_color,
            label_size = input$ss_label_size,
            dynamic = TRUE
          )
        })
        output$ss_HSA_plot <- renderPlotly({
          PlotSensitiveSynergy(
            data = dataReshaped$reshapeD,
            plot_synergy = "HSA",
            point_color = input$ss_point_color,
            point_size = input$ss_point_size,
            show_labels = input$ss_show_label,
            point_label_color = input$ss_label_color,
            label_size = input$ss_label_size,
            dynamic = TRUE
          )
        })
        output$summaryTable <- renderDT(
          {dataReshaped$reshapeD$drug_pairs%>% 
              dplyr::select(
                -input_type, -replicate,
                -dplyr::contains("synergy"),
                -dplyr::contains("response"),
                -dplyr::contains("conc_unit")) %>%
              dplyr::mutate_if(is.numeric, round, 2)
          }, 
          options = list(scrollX = TRUE, scrollCollapse=TRUE),
          rownames= FALSE
        )
      }
    }
  )
  # # report Tab --------------------------------------------------------------
  # Static PDF report ---------------------------------------------------------
  output$static_report <- downloadHandler(
    filename = "SynergyFinder_report.pdf",
    content = function(file) {
      shiny::withProgress(
        message = paste0("Generating report ..."),
        value = 0,
        {
          subdir <- gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time()))))
          dir.create(file.path(reportspath, subdir), recursive = TRUE)
          tempReport <- file.path(
            reportspath, 
            subdir,
            "static_report_template.Rmd")
          file.copy(
            "./doc/static_report_template.Rmd",
            tempReport,
            overwrite = TRUE)
          shiny::incProgress(1/10)
          # Rmarkdown can't render plots from recordPlot function
          # Here we write png for dose-response curve and then read them to
          # Rmarkdown
          ndrug <- sum(grepl("drug", colnames(dataReshaped$reshapeD$drug_pairs)))
          nblock <- length(input$report_block)
          plots <- c()
          for (b in input$report_block) {
            shiny::incProgress(8/(10 * nblock))
            for (i in 1:ndrug) {
              if (input$DRC_grid) {
                p <- PlotDoseResponseCurve(
                  data =dataReshaped$reshapeD,
                  plot_block = b,
                  drug_index = i,
                  point_color = input$DRC_dot_color,
                  curve_color = input$DRC_curve_color
                )
                f <- paste0("DRC_block_", b,"_drug_", i, ".png")
                plots <- c(plots, f)
                png(
                  filename = paste0(reportspath, subdir, "/", f),
                  height = 600,
                  width = 600
                )
                replayPlot(p)
                dev.off()
              } else {
                p <- PlotDoseResponseCurve(
                  data =dataReshaped$reshapeD,
                  plot_block = b,
                  drug_index = i,
                  grid = NULL,
                  point_color = input$DRC_dot_color,
                  curve_color = input$DRC_curve_color
                )
                f <- paste0("DRC_block_", b,"_drug_", i, ".png")
                plots <- c(plots, f)
                png(
                  filename = paste0(reportspath, subdir, "/", f),
                  height = 600,
                  width = 600
                )
                replayPlot(p)
                dev.off()
              }
            }
          }
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = list(
              data = dataReshaped$reshapeD,
              blocks = input$report_block,
              correct_baseline = input$correct_baseline,
              DRC_plots = plots,
              DR_multi_high_value_color = input$DR_multi_high_value_color,
              DR_multi_low_value_color = input$DR_multi_low_value_color,
              DR_multi_point = input$DR_multi_point,
              DR_multi_point_color = input$DR_multi_point_color,
              DR_plot_type = input$DR_plot_type,
              DR_rep_statistic = input$DR_rep_statistic,
              DR_summary_statistic = input$DR_summary_statistic,
              DR_high_value_color = input$DR_high_value_color,
              DR_low_value_color = input$DR_low_value_color,
              DR_heatmap_label_color = input$DR_heatmap_label_color,
              DR_grid = input$DR_grid,
              syn_multi_high_value_color = input$syn_multi_high_value_color,
              syn_multi_low_value_color = input$syn_multi_low_value_color,
              syn_multi_point = input$syn_multi_point,
              syn_multi_point_color = input$syn_multi_point_color,
              syn_plot_type = input$syn_plot_type,
              syn_summary_statistic = input$syn_summary_statistic,
              syn_high_value_color = input$syn_high_value_color,
              syn_low_value_color = input$syn_low_value_color,
              syn_heatmap_label_color = input$syn_heatmap_label_color,
              syn_rep_statistic = input$syn_rep_statistic,
              syn_grid = input$syn_grid,
              bb_panel_title_size = input$bb_panel_title_size,
              bb_axis_text_size = input$bb_axis_text_size,
              bb_highlight_label_size = input$bb_highlight_label_size,
              bb_highlight_pos_color = input$bb_highlight_pos_color,
              bb_highlight_neg_color = input$bb_highlight_neg_color,
              bb_pos_value_color = input$bb_pos_value_color,
              bb_neg_value_color = input$bb_neg_value_color,
              ss_point_color = input$ss_point_color,
              ss_point_size = input$ss_point_size,
              ss_show_label = input$ss_show_label,
              ss_label_color = input$ss_label_color,
              ss_label_size = input$ss_label_size),
            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
  # Dynamic PDF report --------------------------------------------------------
  output$dynamic_report <- downloadHandler(
    filename = "SynergyFinder_report.html",
    content = function(file) {
      shiny::withProgress(
        message = paste0("Generating report ..."),
        value = 0,
        {
          subdir <- gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time()))))
          dir.create(file.path(reportspath, subdir), recursive = TRUE)
          tempReport <- file.path(
            reportspath, 
            subdir,
            "dynamic_report_template.Rmd")
          file.copy(
            "./doc/dynamic_report_template.Rmd",
            tempReport,
            overwrite = TRUE)
          shiny::incProgress(1/10)
          # Rmarkdown can't render plots from recordPlot function
          # Here we write png for dose-response curve and then read them to
          # Rmarkdown
          ndrug <- sum(grepl("drug", colnames(dataReshaped$reshapeD$drug_pairs)))
          nblock <- length(input$report_block)
          plots <- c()
          for (b in input$report_block) {
            shiny::incProgress(8/(10 * nblock))
            for (i in 1:ndrug) {
              if (input$DRC_grid) {
                p <- PlotDoseResponseCurve(
                  data =dataReshaped$reshapeD,
                  plot_block = b,
                  drug_index = i,
                  point_color = input$DRC_dot_color,
                  curve_color = input$DRC_curve_color
                )
                f <- paste0("DRC_block_", b,"_drug_", i, ".png")
                plots <- c(plots, f)
                png(
                  filename = paste0(reportspath, subdir, "/", f),
                  height = 600,
                  width = 600
                )
                replayPlot(p)
                dev.off()
              } else {
                p <- PlotDoseResponseCurve(
                  data =dataReshaped$reshapeD,
                  plot_block = b,
                  drug_index = i,
                  grid = NULL,
                  point_color = input$DRC_dot_color,
                  curve_color = input$DRC_curve_color
                )
                f <- paste0("DRC_block_", b,"_drug_", i, ".png")
                plots <- c(plots, f)
                png(
                  filename = paste0(reportspath, subdir, "/", f),
                  height = 600,
                  width = 600
                )
                replayPlot(p)
                dev.off()
              }
            }
          }
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = list(
              data = dataReshaped$reshapeD,
              blocks = input$report_block,
              correct_baseline = input$correct_baseline,
              DRC_plots = plots,
              DR_multi_high_value_color = input$DR_multi_high_value_color,
              DR_multi_low_value_color = input$DR_multi_low_value_color,
              DR_multi_point = input$DR_multi_point,
              DR_multi_point_color = input$DR_multi_point_color,
              DR_plot_type = input$DR_plot_type,
              DR_rep_statistic = input$DR_rep_statistic,
              DR_summary_statistic = input$DR_summary_statistic,
              DR_high_value_color = input$DR_high_value_color,
              DR_low_value_color = input$DR_low_value_color,
              DR_heatmap_label_color = input$DR_heatmap_label_color,
              DR_grid = input$DR_grid,
              syn_multi_high_value_color = input$syn_multi_high_value_color,
              syn_multi_low_value_color = input$syn_multi_low_value_color,
              syn_multi_point = input$syn_multi_point,
              syn_multi_point_color = input$syn_multi_point_color,
              syn_plot_type = input$syn_plot_type,
              syn_summary_statistic = input$syn_summary_statistic,
              syn_high_value_color = input$syn_high_value_color,
              syn_low_value_color = input$syn_low_value_color,
              syn_heatmap_label_color = input$syn_heatmap_label_color,
              syn_rep_statistic = input$syn_rep_statistic,
              syn_grid = input$syn_grid,
              bb_panel_title_size = input$bb_panel_title_size,
              bb_axis_text_size = input$bb_axis_text_size,
              bb_highlight_label_size = input$bb_highlight_label_size,
              bb_highlight_pos_color = input$bb_highlight_pos_color,
              bb_highlight_neg_color = input$bb_highlight_neg_color,
              bb_pos_value_color = input$bb_pos_value_color,
              bb_neg_value_color = input$bb_neg_value_color,
              ss_point_color = input$ss_point_color,
              ss_point_size = input$ss_point_size,
              ss_show_label = input$ss_show_label,
              ss_label_color = input$ss_label_color,
              ss_label_size = input$ss_label_size),
            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
  # Download: data tables -----------------------------------------------------
  output$download_summary_table <- downloadHandler(
    filename <- function() {
      paste0(
        "SynergyFinder_summary_table_", 
        Sys.Date(), 
        ".", 
        tolower(input$download_table_format)
      )
    },
    content <- function(file) {
      table <- dataReshaped$reshapeD$drug_pairs %>% 
        dplyr::select(
          block_id, drug1, drug2, ZIP_synergy,
          ZIP_synergy_p_value, HSA_synergy, HSA_synergy_p_value, Loewe_synergy,
          Loewe_synergy_p_value, Bliss_synergy, Bliss_synergy_p_value, ic50_1,
          ic50_2, ri_1, ri_2, css1_ic502, css2_ic501, css
        )
      if (input$download_table_format == "XLSX") {
        writexl::write_xlsx(table, path = file)
      } else if (input$download_table_format == "CSV") {
        write.csv(table, file = file, row.names = FALSE)
      } else if (input$download_table_format == "TXT") {
        write.table(table, file = file, sep = "\t", row.names = FALSE)
      }
    }
  )
  output$download_synergy_table <- downloadHandler(
    filename <- function() {
      paste0(
        "SynergyFinder_summary_table_", 
        Sys.Date(), 
        ".", 
        tolower(input$download_table_format)
      )
    },
    content <- function(file) {
      table <- dataReshaped$reshapeD$synergy_scores
      if (input$download_table_format == "XLSX") {
        writexl::write_xlsx(table, path = file)
      } else if (input$download_table_format == "CSV") {
        write.csv(table, file = file, row.names = FALSE)
      } else if (input$download_table_format == "TXT") {
        write.table(table, file = file, sep = "\t", row.names = FALSE)
      }
    }
  )
  # Download R object ---------------------------------------------------------
  output$download_r_object <- downloadHandler(
    filename <- function(){
      paste0(
        "SynergyFinder_r_object_", 
        Sys.Date(), 
        ".rds"
      )
    },
    content = function(file) {
      saveRDS(data_output(), file = file)
    }
  )
}