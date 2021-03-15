vals <- reactiveValues(users_ = 0)

server <- function(input, output, session){
  # Home page ------------------------------------------------------------------
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
  shinyjs::hide(selector = "a[data-value=\"doseresponseTab\"]")
  
  # Dashboard page -------------------------------------------------------------
  # reactive variables
  datannot <- reactiveValues(annot = NULL, outList = NULL, type_ = NULL)
  dataReshaped <- reactiveValues(reshapeD = NULL)
  scores <- reactiveValues(scores = NULL)
  scoreofthepart <- reactiveValues(scores = NULL)
  inputdatatype <- reactiveValues(type_ = "Table")
  
  LETTERS <- unique(as.character(sapply(LETTERS, function(x) {
    paste0(x, sapply(LETTERS, function(x) paste0(x, LETTERS)))
    })))
  shinyjs::hide(selector = "a[data-value=\"doseresponseTab\"]")
  shinyjs::hide(selector = "a[data-value=\"syenrgyTab\"]")
  shinyjs::hide(selector = "a[data-value=\"downloadTab\"]")
  
  # Reset the input data file
  output$resettableInput <- renderUI({
    input$inputDatatype
    tagList(
      fileInput(
        inputId = 'annotfile',
        label = tags$p(
          '2. Select a file',
          bsButton("q1", label = "",
                   icon = icon("fa-question-circle", lib = "font-awesome",
                               class="fa fa-question-circle"
                               ),
                   style = "link",
                   size = "small")
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
  isolate(vals$users_ <- vals$users_ + 1)
  session$onSessionEnded(function(){ 
    isolate(vals$users_ <- vals$users_ - 1)
    if(isolate(vals$users_ ) == 0){
      #delCommand = paste0("rm -r ", getwd()); system(delCommand)
    }
  })
  
  closeAll <- function() {
    shinyjs::hide(selector = "a[data-value=\"doseresponseTab\"]")
    shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
    shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
    dataReshaped$reshapeD <- NULL
    scores$scores <- NULL
    scoreofthepart$scores <- NULL
    updateSelectInput(session, "selectInhVia", selected = "")
    updateCheckboxInput(session, "Switch", value = 0)
    updateCheckboxInput(session, "Switch2", value = 0)
    updateCheckboxInput(session, "Switch4", value = 0) 
    closeAlert(session, "alert1")
    closeAlert(session, "alertPD")
    # shinyjs::runjs("$('#HowToUse').modal('hide');$('#Save_full_').modal('hide');")
  }

  # inputDataTab ------------------------------------------------------------
  
  # Input data type select box
  observeEvent(input$inputDatatype, {
    closeAll()
    inputdatatype$type_ <- input$inputDatatype
    })

  # when annotation file is loaded  
  observeEvent(input$annotfile, {
    tryCatch({
      # if already loaded annotation table, drop all switches and vars
      if (!is.null(datannot$annot)) {
        closeAll()
      }
      # file extension
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
        if (inputdatatype$type_ == "Table") {
          if (ext == 'XLSX') {
            annot <- openxlsx::read.xlsx(
              input$annotfile$datapath
            )
          } else if (ext %in% c("TXT", "CSV")) {
            annot <- read.table(
              file = input$annotfile$datapath,
              header = TRUE,
              sep = ",",
              row.names = NULL,
              fill = TRUE,
              na.strings = c("NA", "")
            )
          }
          
          # take care of NA's and empty rows/cols
          annot <- sapply(annot, function (x) gsub("^\\s+|\\s+$", "", x))
          annot <- annot[!apply(is.na(annot) | annot == "", 2, all), ] # rows with all NA
          annot <- annot[, !apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
           
          # check for missing columns
          # outList.names <- c("PairIndex", "Response", "Drug1", "Drug2", 
          #                     "Conc1", "Conc2", "ConcUnit")
          # mismatch = outList.names[!(colnames(annot) %in% outList.names)]
          #   
          # if(length(mismatch) == 0){
            # dataOutput <- data.frame(
            #   PairIndex = as.integer(annot$PairIndex),
            #   Response = round(as.numeric(annot$Response),2),
            #   Drug1 = as.character(annot$Drug1),
            #   Drug2 = as.character(annot$Drug2),
            #   Conc1 = round(as.numeric(annot$Conc1),2),
            #   Conc2 = round(as.numeric(annot$Conc2),2),
            #   ConcUnit = as.character(annot$ConcUnit), stringsAsFactors=F)
            datannot$annot <- dataOutput
            datannot$type_ <- "Table"
          # } else {
          #    mismatch <- na.omit(mismatch)
          #    createAlert(session, "noPDdata", "alertPD", title = "Error",
          #                content = paste0("Annotation table doesnot contain ",
          #                                 paste0(mismatch, collapse = ", "), 
          #                                 " column(s). \n See example data."),
          #                append = F)
          #    datannot$annot <- "WRONG"
          # }
        } else if (inputdatatype$type_ == "Matrix") {
          if (ext == 'XLSX') {
            annot <- openxlsx::read.xlsx(
              input$annotfile$datapath,
              colNames = FALSE
            )
          } else if (ext %in% c("TXT", "CSV")) {
            annot <- read.table(
              file = input$annotfile$datapath,
              header = FALSE,
              sep =",",
              row.names = NULL,
              fill = TRUE
            )
          }
           
          # take care of NA's and empty rows/cols       
          annot <- sapply(annot, function (x) gsub("^\\s+|\\s+$", "", x))
          annot <- annot[!apply(is.na(annot) | annot == "", 1, all),] # rows with all NA
          annot <- annot[,!apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
           
          D1 <- sum(grepl("Drug1:", annot[,1]))
          D2 <- sum(grepl("Drug2:", annot[,1]))
          ConcUn <- sum(grepl("ConcUnit:", annot[,1]))
          if (D1 != D2 || D1 != ConcUn){
            createAlert(
              session,
              anchorId = "noPDdata",
              alertId = "alertPD",
              title = "Error",
              content = paste0(
                "The input data contains <b>", D1, "</b> Drug1, <b>",
                D2, "</b> Drug2, and <b>", ConcUn,"</b> ConcUnit fields.\n",
                "The numbers are not equal. See example data."
              ),
              append = FALSE
            )
            datannot$annot <- "WRONG"
          } else {
            datannot$annot <- annot
            datannot$type_ <- "Matrix"
          }
        }
      }
    }, error = function(e) {
      toastr_error(
        message = paste0(
          "Something wrong with your file that cannot be handled by application.",
          " Please check that <b>\"", inputdatatype$type_,
          "\"</b> is a correct file format.",
          "For more information about input data see <b>section 3</b> in ",
          "<b>USER GUIDE</b>."
        ),
        title = "Unhandled error occurred!",
        closeButton = FALSE,
        progressBar = FALSE,
        position = "top-right",
        preventDuplicates = FAKSE,
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
  }) # observeEvent
  
  # when readout is changed 
  observeEvent(input$selectInhVia,{
    if (input$selectInhVia != "") {
      if (!is.null(datannot$annot) && datannot$annot != "WRONG") {
        closeAlert(session, "alertPD")
        tryCatch({
          if (inputdatatype$type_ == "Table") {
            dataReshaped$reshapeD <- transformInputData(
              data = datannot$annot,
              data_type = input$selectInhVia
            )
            # Show data table
            output$inputData = renderDT(
              datannot$annot,
              options = list(lengthChange = FALSE)
            )
          } else {
            dataReshaped$reshapeD <- transformInputDataMatrix(
              data = datannot$annot,
              data_type = input$selectInhVia)
            # Show data table
            output$inputData = renderDT(
              dataReshaped$reshapeD$data.table, options = list(lengthChange = FALSE)
            )
          }
        }, error = function(e) {
          closeAll()
          toastr_error(
            message = paste0(
              "Something wrong with your file that cannot be handled by",
              "application. Following is the original error message: \n",
              e,"\n",
              "Please check the input data formation and columns.",
              " For more information about input data see USER GUIDE"
              ),
            title = "Unhandled error occurred!",
            closeButton = !0,
            progressBar = !0,
            position = "top-right",
            preventDuplicates = !0,
            showDuration = 300,
            hideDuration = 1000,
            timeOut = 10000,
            extendedTimeOut = 1000,
            showEasing = "swing",
            hideEasing = "swing",
            showMethod = "fadeIn",
            hideMethod = "fadeOut"
          )
        })
        # warnings (returned from transformInputData)
        if (!is.null(dataReshaped$reshapeD$warning)) {
          toastr_warning(dataReshaped$reshapeD$warning[i], title = "Warning!",
                         closeButton = !0, progressBar = !0,
                         position = "top-right", preventDuplicates = !0,
                         showDuration = 300, hideDuration = 1000,
                         timeOut = 30000, extendedTimeOut = 1000,
                         showEasing = "swing", hideEasing = "swing",
                         showMethod = "fadeIn", hideMethod = "fadeOut")
        if (!is.null(dataReshaped$reshapeD)) {
          if (!is.null(input$tabsDR)) {
            vizDR()
            }
          if (input$Switch2 == 1){
            shinyjs::show(selector = "a[data-value=\"synergyTab\"]")
            data_ <- dataReshaped$reshapeD
            if (!is.null(data_)){
              withProgress({
                setProgress(message = 'Calculation in progress...', value=1)
                # scores$scores <- CalculateSynergy(data_, input$methods, correction = ifelse(input$Switch3 == 1, !0, !1))
                data_[['adjusted.response.mats']] <- lapply(data_[['dose.response.mats']],
                                                            function(x){CorrectBaseLine(response.mat = x,
                                                                                        method = input$correction)})
                names(data_[['adjusted.response.mats']]) <- as.character(data_[['drug.pairs']]$PairIndex)
                names(data_[['dose.response.mats']]) <- as.character(data_[['drug.pairs']]$PairIndex)
                scores$scores <- CalculateSynergy(data_, input$methods)
                if (input$Switch4 == 1){
                  vizSyn(scores$scores)
                } else {
                  shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
                  }
                    
              })
            }
          } else {
            shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
          }
        }
      } else {
        updateSelectInput(session, "selectInhVia", selected = "")
        createAlert(session, "noPDdata", "alertPD", title = "Error",
                    content = "Please load required files first! or upload an example data",
                    append = !1, dismiss = !1)
      }
    }
  })
  

  ####################################################
  #  CREATE AND OBSERVE DYNAMIC TABS FOR DOSE-RESPONSE -------------------------
  ####################################################
  observeEvent(input$Switch, {
    if (input$Switch){
      if(isolate(input$selectInhVia)!="" & !is.null(datannot$annot) & !is.null(dataReshaped$reshapeD)){
        closeAlert(session, "alertPD")
        shinyjs::show(selector = "a[data-value=\"doseresponseTab\"]")
        updateTabItems(session, "menu1", selected = "doseresponseTab")
      } else {
        closeAll();
        createAlert(session, "noPDdata", "alertPD", title = "Error",
                    content = "Please choose a readout and upload required files! or use an example data",
                    append = !1, dismiss = !1)
      }
    } else {
      shinyjs::hide(selector = "a[data-value=\"doseresponseTab\"]")
    }

  })
  
  #create dynamic tabs for Dose response
  output$tabs <- renderUI({
    
    drug.pairs <- dataReshaped$reshapeD$drug.pairs
    
    if (!is.null(drug.pairs)) { 
      print("dyn tabs inside")
      
      tabs <- list(NULL)
      if (!is.null(isolate(input$tabsDR))) {
        curTab = as.integer(isolate(input$tabsDR))
      } else {
          curTab = NULL
      }

      #find all drug pairs
      tabnames <- sapply(X = 1:nrow(drug.pairs), 
                         FUN = function(i){paste0(drug.pairs$drug.col[i], " - ",
                                                  drug.pairs$drug.row[i])})
      J <- length(tabnames)
      pNames <- paste0("plot", LETTERS[1:J])
      
      tabs <- lapply(X = 1:J, function(i){
        tabPanel(tabnames[i],
                 h3(""), 
                 # box(
                 #   width = input$width, status = "info", solidHeader = FALSE,
                 #   collapsible = FALSE, height = input$height+100,
                 #   title = "Dose-response data",
                 fluidRow( width = input$width,
                   column(6,
                          tabsetPanel(
                            tabPanel(head(strsplit(tabnames[i],split=" ")[[1]],1),
                                     plotOutput(pNames[i], height = input$height)),
                            tabPanel(tail(strsplit(tabnames[i],split=" ")[[1]],1),
                                     plotOutput(paste0(pNames[i],"202"),
                                                height = input$height)))
                   ),
                   column(6,
                          plotOutput(paste0(pNames[i], "102"), height = input$height)
                   )
                 ),
                 value=i)
      })
      tabs$id <- "tabsDR"
      do.call(tabsetPanel, c(tabs, selected = curTab)) 
    }
  })
  
  #when dynamic tab is changed/chosen, get data and fill the tab  
  observeEvent(input$tabsDR ,{ 
    if (!is.null(dataReshaped$reshapeD)) 
      vizDR();
  })
  

  ####################################################
  #  VISUALIZE DYNAMIC TABS FOR DOSE-RESPONSE ----------------------------------
  ####################################################
  
  vizDR <- compiler::cmpfun(function(){
    I <- isolate(as.integer(input$tabsDR))
    dat <- isolate(dataReshaped$reshapeD)
    pNames <- paste0("plot", LETTERS[1:nrow(dat$drug.pairs)])
    name1 <- paste0(pNames[I],"102"); name2 <- paste0(pNames[I],"202")
    
    shinyjs::runjs('$("#wraptour").hide();');
    lapply(1:length(pNames), function(i){
      shinyjs::runjs(paste0("$('#", pNames[i], "').empty(); $('#", name1,
                            "').empty(); $('#", name2, "').empty();"))
    })
    
    tryCatch({
      
        dose.response.mats <- dat$dose.response.mats
        drug.pairs <- dat$drug.pairs
        response.mat <- dose.response.mats[[I]]
        unit.text <- paste0("(", drug.pairs$concUnit[I], ")")
        drug.row <- drug.pairs$drug.row[I]
        drug.col <- drug.pairs$drug.col[I]
        # single.fitted <- FittingSingleDrug(response.mat)
        drug.col.response <- ExtractSingleDrug(response.mat, dim = "col")
        drug.col.model <- FitDoseResponse(drug.col.response)
        drug.row.response <- ExtractSingleDrug(response.mat, dim = "row")
        drug.row.model <- FitDoseResponse(drug.row.response)
        
        # remove cols from dose-response
        output$increase1 <- renderUI(selectizeInput("colinput", drug.col, 
                                                    choices = colnames(response.mat)[-1], 
                                                    multiple = T, selected = NULL,
                                                    options = list(maxItems = 1)))
        output$increase2 <- renderUI(selectizeInput("rowinput", drug.row, 
                                                    choices = rownames(response.mat)[-1], 
                                                    multiple = T, selected = NULL, 
                                                    options = list(maxItems = 1)))
        
        output[[name1]] <- renderPlot({ # plot in each tab
          PlotDoseResponseShinyDR("inhibition", paramplot = 1, response.mat, 
                                  unit.text, drug.row, drug.col)
        })
        
        output[[pNames[I]]] <- renderPlot({ # plot in each tab
          PlotDoseResponseShinyDR("inhibition", paramplot = 3, response.mat, 
                                  unit.text, drug.row, drug.col,# single.fitted)
                                  drug.row.model = drug.row.model,
                                  drug.col.model = drug.col.model)
        })
        
        output[[name2]] <- renderPlot({ # plot in each tab
          PlotDoseResponseShinyDR("inhibition", paramplot = 2, response.mat, 
                                  unit.text, drug.row, drug.col,# single.fitted)
                                  drug.row.model = drug.row.model,
                                  drug.col.model = drug.col.model)
        })
    }, error = function(e) {
    
      toastr_error("SynergyFinder cannot display this combination! If you cannot find the error, please contact the app author.", 
                   title = "Unhandled error occurred!", closeButton = !0, 
                   progressBar = !0, position = "top-right",
                   preventDuplicates = !0, showDuration = 300, 
                   hideDuration = 1000, timeOut = 10000, extendedTimeOut = 1000,
                   showEasing = "swing",  hideEasing = "swing",
                   showMethod = "fadeIn", hideMethod = "fadeOut")
    })
  })

  
  na.mean <- compiler::cmpfun(function(mat){
    nr_ <- nrow(mat)
    nc_ <- ncol(mat)
    for (i in 1:nr_){
      for (j in 1:nc_){
        if (is.na(mat[i,j])){
          a = b =c = d = 0
          if ((i+1)<=nr_){
            a = mat[i+1,j]
          }
          if ((j+1)<=nc_){
            b = mat[i,j+1]
          }
          if ((i-1)>=1){
            c = mat[i-1,j]
          }
          if ((j-1)>=1){
            d = mat[i,j-1]
          }
          
          sum_ = sum(c(a,b,c,d) != 0)
          
          if (sum_==0){
            mat[i,j] == 0
          } else {
            mat[i,j] = sum(c(a,b,c,d)) / sum_
          }
        }
      }
    }
    mat
  })
  
   observeEvent(input$excludeconc,{
     rowinp <- isolate(input$rowinput)
     colinp <- isolate(input$colinput)
     
     if(!is.null(colinp) | !is.null(rowinp)){
       I = isolate(as.integer(input$tabsDR))
       drmatr <- isolate(dataReshaped$reshapeD$dose.response.mats[[I]])
       drmatr[(rownames(drmatr) == rowinp),(colnames(drmatr) == colinp)] <- NA
       
       dimna <- dimnames(drmatr)
       drmatr <- na.mean(drmatr)
       dimnames(drmatr) <- dimna
       updateCheckboxInput(session, "Switch2", value = 0)
       updateCheckboxInput(session, "Switch4", value = 0) 
       shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
       shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
       dataReshaped$reshapeD$dose.response.mats[[I]] <- drmatr
       vizDR()
     }
   })
  
  #######################################################
  #  CREATE AND OBSERVE DYNAMIC TABS FOR SYNERGY PLOTS -------------------------
  #######################################################
  
  #create dynamic tabs for synergy
  output$tabs2 <- renderUI({  
    print("create tabs")
    
    tabs <- list(NULL)
    
    if(!is.null(isolate(input$tabsSyn))){
      curTab <- as.integer(isolate(input$tabsSyn))
    } else {
      curTab <- NULL
    }
    
    # not isolate! calls drawing of tab content
    drug.pairs <- dataReshaped$reshapeD$drug.pairs
    
    if (!is.null(drug.pairs)) {   
      tabnames <- sapply(X = 1:nrow(drug.pairs), 
                        FUN = function(i){paste0(drug.pairs$drug.col[i], 
                                                " & ", drug.pairs$drug.row[i])})
      tabs <- lapply(X = 1:length(tabnames), 
                     function(i){tabPanel(tabnames[i], value=i)})
      
      tabs$id <- "tabsSyn"; print("create tabs done")
      do.call(tabsetPanel, c(tabs, selected = curTab)) 
    }
  })
  
  #create dynamic tabs for synergy
  observeEvent(input$Switch2,{
    if (input$Switch2 == 1){
      shinyjs::show(selector = "a[data-value=\"synergyTab\"]")
      updateTabItems(session, "menu1", selected = "synergyTab")
    } else {
      shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
    }

  })
  
  redrawSynPlotsMethodOrCorr <- compiler::cmpfun(function(){  
    if (input$Switch2 == 1) {
      shinyjs::show(selector = "a[data-value=\"synergyTab\"]")
      updateTabItems(session, "menu1", selected = "synergyTab")
      data_ <- dataReshaped$reshapeD
      if (!is.null(data_)) { 
        withProgress({setProgress(message = 'Calculation in progress...', value=1) 
          # scores$scores <- CalculateSynergy(data_, input$methods, correction = ifelse(input$Switch3 == 1, !0, !1))
          data_[['adjusted.response.mats']] <- lapply(data_[['dose.response.mats']], 
                                    function(x) {CorrectBaseLine(response.mat = x,
                                                   method = input$correction)})
          names(data_[['adjusted.response.mats']]) <- as.character(data_[['drug.pairs']]$PairIndex)
          names(data_[['dose.response.mats']]) <- as.character(data_[['drug.pairs']]$PairIndex)
          scores$scores <- CalculateSynergy(data_, input$methods)
          if(input$Switch4 == 1) {
            vizSyn(scores$scores)
          } else {
            shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
          }
        })
      } else {
        shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
      }
    }
  })

  observeEvent(input$correction, redrawSynPlotsMethodOrCorr())
  observeEvent(input$methods, redrawSynPlotsMethodOrCorr())
  observeEvent(input$Switch4, {
    if (input$Switch2 == 1)
    {
      if(isolate(input$selectInhVia)!="" & !is.null(datannot$annot))
      {
        closeAlert(session, "alertPD")
        data_ <- dataReshaped$reshapeD
        
        if (!is.null(data_)) {
          shinyjs::runjs('$("#wraptour").hide();')
          withProgress({ setProgress(message = 'Calculation in progress...', value=1) 
            # scores$scores <- CalculateSynergy(data_, input$methods, correction = ifelse(input$Switch3 == 1, !0, !1))
            data_[['adjusted.response.mats']] <- lapply(data_[['dose.response.mats']], 
                                                       function(x) {CorrectBaseLine(response.mat = x,
                                                                                    method = input$correction)})
            names(data_[['adjusted.response.mats']]) <- as.character(data_[['drug.pairs']]$PairIndex)
            names(data_[['dose.response.mats']]) <- as.character(data_[['drug.pairs']]$PairIndex)
            scores$scores <- CalculateSynergy(data_, input$methods)
          })
        }
        vizSyn(scores$scores)
        shinyjs::show(selector = "a[data-value=\"reportTab\"]")
        updateTabItems(session, "menu1", selected = "synergyTab")
      }
      else
      {
        updateCheckboxInput(session, "Switch", value = 0)
        dataReshaped$reshapeD = NULL; scores$scores = NULL
        updateSelectInput(session, "selectInhVia", selected = "")
        updateCheckboxInput(session, "Switch2", value = 0)
        updateCheckboxInput(session, "Switch4", value = 0)
        shinyjs::hide(selector = "a[data-value=\"doseresponseTab\"]")
        shinyjs::hide(selector = "a[data-value=\"synergyTab\"]")
        shinyjs::hide(selector = "a[data-value=\"reportTab\"]")
        createAlert(session, "noPDdata", "alertPD", title = "Error", 
                    content = "Please choose a readout and upload required files! or use an example data", 
                    append = !1, dismiss = !1)
      }
    }
  })
  
  
  ##########################################################################################################################################
  #  VISUALIZE DYNAMIC TABS FOR SYNERGY PLOTS
  ####################################################
  
  vizSyn <- compiler::cmpfun(function(scores_)
  {
    shinyjs::runjs("$('#plotsyn1').empty();$('#plotsyn2').css({opacity:100});$('#wraptour').hide();");
    #shinyjs::runjs("$('#theImg').remove();$('.heysyn .box-body').prepend('<img id=\"theImg\" style=\"display: block; margin: 0 auto; \" src=\"load.gif\" />')");
    
    if(!is.null(isolate(input$tabsSyn)) & !is.null(isolate(scores_$scores))){
     
      tryCatch({
     
        withProgress({    
          setProgress(message = 'Visualising data...', value=1)  
          I = as.integer(isolate(input$tabsSyn))
          
          data_ = calcsyn(scores_$scores[[I]],  scores_$drug.pairs[I,])
          output$plotsyn2 <- renderPlotly({ # plot in each tab
            PlotSynergyShiny(data_ , "3D", gridsize = input[["sizegridsyn2"]],
                             method_ = isolate(input$methods))
          })
          output$plotsyn1 <- renderPlot({ # plot in each tab
            PlotSynergyShiny(data_ , "2D", 2, ranges$x, ranges$y, 
                             gridsize2 = input[["sizegridsyn"]], 
                             newscore = isolate(scoreofthepart$scores), 
                             method_ = isolate(input$methods), 
                             synScoresMtx = scores_$scores[[1]], 
                             mostsynarea = input[["synarea"]])
          })
         # shinyjs::runjs("$('#plotsyn2').show();");
        })
        
      }, error = function(e) {
        print(e)
        toastr_error("Synergy was not calculated for this combination! If you cannot find the error, please contact the app author.",
                     title = "Unhandled error occurred!", closeButton = !0, 
                     progressBar = !0, position = "top-right", 
                     preventDuplicates = !0, showDuration = 300, 
                     hideDuration = 1000, timeOut = 10000, 
                     extendedTimeOut = 1000, showEasing = "swing",
                     hideEasing = "swing", showMethod = "fadeIn", 
                     hideMethod = "fadeOut")
        
        
      })
        
    }
  })
  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot7_dblclick, { print("o")
                    
     brush <- input$plotincrease_brush7
     
     if (!is.null(brush)) {
       drmat = isolate(dataReshaped$reshapeD$dose.response.mats[[as.integer(isolate(input$tabsSyn))]])
       
       xpos = sapply(0:(ncol(drmat) - 1), function(i) i*5 + 1)
       ypos = sapply(0:(nrow(drmat) - 1), function(i) i*5 + 1)
       
       mixx = which.min(abs(xpos - brush$xmin))
       maxx = which.min(abs(xpos - brush$xmax))
       bminx <- xpos[mixx]; bmaxx <- xpos[maxx] 
       
       miyy = which.min(abs(ypos - brush$ymin))
       mayy = which.min(abs(ypos - brush$ymax))
       bminy <- ypos[miyy]; bmaxy <- ypos[mayy]
       
       if(bminy == bmaxy || bminx == bmaxx)
       {
         maxx = length(xpos); mayy = length(ypos)
         bminy = bminx = mixx = miyy = 1; bmaxx = bmaxy = 36; 
       }
       
       ranges$x <- c(bminx, bmaxx)
       ranges$y <- c(bminy, bmaxy)
       
       I <- as.integer(isolate(input$tabsSyn))
       
       data_small_ <- isolate(dataReshaped$reshapeD)
       dose.response.mats.cur = data_small_$dose.response.mats[[I]]
       drug.pairs.cur = as.data.frame(c(data_small_$drug.pairs[I,]))
       drug.pairs.cur$blockIDs = 1
       data_small_ = list(dose.response.mats = list(dose.response.mats.cur), 
                          drug.pairs = drug.pairs.cur)
       
       method <- isolate(input$methods)
       if(is.null(method)) method = "ZIP"
       
       # if (isolate(input$Switch3) == 1)
       #   scores_small_ <- CalculateSynergy(data_small_, method, correction = !0)
       # 
       # else
       #   scores_small_ <- CalculateSynergy(data_small_, method, correction = !1) 
       data_small_[['adjusted.response.mats']] <- apply(data_small_[['dose.response.mats']], 
                                                  function(x) {CorrectBaseLine(response.mat = x,
                                                                               method = input$correction)})
       names(data_small_[['adjusted.response.mats']]) <- as.character(data_small_[['drug.pairs']]$PairIndex)
       names(data_small_[['dose.response.mats']]) <- as.character(data_small_[['drug.pairs']]$PairIndex)
       scores$scores <- CalculateSynergy(data_small_, method = method)
       
       if(bmaxx != 6 && bmaxy !=6){
         score.dose = scores_small_$scores[[1]][miyy:mayy, mixx:maxx]
         score.dose = score.dose[rownames(score.dose) != "0", ]
         score.dose = score.dose[, colnames(score.dose) != "0"]
         
         scores.dose <- t(score.dose)
         
         if(bmaxx == 36 && bmaxy == 36)
           scores.dose[nrow(scores.dose),ncol(scores.dose)] <- NA
           
         summary.score <- round(mean(scores.dose, na.rm=!0), 3)
 
         if(bminy == bmaxy || bminx == bmaxx)
           scoreofthepart$scores <- NULL
         else
           scoreofthepart$scores <- summary.score
       }
       else{
         ranges$x <- ranges$y <- scoreofthepart$scores <- NULL
       }
     } else {
       ranges$x <- ranges$y <- scoreofthepart$scores <- NULL
     }
  })
  
  #when dynamic tab is changed/chosen, get data and fill the tab   
  observeEvent(input$tabsSyn,{ 
    ranges$x =ranges$y = scoreofthepart$scores <- NULL
    scores_ <- isolate(scores$scores)
    if (!is.null(scores_)) vizSyn(scores_)
  })

  ##########################################################################################################################################
  #  DOWNLOADS
  #################################################### 

  # load example data through menu
  output$loadExData_small <- downloadHandler(
    filename = function(){ paste0("ExampleData.zip") },
    content = function(file){
      #file.copy("data_/ExampleData.zip", file)},
      zip(file, "ExampleData/")},
    contentType = "application/zip"
  )

# create select inputs to choose drug combinations pairs for printing
output$selectinputconprints <- renderUI({
  x = dataReshaped$reshapeD$drug.pairs
  drug.pairs = sapply(1:nrow(x), function (i) paste0(x$drug.row[i], " - ", x$drug.col[i]))
  selectizeInput("pairsprintstatic","Choose drug pairs", choices = drug.pairs, 
                 multiple = T, selected = drug.pairs[1:length(drug.pairs)])
})
# create select inputs to choose drug combinations pairs for printing
output$selectinputconprintd <- renderUI({
  x = dataReshaped$reshapeD$drug.pairs
  drug.pairs = sapply(1:nrow(x), function (i) paste0(x$drug.row[i], " - ", x$drug.col[i]))
  selectizeInput("pairsprintdynamic","Choose drug pairs", choices = drug.pairs, 
                 multiple = T, selected = drug.pairs[1:length(drug.pairs)])
})
# create select inputs to choose drug combinations pairs for printing
output$selectinputconprintcomb <- renderUI({
  x = dataReshaped$reshapeD$drug.pairs
  drug.pairs = sapply(1:nrow(x), function (i) paste0(x$drug.row[i], " - ", x$drug.col[i]))
  selectizeInput("pairsprintcomb","Choose drug pairs", choices = drug.pairs, 
                 multiple = T, selected = drug.pairs[1:length(drug.pairs)])
})

content2dSyn <- compiler::cmpfun(function(file){
  I = as.integer(isolate(input$tabsSyn))
  data_ = calcsyn(scores$scores$scores[I][[1]], scores$scores$drug.pairs[I,])
  plotname_ <- paste0(gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time())))),
                      "_plot2D.pdf")
  PlotSynergyShiny(data_ , "2D", 2, ranges$x, ranges$y,
                   gridsize2 = input[["sizegridsyn"]], savee2D = 1,
                   name_3D = plotname_, newscore = isolate(scoreofthepart$scores),
                   method_ = isolate(input$methods),
                   synScoresMtx = scores_$scores[[1]],
                   mostsynarea = input[["synarea"]])
  file.copy(plotname_, file)})

output$download2Dsyn <- downloadHandler(
    filename = function() paste0("plot2D_",Sys.Date(),".pdf"),
    content = content2dSyn,
    contentType = NULL
  )

content3dSyn <- compiler::cmpfun(function(file){
  I = as.integer(isolate(input$tabsSyn))
  data_ = calcsyn(scores$scores$scores[I][[1]], scores$scores$drug.pairs[I,])
  plotname_ <- paste0(gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time())))),
                      "_plot3D.html")
  PlotSynergyShiny(data_ , "3D", gridsize = input[["sizegridsyn2"]],
                   savee3D = 1, name_3D = plotname_, method_ = isolate(input$methods))
  file.copy(plotname_, file)})
  
output$download3Dsyn <- downloadHandler(
  filename = function() paste0("plot3D_",Sys.Date(),".html"),
  content = content3dSyn
)
  

  ##########################################################################################################################################
  #  SAVE REPORT
  #######################################################

dynReportContent <- compiler::cmpfun(function(file){
  
  withProgress({
    setProgress(message = 'Report is being generated',
                detail = 'This may take a while...', value=1)  
    
    curdir <- getwd()
    dir_ <- gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time()))))
    dir.create(file.path(reportspath, dir_), recursive = T)
    
    for (i in list.files(path = ".")){
      if (file.exists(i))
        if (grepl(".sty", i))
          file.copy(i, file.path(reportspath, dir_))
    }
    
    setwd(file.path(reportspath, dir_))
    
    data_ <- dataReshaped$reshapeD
    # choose pairs for printing
    all.drug.pairs = sapply(1:nrow(data_$drug.pairs), 
                            function (i) paste0(data_$drug.pairs$drug.row[i],
                                                " - ", data_$drug.pairs$drug.col[i]))
    drug.pairs.used.ind = which(all.drug.pairs %in% isolate(input$pairsprintdynamic))
    
    method <- input$methods
    scores_ <- scores$scores$scores[drug.pairs.used.ind]
    data_ <-  scores$scores$dose.response.mats[drug.pairs.used.ind]
    pairs_ <- scores$scores$drug.pairs[drug.pairs.used.ind,]
    # params <- paste0("Readout: ", input$selectInhVia, " ; Baseline correction: ", ifelse(isolate(input$Switch3)==1, "Yes ;", "No ;"))
    params <- paste0("Readout: ", input$selectInhVia, " ; Baseline correction: ", 
                     input$correction)
    calcSyn_ <- lapply(1:length(scores_), function(i) calcsyn(scores_[[i]], pairs_[i,]))
    
    # report table
    if (isolate(input$synareaRepDyn)==0) {
      outList.names <- c("Drug combination", "Synergy score", "Method") 
    } else {
      outList.names <- c("Drug combination", "Synergy score", 
                         "Most synergistic area score", "Method")
    }
    
    outList <- sapply(outList.names,function(x) NULL)
    
    for (i in 1:length(scores_)){
      outList$`Drug combination`[i] <- paste0(pairs_$drug.col[i], " - ", 
                                              pairs_$drug.row[i])
      outList$`Synergy score`[i] <- calcSyn_[[i]]$summary.score
      outList$Method[i] <- method
      if(isolate(input$synareaRepDyn) != 0){
        outList$`Most synergistic area score`[i] <- calcSyn_[[i]]$max_
      }
    }
    outFrame <- as.data.frame.list(outList)
    names(outFrame) <- outList.names
    large <- function(x) paste0('{\\large{\\bfseries ', x, '}}')
    small <- function(x) paste0('{\\small ', x, '}')
    
    tt <- print(xtable(outFrame, align = ifelse(isolate(input$selectDynamic)==0,
                                                "cccc","ccccc")), type='latex', 
                include.rownames = !1,size = "\\large", 
                sanitize.colnames.function = large, 
                sanitize.text.function = small)
    texfile <- 'first0.tex'
    cat("\\documentclass[12pt]{report} \\usepackage[landscape]{geometry} \\usepackage{color} \\date{} \\begin{document}\\pagenumbering{gobble} \\clearpage \\thispagestyle{empty} \\bigbreak\\bigbreak  \\Large{\\bf\\centerline{\\color{blue} Calculation and Visualization of synergy scores for Drug Combinations}}\\bigbreak\\bigbreak\\bigbreak\\bigbreak\\large{\\bf{Drug combinations", 
        "" ,":}} \\bigbreak \\bigbreak", tt, 
        "\\bigbreak\\bigbreak\\bigbreak\\large{\\bf{Chosen parameters:}} \\bigbreak{", 
        params, "}\\bigbreak\\bigbreak\\bigbreak \\end{document}", 
        sep='', file=texfile)
    
    pdf("first1.pdf", width = 10, height = 8, onefile = T) 
    PlotDoseResponseReport(data_, pairs_, isolate(input$selectDynamic2))
    dev.off();
    
    system(paste0('pdflatex ', '-output-directory ./ ', texfile))  
    PlotSynergyReportdynamic(data_, scores_, calcSyn_, method, 
                             mostsynarea = isolate(input$synareaRepDyn), 
                             isolate(input$selectDynamic))
    file.copy("result.pdf", file)
    setwd(curdir)
  })
})  

  ######## DYNAMIC REPORT
output$downloadData <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_", Sys.Date(),".pdf"),
  content <- dynReportContent,
  contentType = NULL
)


staReportContent <- compiler::cmpfun(function(file){
  
  withProgress({
    setProgress(message = 'Report is being generated', 
                detail = 'This may take a while...', value=1)  
    
    curdir <- getwd()
    dir_ <- gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time()))))
    dir.create(file.path(reportspath, dir_), recursive = T)
    
    for (i in list.files(path = ".")){
      if (file.exists(i))
        if (grepl(".sty", i))
          file.copy(i, file.path(reportspath, dir_))
    }
    setwd(file.path(reportspath, dir_))
    
    data_ <- dataReshaped$reshapeD
    # choose pairs for printing
    all.drug.pairs = sapply(1:nrow(data_$drug.pairs), 
                            function (i) paste0(data_$drug.pairs$drug.row[i], 
                                                " - ", 
                                                data_$drug.pairs$drug.col[i]))
    drug.pairs.used.ind = which(all.drug.pairs %in% isolate(input$pairsprintstatic))
    
    method <- input$methods
    scores_ <- scores$scores$scores[drug.pairs.used.ind]
    data_ <-  scores$scores$dose.response.mats[drug.pairs.used.ind]
    pairs_ <- scores$scores$drug.pairs[drug.pairs.used.ind,]
    # params <- paste0("Readout: ", input$selectInhVia, " ; Baseline correction: ", ifelse(isolate(input$Switch3)==1, "Yes ;", "No ;"))
    params <- paste0("Readout: ", input$selectInhVia, " ; Baseline correction: ",
                     input$correction)
    calcSyn_ <- lapply(1:length(scores_), function(i) calcsyn(scores_[[i]], 
                                                              pairs_[i,]))
    
    # report table
    if (isolate(input$synareaRepStat)==0){
      outList.names <- c("Drug combination", "Synergy score", "Method") 
    } else {
      outList.names <- c("Drug combination", "Synergy score", 
                         "Most synergistic area score", "Method")
    }
    outList <- sapply(outList.names,function(x) NULL)
    
    for (i in 1:length(scores_)){
      outList$`Drug combination`[i] = paste0(pairs_$drug.col[i], " - ", 
                                             pairs_$drug.row[i])
      outList$`Synergy score`[i] = calcSyn_[[i]]$summary.score
      outList$Method[i] = method
      if(isolate(input$synareaRepStat) != 0){
        outList$`Most synergistic area score`[i] <- calcSyn_[[i]]$max_
      }
    }
    outFrame <- as.data.frame.list(outList)
    names(outFrame) <- outList.names
    large <- function(x) paste0('{\\large{\\bfseries ', x, '}}')
    small <- function(x) paste0('{\\small ', x, '}')
    
    tt <- print(xtable(outFrame, align = ifelse(isolate(input$synareaRepStat)==0,
                                                "cccc","ccccc")), type='latex', 
                include.rownames = !1,size = "\\large", 
                sanitize.colnames.function = large, 
                sanitize.text.function = small)
    texfile <- 'first0.tex'
    cat("\\documentclass[12pt]{report} \\usepackage[landscape]{geometry} \\usepackage{color} \\date{} \\begin{document}\\pagenumbering{gobble} \\clearpage \\thispagestyle{empty} \\bigbreak\\bigbreak  \\Large{\\bf\\centerline{\\color{blue} Calculation and Visualization of synergy scores for Drug Combinations}}\\bigbreak\\bigbreak\\bigbreak\\bigbreak\\large{\\bf{Drug combinations", 
        "" ,":}} \\bigbreak \\bigbreak", tt, 
        "\\bigbreak\\bigbreak\\bigbreak\\large{\\bf{Chosen parameters:}} \\bigbreak{", 
        params, "}\\bigbreak\\bigbreak\\bigbreak \\end{document}", 
        sep = '', file = texfile)
    
    pdf("first1.pdf", width = 10, height = 8, onefile = T) 
    PlotDoseResponseReport(data_, pairs_, isolate(input$selectStatic2))
    dev.off();
    
    system(paste0('pdflatex ', '-output-directory ./ ', texfile))  
    PlotSynergyReport(data_,scores_, calcSyn_, method, 
                      mostsynarea = isolate(input$synareaRepStat), 
                      isolate(input$selectStatic))
    file.copy("result.pdf", file)
    setwd(curdir)
  })
})  
  
  ######## STATIC REPORT
  output$downloadData2 <- downloadHandler(
    filename <- function() paste0("result_",input$methods, "_", Sys.Date(), ".pdf"),
    content <- staReportContent,
    contentType = NULL
  )

combReportContent <- compiler::cmpfun(function(file) {
  
  withProgress({
    setProgress(message = 'Report is being generated', 
                detail = 'This may take a while...', value=1)  
    
    curdir <- getwd()
    dir_ <- gsub(":","",gsub("-", "", gsub("\\s", "", paste0(Sys.time()))))
    dir.create(file.path(reportspath, dir_), recursive = T)
    
    for (i in list.files(path = ".")){
      if (file.exists(i))
        if (grepl(".sty", i))
          file.copy(i, file.path(reportspath, dir_))
    }
    setwd(file.path(reportspath, dir_))
    data_ <- dataReshaped$reshapeD
    # choose pairs for printing
    all.drug.pairs <- sapply(1:nrow(data_$drug.pairs), 
                            function (i) paste0(data_$drug.pairs$drug.row[i], 
                                                " - ", 
                                                data_$drug.pairs$drug.col[i]))
    drug.pairs.used.ind <- which(all.drug.pairs %in% isolate(input$pairsprintcomb))
    
    method <- input$methods
    scores_ <- scores$scores$scores[drug.pairs.used.ind]
    data_ <-  scores$scores$dose.response.mats[drug.pairs.used.ind]
    pairs_ <- scores$scores$drug.pairs[drug.pairs.used.ind,]
    # params <- paste0("Readout: ", input$selectInhVia, " ; Baseline correction: ", ifelse(isolate(input$Switch3)==1, "Yes ;", "No ;"))
    params <- paste0("Readout: ", input$selectInhVia, " ; Baseline correction: ", 
                     input$correction)
    calcSyn_ <- lapply(1:length(scores_), function(i) calcsyn(scores_[[i]],
                                                              pairs_[i,]))
    
    # report table
    if (isolate(input$synareaRep)==0){
      outList.names <- c("Drug combination", "Synergy score", "Method")
    } else {
      outList.names <- c("Drug combination", "Synergy score", 
                         "Most synergistic area score", "Method")
    }
    outList <- sapply(outList.names,function(x) NULL)
    
    for (i in 1:length(scores_)){
      outList$`Drug combination`[i] = paste0(pairs_$drug.col[i], " - ",
                                             pairs_$drug.row[i])
      outList$`Synergy score`[i] = calcSyn_[[i]]$summary.score
      outList$Method[i] = method
      if(isolate(input$synareaRep) != 0){
        outList$`Most synergistic area score`[i] <- calcSyn_[[i]]$max_
      }
    }
    outFrame <- as.data.frame.list(outList)
    names(outFrame) <- outList.names
    large <- function(x) paste0('{\\large{\\bfseries ', x, '}}')
    small <- function(x) paste0('{\\small ', x, '}')
    
    tt <- print(xtable(outFrame, align = ifelse(isolate(input$synareaRep)==0,
                                                "cccc","ccccc")), type='latex', 
                include.rownames = !1,size = "\\large", 
                sanitize.colnames.function = large, 
                sanitize.text.function = small)
    texfile <- 'first0.tex'
    cat("\\documentclass[12pt]{report} \\usepackage[landscape]{geometry} \\usepackage{color} \\date{} \\begin{document}\\pagenumbering{gobble} \\clearpage \\thispagestyle{empty} \\bigbreak\\bigbreak  \\Large{\\bf\\centerline{\\color{blue} Calculation and Visualization of synergy scores for Drug Combinations}}\\bigbreak\\bigbreak\\bigbreak\\bigbreak\\large{\\bf{Drug combinations", 
        "" ,":}} \\bigbreak \\bigbreak", tt, 
        "\\bigbreak\\bigbreak\\bigbreak\\large{\\bf{Chosen parameters:}} \\bigbreak{", 
        params, "}\\bigbreak\\bigbreak\\bigbreak \\end{document}", 
        sep = '', file = texfile)
    
    system(paste0('pdflatex ', '-output-directory ./ ', texfile))  
    PlotSynergyReportcomb(data_,scores_, calcSyn_, method, 
                          mostsynarea = isolate(input$synareaRep)) # drmatrix, synergyscores, summary, method
    file.copy("result.pdf", file)
    setwd(curdir)
  })
})
  
######## COMB REPORT
output$downloadData3 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_", Sys.Date(),".pdf"),
  content <- combReportContent,
  contentType = NULL
)

observeEvent(input$videobt,toggleModal(session, "HowToUse", "open"))

SaveReportFunc <- compiler::cmpfun(function(){
  if (isolate(input$selectInhVia)!="" & !is.null(datannot$annot) & !is.null(scores$scores)){
    closeAlert(session, "alertPD"); toggleModal(session, "Save_full_", "open")
  } else {
    if(is.null(scores$scores)){
      toastr_warning("Please calculate synergy first!", title = "Warning!", 
                     closeButton = !0, progressBar = !0, position = "top-right",
                     preventDuplicates = !0, showDuration = 300, 
                     hideDuration = 1000, timeOut = 5000, extendedTimeOut = 1000, 
                     showEasing = "swing",hideEasing = "swing", 
                     showMethod = "fadeIn", hideMethod = "fadeOut")
    } else {
      createAlert(session, "noPDdata", "alertPD", title = "Error", 
                  content = "Please choose a readout and upload required files! or use an example data",
                  append = !1, dismiss = !1)
    }
  }
})


# Download summarized synergy score table ----------------------------------

downloadSynScores <- function() {
  data_ <- dataReshaped$reshapeD
  # choose pairs for printing
  method <- input$methods
  scores_ <- scores$scores$scores
  data_ <- scores$scores$dose.response.mats
  pairs_ <- scores$scores$drug.pairs
  calcSyn_ <- lapply(1:length(scores_), function(i) calcsyn(scores_[[i]],
                                                            pairs_[i,]))
  
  # report table
  outList.names <- c("Drug combination", "Synergy score",
                     "Most synergistic area score", "Method")
  outList <- sapply(outList.names,function(x) NULL)
  
  for (i in 1:length(scores_)){
    outList$`Drug combination`[i] = paste0(pairs_$drug.col[i], " - ",
                                           pairs_$drug.row[i])
    outList$`Synergy score`[i] = calcSyn_[[i]]$summary.score
    outList$Method[i] = method
    outList$`Most synergistic area score`[i] = calcSyn_[[i]]$max_
  }
  as.data.frame.list(outList)
}

output$downloadSynscores1 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_", Sys.Date(),".xlsx"),
  content <- function(file){
    outFrame <- downloadSynScores()
    openxlsx::write.xlsx(outFrame, "./www/synergy_scores.xlsx", asTable = T)
    file.copy("./www/synergy_scores.xlsx", file)
  },
  contentType = NULL
)

output$downloadSynscores2 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_", Sys.Date(),".csv"),
  content <- function(file){
    outFrame <- downloadSynScores()
    write.csv(outFrame, "./www/synergy_scores.csv")
    file.copy("./www/synergy_scores.csv", file)
  },
  contentType = NULL
)

output$downloadSynscores3 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_", Sys.Date(),".txt"),
  content <- function(file){
    outFrame <- downloadSynScores()
    write.table(outFrame, "./www/synergy_scores.txt")
    file.copy("./www/synergy_scores.txt", file)
  },
  contentType = NULL
)

# Download full synergy score table ----------------------------------

downloadSynScoresFull <- function(){
  # data_ <- dataReshaped$reshapeD
  # choose pairs for printing
  method <- input$methods
  scores_ <- scores$scores$scores
  data_ <- scores$scores$dose.response.mats
  pairs_ <- scores$scores$drug.pairs
  
  # report table
  output_table <- NULL
  
  for (i in 1:length(scores_)){
    tmp <- reshape2::melt(data_[[i]])
    names(tmp) <- c("Conc1", "Conc2", "PercentageInhibition")
    tmp_score <- reshape2::melt(scores_[[i]])
    names(tmp_score) <- c("Conc1", "Conc2", paste0("Synergy", method))
    tmp <- tmp %>% 
      dplyr::left_join(tmp_score, by = c("Conc1", "Conc2")) %>% 
      dplyr::mutate(PairIndex = rep(i, dplyr::n()),
                    Drug1 = rep(pairs_$drug.row[i], dplyr::n()),
                    Drug2 = rep(pairs_$drug.col[i], dplyr::n()),
                    concUnit = rep(pairs_$concUnit[i], dplyr::n())) %>% 
      dplyr::select("PairIndex",	"Drug1", "Drug2", "Conc1", "Conc2", 
                    "PercentageInhibition", paste0("Synergy", method), 
                    "concUnit")
    output_table <- rbind.data.frame(output_table, tmp) 
  }
  return(output_table)
}

output$downloadSynscoresFull1 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_full_matrix_", Sys.Date(),".xlsx"),
  content <- function(file){
    outFrame <- downloadSynScoresFull()
    openxlsx::write.xlsx(outFrame, "./www/synergy_scores_full_table.xlsx", asTable = T)
    file.copy("./www/synergy_scores_full_table.xlsx", file)
  },
  contentType = NULL
)

output$downloadSynscoresFull2 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_full_matrix_", Sys.Date(),".csv"),
  content <- function(file){
    outFrame <- downloadSynScoresFull()
    write.csv(outFrame, "./www/synergy_scores_full_table.csv")
    file.copy("./www/synergy_scores_full_table.csv", file)
  },
  contentType = NULL
)

output$downloadSynscoresFull3 <- downloadHandler(
  filename <- function() paste0("result_",input$methods, "_full_matrix_", Sys.Date(),".txt"),
  content <- function(file){
    outFrame <- downloadSynScoresFull()
    write.table(outFrame, "./www/synergy_scores_full_table.txt")
    file.copy("./www/synergy_scores_full_table.txt", file)
  },
  contentType = NULL
)

observeEvent(input$Save_report,{SaveReportFunc()})
}
