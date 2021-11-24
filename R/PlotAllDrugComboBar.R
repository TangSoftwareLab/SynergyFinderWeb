.extractDrugPlotData <- function(data,
                                  plot_block = 1,
                                  drugs = c(1, 2),
                                  plot_value = "response",
                                  statistic = NULL){
  # 1. Check the input data
  # Data structure of 'data'
  if (!is.list(data)) {
    stop("Input data is not in list format!")
  }
  if (!all(c("drug_pairs", "response") %in% names(data))) {
    stop("Input data should contain at least tow elements: 'drug_pairs' and 
         'response'. Please prepare your data with 'ReshapeData' function.")
  }
  # Parameter 'drugs'
  # if (length(drugs) != 2) {
  #   stop("The length of 'drugs' parameter is not 2. Please chosed exactly 2
  #        drugs for heatmap.")
  # }
  # Parameter 'plot_value'
  avail_value <- c("response", "response_origin", "ZIP_ref", "ZIP_fit",
                   "ZIP_synergy", "HSA_ref", "HSA_synergy", "Bliss_ref",
                   "Bliss_synergy", "Loewe_ref", "Loewe_synergy")
  if (!plot_value %in% avail_value) {
    stop("The parameter 'plot_value = ", plot_value, "' is not available.",
         "Avaliable values are '", paste(avail_value, collapse = ", "), "'.")
  }
  
  # Annotation data
  drug_pair <- data$drug_pairs[data$drug_pairs$block_id == plot_block, ] %>% 
    dplyr::select(
      dplyr::all_of(c(paste0("drug", drugs), paste0("conc_unit", drugs))),
      replicate,
      input_type
    )
  
  # Parameter 'statistic'
  if (is.null(statistic)){
    statistic_table <- drug_pair$replicate
  } else {
    avail_statistic <- c("sd", "sem", "ci")
    if (!drug_pair$replicate) {
      warning("The selected block ", plot_block,
              " doesn't have the replicate data. Statistics is not available.")
      statistic_table <- FALSE
    } else if(!statistic %in% avail_statistic) {
      warning("The parameter 'statistic = ", statistic, "' is not available.",
              "Avaliable values are ", paste(avail_statistic, sep = ", "), ".")
      statistic_table <- FALSE
    } else {
      statistic_table <- TRUE
    }
  }
  
  # 1. Extract tables for plotting
  
  # Data table
  concs <- grep("conc\\d", colnames(data$response), value = TRUE)
  selected_concs <- paste0("conc", drugs)
  
  if (statistic_table){
    if (startsWith(plot_value, "response")){
      plot_table <- data$response_statistics
    } else {
      if (!"synergy_scores" %in% names(data)){
        stop("The synergy scores are not calculated. Please run function ",
             "'CalculateSynergy' first.")
      }
      plot_table <- data$synergy_scores_statistics
    }
    plot_table <- plot_table %>% 
      dplyr::filter(block_id == plot_block) %>% 
      dplyr::ungroup()
    if (is.null(statistic)) {
      plot_table <- plot_table %>% 
        dplyr::select(
          dplyr::starts_with("conc"), 
          value = !!paste0(plot_value, "_mean")
        ) %>%
        dplyr::mutate(
          text = as.character(.roundValues(value))
        )
    } else if (statistic == "sd") {
      plot_table <- plot_table %>% 
        dplyr::select(
          dplyr::starts_with("conc"), 
          value = !!paste0(plot_value, "_mean"),
          statistic = !!paste0(plot_value, "_sd")
        ) %>%
        dplyr::mutate(
          text = paste(
            .roundValues(value), "\n" ,
            "\u00B1",
            .roundValues(statistic)
          )
        )
    } else if (statistic == "sem") {
      plot_table <- plot_table %>% 
        dplyr::select(
          dplyr::starts_with("conc"), 
          value = !!paste0(plot_value, "_mean"),
          statistic = !!paste0(plot_value, "_sem")
        ) %>%
        dplyr::mutate(
          text = paste(
            .roundValues(value), "\n" ,
            "\u00B1",
            .roundValues(statistic)
          )
        )
    } else if (statistic == "ci") {
      plot_table <- plot_table %>% 
        dplyr::select(
          dplyr::starts_with("conc"), 
          value = !!paste0(plot_value, "_mean"),
          left = !!paste0(plot_value, "_ci_left"),
          right = !!paste0(plot_value, "_ci_right")
        ) %>%
        dplyr::mutate(
          text = paste0(
            .roundValues(value), "\n",
            "[",
            .roundValues(left),
            ", ",
            .roundValues(right),
            "]"
          )
        )
    }
  } else {
    if (startsWith(plot_value, "response")){
      plot_table <- data$response
    } else {
      if (!"synergy_scores" %in% names(data)){
        stop("The synergy scores are not calculated. Please run function ",
             "'CalculateSynergy' first.")
      }
      plot_table <- data$synergy_scores
    }
    plot_table <- plot_table %>% 
      dplyr::filter(block_id == plot_block) %>% 
      dplyr::select(
        dplyr::starts_with("conc"),
        value = !!plot_value
      ) %>%
      dplyr::mutate(
        text = as.character(.roundValues(value))
      )
  }
  
  # Extract data for selected two drugs from multi-drug combination
  other_concs <- setdiff(concs, selected_concs)
  if (length(other_concs) > 0) {
    conc_zero <- apply(
      dplyr::select(plot_table, dplyr::all_of(concs)), 
      1,
      function(x) {
        sum(x != 0) <= length(selected_concs)
      })
    plot_table <- plot_table[conc_zero, ]
    other_concs_sum <- plot_table %>% 
      dplyr::ungroup() %>% 
      dplyr::select(dplyr::all_of(other_concs)) %>% 
      rowSums()
    plot_table <- plot_table[other_concs_sum == 0, ] %>% 
      dplyr::select(-dplyr::all_of(other_concs))
    colnames(plot_table) <- sapply(colnames(plot_table), function(x){
      if (x %in% selected_concs){
        return(paste0("conc", which(x == selected_concs)))
      } else {
        return(x)
      }
    })
  }
  concs_final <- grep("conc\\d", colnames(plot_table), value = TRUE)
  # Transform conc into factor
  plot_table[, concs_final] <- lapply(
    plot_table[, concs_final],
    function(x) {
      factor(.roundValues(x))
    }
  )
  plot_table <- plot_table %>% 
    dplyr::select(dplyr::all_of(concs_final), value, text)
  return(list(plot_table = plot_table, drug_pair = drug_pair))
}
.roundValues <- function(numbers) {
  numbers[abs(numbers) >= 1 & !is.na(numbers)] <- round(
    numbers[abs(numbers) >= 1 & !is.na(numbers)],
    2
  )
  numbers[abs(numbers) < 1 & !is.na(numbers)] <- signif(
    numbers[abs(numbers) < 1 & !is.na(numbers)],
    2
  )
  return(numbers)
}
PlotAllDrugComboBar <- function(data,
                                plot_value = "ZIP_synergy",
                                plot_block = 1,
                                legend_position = "right",
                                colors = NULL) {
  avail_value <- c("response", "response_origin", "ZIP_ref", "ZIP_fit",
                   "ZIP_synergy", "HSA_ref", "HSA_synergy", "Bliss_ref",
                   "Bliss_synergy", "Loewe_ref", "Loewe_synergy")
  if (!plot_value %in% avail_value) {
    stop("The parameter 'plot_value = ", plot_value, "' is not available.",
         "Avaliable values are '", paste(avail_value, collapse = ", "), "'.")
  }
  
  plot_table <- tibble::tibble(
    combination = character(),
    value = numeric()
  )
  drugs <- data$drug_pairs %>% 
    dplyr::filter(block_id == plot_block) %>% 
    dplyr::select(dplyr::starts_with("drug"))
  for (i in length(drugs):2){
    combo_drug <- combn(drugs, i)
    combo_index <- combn(1:length(drugs), i)
    for (i in 1:ncol(combo_drug)){
      drug_data <- .extractDrugPlotData(
        data = data,
        plot_block = plot_block,
        drugs = combo_index[,i],
        plot_value = plot_value,
        statistic = NULL
      )
      tmp <- drug_data$plot_table %>% 
        dplyr::filter_at(dplyr::vars(dplyr::starts_with("conc")), dplyr::all_vars(. != 0)) %>% 
        select(value) %>% 
        mutate(combination = paste(combo_drug[, i], collapse = " & "))
      plot_table <- rbind.data.frame(plot_table, tmp)
    }
  }
  plot_table$combination <- factor(plot_table$combination, levels = unique(plot_table$combination))
  y_title <- switch (plot_value,
    "response" = "Response (% inhibition)",
    "response_origin" = paste0(
      "Original response (% ",
      data$drug_pairs$input_type[data$drug_pairs$block_id == plot_block],
      ")"
    ),
    "ZIP_ref" = "ZIP additive response (% inhibition)",
    "ZIP_fit" = "ZIP fitted response (% inhibition)",
    "ZIP_synergy" = "ZIP synergy score",
    "HSA_ref" = "HSA additive response (% inhibition)",
    "HSA_synergy" = "HSA synergy score",
    "Bliss_ref" = "Bliss additive response (% inhibition)",
    "Bliss_synergy" = "Bliss synergy score",
    "Loewe_ref" = "Loewe additive response (% inhibition)",
    "Loewe_synergy" = "Loewe synergy score"
  )

  p <- plot_table %>%
    dplyr::group_by(combination) %>% 
    dplyr::summarise(
      mean = mean(value),
      sd = sd(value)
    ) %>% 
  ggplot2::ggplot(aes(x = combination, y = mean, fill = combination)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept = 0) +
    theme_classic() +
    labs(
      x = "Drug combination",
      y = y_title,
      fill = "") +
    theme(
      axis.text.x = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = legend_position,
      panel.grid.major.y = element_line(),
      legend.box="vertical"
    )
  if (!is.null(colors)){
    p <- p + 
      scale_fill_manual(values = colors)
  }
  return(p)
}
