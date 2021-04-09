transformInputDataMatrix <- function(data) {
  D1 <- grep("Drug1", data[, 1])
  D2 <- grep("Drug2", data[, 1])
  CU <- grep("ConcUnit", data[, 1])
  if (length(D1) != length(D2) | length(D1) != length(CU)){
    stop(paste0(
      "The input data contains <b>", length(D1), "</b> Drug1, <b>",
      length(D2), "</b> Drug2, and <b>", length(CU),"</b> ConcUnit fields.",
      "The numbers are not equal. See example data."
    ))
  }
  num_pairs <- length(D1)
  data_table <- NULL
  for(i in 1:num_pairs){
    if (i != num_pairs) {
      tmp_mat <- data[(CU[i] + 1):(D1[i + 1] - 1), ]
    } else {
      tmp_mat <- data[(CU[i] + 1):nrow(data), ]
    }
    tmp_mat <- tmp_mat[, !apply(is.na(tmp_mat) | tmp_mat == "", 2, all)] # remove cols with all NA
    tmp_mat <- tmp_mat[!apply(is.na(tmp_mat) | tmp_mat == "", 1, all), ] # remove rows with all NA
    row_names <- tmp_mat[, 1]
    row_names <- as.numeric(row_names[row_names != "" & !is.na(row_names)])
    col_names <- tmp_mat[1, ]
    col_names <- as.numeric(col_names[col_names != "" & !is.na(col_names)])
    resp_mat <- matrix(
      sapply(tmp_mat[-1, -1], as.numeric),
      ncol = length(col_names),
      nrow = length(row_names))
    rownames(resp_mat) <- row_names
    colnames(resp_mat) <- col_names
    df <- reshape2::melt(resp_mat)
    colnames(df) <- c("conc1", "conc2", "response")
    df$block_id <- i
    df$drug1 <- data[D1[i], 2]
    df$drug2 <- data[D2[i], 2]
    if (grepl("cell", tolower(data[D2[i] + 1, 1]), fixed = TRUE)) {
      cell <- trimws(data[D2[i] + 1, 2])
      if (is.na(cell)) {
        cell <- NA
      } else if (cell == "") {
        cell <- NA
      } else {
        df$cell_line_name <- cell
      }
    } else {
      df$cell_line_name <- NA
    }
    df$conc_unit <- data[CU[i], 2]
    # return data
    data_table <- rbind.data.frame(data_table, df)
  }
  
  if (all(is.na(data_table$cell_line_name))) {
    data_table <- data_table[, c("block_id", "drug1", "drug2", "conc1", "conc2",
                                 "response", "conc_unit")]
  } else {
    data_table <- data_table[, c("block_id", "drug1", "drug2", "cell_line_name",
                                 "conc1", "conc2", "response", "conc_unit")]
  }
  
  return(data_table)
}