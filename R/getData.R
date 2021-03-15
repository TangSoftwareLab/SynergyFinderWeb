transformInputData <- function(data, data_type = NULL) {
  error <- NULL
  warning <- NULL
  tryCatch({
    data <- synergyfinder::ReshapeData(
      data,
      data_type = data_type,
      impute = FALSE
    )
  }, error = function(e) {
    print(e)
    error <<- 1
  }, warning = function(w) {
    print(w)
    warning <<- w
  })
  return(list(data = data, error = error, warning = warning))
}

transformInputDataMatrix <- function(data, data_type) {
  D1 <- grep("Drug1", data[, 1])
  D2 <- grep("Drug2", data[, 1])
  CU <- grep("ConcUnit", data[, 1])
  num_pairs <- length(D1)
  error <- NULL
  warning <- NULL
  drug_pairs <- NULL
  data_table <- NULL
  for(i in 1:num_pairs){
    if (i != num_pairs) {
      tmp_mat <- data[(CU[i] + 1):(D1[i + 1] - 1), ]
    } else {
      tmp_mat <- data[(CU[i] + 1):nrow(data), ]
    }
    tmp_mat <- tmp_mat[, !apply(is.na(tmp_mat) | tmp_mat == "", 2, all)] # remove cols with all NA
    tmp_mat <- tmp_mat[!apply(is.na(tmp_mat) | tmp_mat == "", 1, all), ] # remove rows with all NA
    row_names <- as.numeric(na.omit(tmp_mat[, 1]))
    col_names <- as.numeric(na.omit(tmp_mat[1, ]))
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
    df$conc_unit <- data[CU[i], 2]
    # return data
    data_table <- rbind.data.frame(data_table, df)
  }
  data_table <- data_type[, c("block_id", "drug1", "drug2", "conc1",
                              "conc2", "response", "conc_unit")]
  tryCatch({
    data <- synergyfinder::ReshapeData(
      data_table,
      data_type = data_type,
      impute = FALSE
    )
  }, error = function(e) {
    print(e)
    error <<- 1
  }, warning = function(w) {
    print(w)
    warning <<- w
  })
  
  return(
    list(
      data = data, 
      data_table = data_table,
      error = error,
      warning = warning
    )
  )
}