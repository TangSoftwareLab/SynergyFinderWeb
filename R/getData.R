transformInputData <- function(data, data_type = NULL) {
  error <- NULL
  warning <- NULL
  tryCatch({
    dataOutput <- synergyfinder::ReshapeData(
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
  return(list(dataOutput, error = error, warning = warning))
}

transformInputDataMatrix <- function(dataOutput) {
  dose.response.mats <- list()
  D1 <- grep("Drug1", dataOutput[, 1])
  D2 <- grep("Drug2", dataOutput[, 1])
  CU <- grep("ConcUnit", dataOutput[, 1])
  num_pairs <- length(D1)
  error <- NULL
  warning <- NULL
  drug_pairs <- NULL
  data_table <- NULL
  for(i in 1:num_pairs){
    if (i != num_pairs) {
      tmp_mat <- sapply(
        dataOutput[(CU[i] + 1):(D1[i + 1] - 1), ],
        as.numeric
      ) 
    } else {
      tmp_mat <- sapply(
        dataOutput[(CU[i] + 1):nrow(dataOutput), ],
        as.numeric
      )
    }
    tmp_mat <- tmp_mat[, !apply(is.na(tmp_mat) | tmp_mat == "", 2, all)] # remove cols with all NA
    tmp_mat <- tmp_mat[!apply(is.na(tmp_mat) | tmp_mat == "", 1, all), ] # remove rows with all NA
    row_names <- tmp_mat[2:nrow(tmp_mat), 1]
    col_names <- as.numeric(tmp_mat[1, 1:(ncol(tmp_mat) - 1)])
    resp_mat <- tmp_mat[-1, -1]
    rownames(resp_mat) <- row_names
    colnames(resp_mat) <- col_names
    df <- reshape2::melt(resp_mat)
    colnames(df) <- c("conc1", "conc2", "response")
    df$block_id <- i
    df$drug1 <- dataOutput[D1[i],2]
    df$drug2 <- dataOutput[D2[i],2]
    df$conc_unit <- dataOutput[CU[i],2]
    # return data
    data_table <- rbind.data.frame(data_table, df)
  }
  tryCatch({
    dataOutput <- synergyfinder::ReshapeData(
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
  
  return(list(data, error = error, warning = warning))
}