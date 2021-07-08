################################################################################
# Copyright Shuyu Zheng and Jing Tang - All Rights Reserved
# Unauthorized copying of this file, via any medium is strictly prohibited
# Proprietary and confidential
# Written by Shuyu Zheng<shuyu.zheng@helsinki.fi>, November 2020
################################################################################

# TidyComb
# Functions for generating "cell_line" table for DrugComb databse.
#
# Fuctions on this page:
# MatchCellAcc: Search Cellosaurus accession according to cell line names.
# GenerateCell: Generate cell_line tabe depending on input Cellosaurus accession.

#' Match cell line name with cellosaurus accession
#'
#' @param names A vector of charactors contains names of interested cell lines
#'
#' @param file The path to Cellosaurus XML file which was posted on
#' \url{https://web.expasy.org/cellosaurus/}
#'
#' @return A data frame contains:
#' \itemize{
#'   \item \strong{input_name} The input cell lines names
#'   \item \strong{cellosaurus_accession} The cellosaurus accession matched with
#'   cell line names.
#'   \item \strong{all_name} All names for the mached cell lines, including
#'   primary name and synonyms.
#' }
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export
#'
MatchCellAcc <- function(names, file){
  doc <- ParseCell(file)
  acc <- NULL
  for (name in names){
    cell <- GetCell(doc, ids = name, type = "name")
    if (is.null(cell)){
      tmp <- data.frame(cellosaurus_accession = NA, name = NA, synonyms = NA,
                        input_name = name)
      acc <- rbind.data.frame(acc, tmp)
      next()
    }

    tmp <- lapply(cell, function(node){
      acc_tmp <- GetAccession(node)
      acc_tmp <- data.frame(cellosaurus_accession = acc_tmp,
                            stringsAsFactors = FALSE)
      name_tmp <- GetCellName(node)
      cbind.data.frame(acc_tmp, name_tmp)
    })
    tmp <- Reduce(function(x, y){rbind.data.frame(x, y)}, tmp)
    tmp$input_name <- rep_len(name, nrow(tmp))
    acc <- rbind.data.frame(acc, tmp)
  }

  acc <- acc[, c("input_name", "name","cellosaurus_accession", "synonyms")]

  return(acc)
}



#' Generate "cell_line" and "cell_id" tables
#'
#' @param acc A vector of charactors contains cellosaurus accessions of
#'  interested cell lines.
#'
#' @param file The path to Cellosaurus XML file which was posted on
#' \url{https://web.expasy.org/cellosaurus/}
#'
#' @return  A list with 2 data frames:
#' \itemize{
#'   \item \strong{cell_line} The cell_line table prepared for uploading.
#'   \item \strong{cell_id} The DrugComb IDs for new cell lines.
#' }
#'
#' @importFrom magrittr %>%
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export
#'
GenerateCell <- function(acc, file){
  if (!all(grepl("CVCL_.+", acc))) {
    stop("Illegal cellosaurus accession! Please check your input list. \n\r",
         "Cellosaurus accessions should in form 'CVCL_xxxx'.")
  }
  doc <- ParseCell(file)
  # Checking whether cell lines have been in DrugComb
  exist.cell <- CheckCell(acc)

  if (length(exist.cell$new) == 0) {
    cell_line <- data.frame(name = NA,
                            synonyms = NA,
                            disease_name = NA,
                            disease_id = NA,
                            tissue = NA,
                            cellosaurus_accession = NA,
                            id = NA,
                            stringsAsFactors = FALSE)
    tissue <- data.frame(id = NA, tname = NA)
    disease <- data.frame(id = NA, name = NA)
  } else {
    # 1. get cell line information
    cell_info <- GetCellInfo(exist.cell$new, doc)

    # 2. generate cell_id for new cell lines
    id <- seq(exist.cell$n + 1, length.out = length(exist.cell$new))

    cell_line <- cell_info %>%
      dplyr::mutate(id = id)
  }

  # Generate cell_id index table
  cell_id <- cell_line[, c("cellosaurus_accession", "id")]
  cell_id <- rbind.data.frame(cell_id, exist.cell$old)
  return(list(cell_line = cell_line,
              cell_id = cell_id))
}

#' Annotate cell lien from cell line name
#'
#' @param cell A vector of characters contains cell line names.
#'
#' @param file The path to Cellosaurus XML file which was posted on
#' \url{https://web.expasy.org/cellosaurus/}
#'
#' @return  A list with 2 data frames:
#' \itemize{
#'   \item \strong{cell_line} The cell_line table prepared for uploading.
#'   \item \strong{cell_id} The DrugComb IDs for new cell lines.
#' }
#'
#' @importFrom magrittr %>%
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export
#'
AnnotateCell <- function(cell_names, file){
  message("\nAnnotating cell lines ...")
  doc <- ParseCell(file)
  annotation_table <- NULL
  stepi <- 1
  n <- length(cell_names)
  cell <- GetCell(doc, ids = cell_names, type = "name")
  if (is.null(cell)){
    annotation_table <- data.frame(
      `Cell Name` = cell_names,
      Synonyms = NA,
      `Cellosaurus Accession` = NA,
      Tissue = NA,
      `Disease Name` = NA,
      `Disease NCIt ID` = NA
    )
  } else {
    annotation_table <- lapply(cell, function(node){
      acc_tmp <- GetAccession(node)
      acc_tmp <- data.frame(cellosaurus_accession = acc_tmp,
                            stringsAsFactors = FALSE)
      info_tmp <- GetCellInfo(accessions = acc_tmp, node)
    })
    annotation_table <- Reduce(function(x, y){rbind.data.frame(x, y)}, annotation_table)
    annotation_table <- annotation_table %>%
      dplyr::select(
        "Cell Name" = "name",
        "Synonyms" = "synonyms",
        "Cellosaurus Accession" = "cellosaurus_accession",
        "Tissue" = "tissue",
        "Disease Name" = "disease_name",
        "Disease NCIt ID" = "disease_id"
      )
  }
  

  return(annotation_table)
}