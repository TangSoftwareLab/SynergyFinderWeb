################################################################################
# Copyright Shuyu Zheng and Jing Tang - All Rights Reserved
# Unauthorized copying of this file, via any medium is strictly prohibited
# Proprietary and confidential
# Written by Shuyu Zheng <shuyu.zheng@helsinki.fi>, November 2020
################################################################################

# TidyComb
# Functions for retrieving or updating celline information from Cellosaurus.
#
# Functions on this page:
# CellVersion: Check the Cellosaurus data version.
# UpdateCell: Update the local Cellosauruse XML file version.
# GetCellInfo: Get information of input cell lines from Cellosaurus.
#
# Internal functions:
# ParseCell: Parse the Cellosaurus XML file.
# GetCell: Extract the nodes containing information of inputted cell lines
# GetCellName: Extract cell lines' names and synonyms
# GetDisease: Extract diseases where cell lines are derived
# GetTissue: Extract tissues where cell lines are derived
# GetAccession: Extract Cellosaurus Accession for cell lines


#' Check Cellosaurus XML file version.
#'
#' \code{CellVersion} is used to check the version of a Cellosaurus XML file.
#'
#' Cellosaurus publishes its dataset as an XML file which could be downloaded
#' from \url{ftp://ftp.expasy.org/databases/cellosaurus/}. \code{CellVersion} is
#' used to check the dataset version of a "cellosaurus.xml" file. By default it
#' will check the online dataset version. If you'd like to check the version of
#' a local file, please pass the path of file to argument \code{file}.
#'
#' @param file File path to an XML file contains the Cellosaurus dataset.
#'
#' @param ... Other arguments required by \code{\link[XML]{xmlEventParse}}
#'
#' @return A named character vector. It contains: version, date of update,
#' number of archived cell lines and number of archived publications.
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export
CellVersion <- function(
          file = "ftp://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml",
          ...){
  version <- NULL
  # Building handler
  startElement = function(ctxt, name, attrs, ...) {
    if (name == "release") {
      version <<- attrs
      XML::xmlStopParser(ctxt)
    }
  }
  class(startElement) = "XMLParserContextFunction"
  # Persing xml
  invisible(XML::xmlEventParse(file,
                               handlers = list(startElement = startElement)))
  return(version)
}


#' Download the xml file for Cellosaurus database
#'
#' This function will download the xml file for Cellosaurus data from
#' https://ftp.expasy.org/databases/cellosaurus
#'
#' @param file A character value. It indicates the path and name of the file to
#'   save the downloaded data.
#'
#' @return NULL
#' @export
#'
DownloadCellosaurus <- function(file){
  url <- "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml"
  utils::download.file(url, file)
}

#' Update local Cellosaurus XML file
#'
#' \code{UpdateCell} checks and compares the versions of online and
#' local Cellosaurus XML file. If the local file is not up-to-date, it will
#' automatically update the local file.
#'
#' For parsing Cellosaurus XML file, we do \strong{not} recommend to directly
#' parse the online Cellosaurus dataset. It requires a huge amount of memory and
#' will lead to a crash of R. We recommend to download the dataset and then
#' parse the local XML file. In this case, it is necessary to update your local
#' file before processing it.
#'
#' \code{UpdateCell} using the \code{\link{CellVersion}} function to check the
#' current online version of Cellosaurus dataset and the version of a local
#' file. It will automatically update the local file if it is not up-to-date.
#'
#' Warning: The local file will be overwritted when updating data. If you'd like
#' to keep the old version of dataset, please backup it before updating.
#'
#' @param file The path to the local Cellosaurus file.
#'
#' @return A message about the version checking results. If the local file is
#' \strong{not} up-to-date, the local file will be updated with online
#' Cellosaurus data.
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export
UpdateCell <- function(file) {
  version.local <- as.numeric(CellVersion(file)["version"])
  version.online <- as.numeric(CellVersion()["version"])
  if (version.local < version.online) {
    message("Online Cellosaurus dataset published new version ",
            version.online, "\n",
            " Local file is under version ", version.local, "\n",
            "Updating local files for you, please wait for a monment...")
    url <- "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml"
    utils::download.file(url, file)
    message("Local Cellosaurus file hase been up to date now!")
  } else if (version.local == version.online) {
    message("Local file is already the latest version ",
            version.local, "No need for updating.")
  } else {
    message("There is something wrong with local Cellosaurus file. ",
            "Please check it.")
  }
}

#' Parse the Cellosaurus XML file
#'
#' \code{GetAllCell} will parse the Cellosaurus XML file and extract all content
#' in "cell-line-list" node as a \code{XML document} object.
#'
#' Cellosaurus.xml file contains 5 child nodes in its root node:
#' "header", "cell-line-list", "publication-list", "copyright". (more
#' information in "ftp://ftp.expasy.org/databases/cellosaurus/cellosaurus.xsd")
#' All the cell line informations we need for preparing data are in
#' "cell-line-list" so this function will parse the dataset file and remove all
#' rudundant informations.
#'
#' Warning: Although it is possible to parsing the online database directly by
#' passing url \url{ftp://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml}
#' to function \code{GetAllCell}, it is easy to cause crach of R as the huge
#' reuirement of memory. We recommend to download the dataset to a local file
#' and then parse this local file by using this function.
#'
#' @param file File path to a Cellosaurus xml file.
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @return An XMLNode containing all cell lines' information archieved in
#' Cellosaurus dataset.
#' @export

ParseCell <- function(file) {
  doc <- XML::xmlInternalTreeParse(file)
  all.cell <- XML::xmlRoot(doc)[[2]]
  return(all.cell)
}

#' Extract the nodes containing information of inputted cell lines
#'
#' This function searches the value of <name> tags in Cellosaurus XML document
#' to find entries that matches the names provided in the \code{names}
#' parameter.
#'
#' @param node An XMLNodelist. It is the output of \code{\link{ParseCell}}
#' function which contains all cell lines' information in Cellosaurus dataset.
#'
#' @param ids A vector of characters. It is the name or Cellosaurus accession of
#' cell lines that will be searched in Cellosaurus XML file.
#'
#' @param type A charatcer. It indicate the type of \code{id}. It could be
#' "name", "accession".
#'
#' @return An XMLNodeSet containing matched cell lines in the dataset. If no
#' cell line is matched, a NULL list will be return.
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export

GetCell <- function(node, ids, type = "name"){
  if(type == "name"){

    xpath <- paste0("//name[text()='",
                    paste(ids, collapse = "' or text() = '"),
                    "']/ancestor::cell-line")
  } else if (type == "accession") {
    xpath <- paste0("//accession[text()='",
                    paste(ids, collapse = "' or text() = '"),
                    "']/ancestor::cell-line")
  } else {
    stop("Type ", type,
         'is not allowed. Available types are: "name", "accession"')
  }
    cells <- XML::getNodeSet(node, xpath)
}

#' Extract primary name and synonyms of one cell line.
#'
#' \code{GetCellName} extract primary name and synonyms from only one
#' \emph{cell-line-list} node in Cellosaurus xml file.
#'
#' This function extracts the primary name (as "name") and synonyms (as
#' "synonyms") of an \code{XMLIntervalElementNode} object containing
#' one \emph{cell-line-list} node extracted from Cellosaurus xml file.
#'
#' If you'd like to extract information from multiple \emph{cell-line-list}
#' nodes, combining this function with \code{xpathSapply} or
#' \code{sapply} is recommanded.
#'
#' @param node An \code{XMLInternalElementNode} with only one
#' \emph{cell-line-list} node extracted from \emph{Cellosaurus xml file}.
#'
#' @return A data frame contains two variables:
#' \itemize{
#'   \item \code{name} the primary name of cell line.
#'   \item \code{synonyms} synonyms of the cell line separated by semicolons.
#' }
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export

GetCellName <- function(node) {
  tryCatch({
    name.list <- XML::xmlToDataFrame(XML::xmlChildren(node)$'name-list',
                                     stringsAsFactors = FALSE)
    name <- name.list[1,]
    synonyms <- sapply(name.list,
                       function(x) {
                         paste(x, collapse = "; ")
                       })
  }, error = function(e){
    name <<- NA
    synonyms <<- NA
  })
    names <- data.frame(name = name, synonyms = synonyms,
                        stringsAsFactors = FALSE)

  return(names)
}

#' Extract the disease information of one cell line.
#'
#' \code{GetDisease} extracts the cell line associated diseases and NCI
#' Thesaurus entry code from one \code{cell-line-list} node in Cellosaurus xml
#' file.
#'
#' This function extract the cell line associated disease and disease ID (NCI
#' Thesaurus entry code) which the cell line associated with from an
#' XMLIntervalElementNode object extracted from Cellosaurus xml file. Combining
#' this function with \code{apply} or \code{sapply} can extract disease
#' information from an XMLNodeSet object.
#'
#' @param node An XMLInternalElementNode with only one cell line's information
#' which was extracted from Cellosaurus xml file.
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @return A data frame contains two variables:
#' \item{disease}{name of the cell line associated disease.}
#' \item{disease_id}{NCI Thesaurus entry code of the associated disease.}

GetDisease <- function(node){
  tryCatch({
    disease.list <- XML::xmlChildren(node)$'disease-list'
    disease <- XML::xmlValue(disease.list)
    disease.id <- sapply(XML::xmlChildren(disease.list), XML::xmlAttrs)[2]
  }, error = function(e){
    disease <<- NA
    disease.id <<- NA
  })

  diseases <- data.frame(disease_name = disease,
                         disease_id = disease.id,
                         stringsAsFactors = FALSE)
  return(diseases)
}

#' Extract the source tissue of one cell line.
#'
#' This function extract source tissue according to cross-reference "CCLE Name"
#' from an XMLIntervalElementNode object extracted from Cellosaurus xml file.
#' Combining it with \code{apply} or \code{sapply} can extract tissue from an
#' XMLNodeSet object.
#'
#' @param node An XMLInternalElementNode with only one cell line's information
#' which was extracted from Cellosaurus xml file.
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @return A character. The tissue name of cell line according to CClE category.
GetTissue <- function(node){
  tryCatch({
    ref.list <- XML::xmlChildren(node)$`xref-list`
    ref <- sapply(XML::xmlChildren(ref.list), XML::xmlAttrs)
    ccle <- ref[3, which(ref[1,] == "CCLE")]
    tissue <- tolower(gsub("^[^_]+(?=_)_", "",ccle, perl = TRUE))
    if (length(tissue) == 0){
      tissue <- NA
    } else if (length(tissue) >1){
      tissue <- paste(unique(tissue), collapse = "; ")
    }
  }, error = function(e){
    tissue <<- NA
  })
  return(tissue)
}

#' Extract the Cellosaurus accession ID of one cell line.
#'
#' This function extract Cellosaurus accession ID of one cell line from an
#' XMLIntervalElementNode object extracted from Cellosaurus xml file. Combining
#' this function with \code{apply} or \code{sapply} can extract disease
#' information from an XMLNodeSet object.
#'
#' @param node An XMLInternalElementNode with only one cell line's information
#' which was extracted from Cellosaurus xml file.
#'
#' @return A character. The Cellosaurus accession ID of cell line
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}

GetAccession <- function(node){
  tryCatch({
    accession <- XML::xpathSApply(node,
                                  './accession-list/accession[@type="primary"]',
                                  XML::xmlValue)
  }, error = function(e){
    accession <<- NA
  })

  return(accession)
}

#' Extract cell line information
#'
#' This function will extract the primary name, synonyms, Cellosausurs Accession
#' ID, disease, disease_id, tissue) of the inputted cell lines and wrap them
#' into a data frame.
#'
#' @param parsed_cell_file An "XMLInternalElementNode" extracted from the
#' Cellosaurus XML file by function \code{\link{ParseCell}}
#'
#' @param accessions A vector containing the Cellosaurus accessions for
#' searching
#'
#' @return A data frame containing 6 columns:
#' \itemize{
#'   \item \strong{name} primary name of cell lines.
#'   \item \strong{synonyms} the synonyms of cell lines.
#'   \item \strong{cellosaurus_accession} Cellosaurus Accession for cell lines.
#'   \item \strong{disease_name} diseases that are associated with the cell
#'     lines
#'   \item \strong{disease_id} NCI Thesaurus entry code of the diseases.
#'   \item \strong{tissue} Tissues that the cell line generated from. Following
#'   the CCLE category.
#'}
#'
#' @return  cell line information selected by \code{info}
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @export

GetCellInfo <- function(accessions, parsed_cell_file) {
  temp <- NULL
  mat <- NULL
  # stepi <- 1
  # n <- length(accessions)
  # pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  cells <- GetCell(parsed_cell_file, accessions, type = "accession")
  for (cell in cells) {
    # utils::setTxtProgressBar(pb, stepi)
    # stepi <- stepi + 1
    accession <- GetAccession(cell)
    tryCatch({
      # Get cell name
      names <- GetCellName(cell)
      # Get disease
      diseases <- GetDisease(cell)
      # Assamble the result
      temp <- cbind.data.frame(names, diseases)
      # Get Accession
      temp$cellosaurus_accession <- accession
      # Get tissue
      temp$tissue <- GetTissue(cell)

    }, error = function(e){
      print(accession)
      print(e)
      temp <<- data.frame(name = NA,
                         synonyms = NA,
                         disease_name = NA,
                         disease_id = NA,
                         tissue = NA,
                         cellosaurus_accession = accession,
                         stringsAsFactors = FALSE)
  })
    mat <- rbind.data.frame(mat, temp)
    temp <- NULL
  }

  return(mat)
}
