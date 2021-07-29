#' Get properties of drugs
#'
#' \code{GetPubchemPro} function retrieves the properties (InChIKey, Canonical
#' SMILES, and molecula formula) of drugs from PubChem database via
#' \href{https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865567}{PUG REST},
#' accordint to CIDs.
#'
#' @param cids A vector of integer or character indicates the CIDs of drugs.
#'
#' @return A data frame contains 4 columns:
#' \itemize{
#'   \item \strong{CID} CID of drugs which is inputted to \code{cids} argument.
#'   \item \strong{InChIKey} Standard InChIKey of matched drugs.
#'   \item \strong{CanonicalSMILES} Standard Canonical SMILES of matched drugs.
#'   \item \strong{MolecularFormula} Molecular formula for matched drugs.
#' }
#' @export
#'
#' @author
#' Jing Tang \email{jing.tang@helsinki.fi}
#' Shuyu Zheng \email{shuyu.zheng@helsinki.fi}
#'
#' @examples
#' property <- GetPubchemPro(c(1,2,3,4))
#' 
GetPubchemPro <- function(cids) {
  res <- NULL
  batch <- split(cids, ceiling(seq_along(cids)/100))
  
  n <- length(batch)
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  for (i in 1:n) {
    tryCatch({
      temp <- NULL
      compound <- paste0(batch[[i]], collapse = ",")
      property <- paste0(c("InChIKey", "IsomericSMILES"),
                         collapse = ",")
      url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",
                    compound, "/property/", property, "/CSV")
      temp <- utils::read.csv(url, stringsAsFactors = FALSE)
      res <- rbind.data.frame(res, temp)
    }, error = function(e){
      print(e)
    }, finally = {
      utils::setTxtProgressBar(pb, i)
    })
  }
  return(res)
}

#' Annotate drug
#'
#' This function is a wrapper for multiple query functions. It will query
#' PubChem, and MICHA to fetch information for drugs
#'
#' @param drug_names A vector of characters containing the names for the drugs
#'
#' @return A list with two data frames:
#'   \itemize{
#'     \item \strong{drug} It contain the basic information for drugs
#'     \item \strong{target} It contains the target information for drugs.
#'   }
#'
#' @export
#'
#' @examples
#' drug_names <- c("Gefitinib", "Paclitaxel", "sunitinib")
#' AnnotateDrug(c("aspirin", "gefitinib"))
AnnotateDrug <- function(drug_names){

  drug_names <- na.omit(unique(drug_names))
  drug <- data.frame(
    Name = drug_names,
    name_upper = toupper(drug_names), 
    stringsAsFactors = FALSE
  )
  
  # MICHA: get CID from drug name
  message("\nQuerying MICHA for CIDs...")
  config <- config::get("micha")
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = config$dbname,
    host = config$host,
    port = config$port,
    password = config$password,
    user = config$user)
  
  cid <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT id, upper_synon
      FROM public.compound_synonyms
      WHERE upper_synon in ('",
      paste(
        unique(drug$name_upper),
        collapse = "', '"
      ),
      "')"
    )
  )
  
  # DBI::dbDisconnect(con)

  cid <- cid %>% 
    unique() %>%
    dplyr::rename(name_upper = upper_synon, CID = "id")
  
  # PubChem
  message("\nQuerying PubChem...")
  pubchem <- GetPubchemPro(unique(na.omit(cid$CID)))
  pubchem$CID <- as.character(pubchem$CID)
  
  # MICHA
  message("\nQuerying MICHA for drug info...")
  
  ## cross reference
  # config <- config::get("micha")
  # con <- DBI::dbConnect(
  #   drv = RPostgres::Postgres(),
  #   dbname = config$dbname,
  #   host = config$host,
  #   port = config$port,
  #   password = config$password,
  #   user = config$user)
  cross_ref <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT * FROM public.uc_source_id_mapping
      WHERE standard_inchi_key IN ('",
      paste(pubchem$InChIKey, collapse = "', '"),
      "') AND
      name_label in ('BindingDB', 'PubChem', 'ChEMBL', 'DrugBank', 'PharmGKB', 
      'Guide to Pharmacology', 'ChEBI', 'Selleck', 'Zinc')"
    )
  )
  cross_ref <- cross_ref %>%
    dplyr::mutate(
      cross_reference = 
        paste0(
          "<a href='", base_id_url, "' target='_blank'>", name_label, ":", compound_id, "</a><br>"
      )
    ) %>% 
    dplyr::group_by(standard_inchi_key) %>% 
    summarise(cross_reference = paste(cross_reference, collapse = ""))
  
  ## Max clinical phase
  phase <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT * FROM public.uc_source_id_mapping
      WHERE standard_inchi_key IN ('",
      paste(pubchem$InChIKey, collapse = "', '"),
      "') AND
      name_label in ('BindingDB', 'ChEMBL', 'DrugBank', 'PharmGKB', 
      'Guide to Pharmacology', 'ChEBI', 'Selleck', 'Zinc')"
    )
  )
  
  ## Target
  target <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT distinct standard_inchi_key, primary_target_ids,
      primary_target_names, all_potent_target_names, max_phase,
      full_molformula
      FROM public.micha_compounds
      WHERE standard_inchi_key IN ('",
      paste(pubchem$InChIKey, collapse = "', '"),
      "')"
    )
  )
  target$all_potent_target_names <- sub("^,", "",  target$all_potent_target_names)
  ## Disease
  disease <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT standard_inchi_key, efo_term
      FROM public.drug_indication_refs
      WHERE standard_inchi_key IN ('",
      paste(pubchem$InChIKey, collapse = "', '"),
      "')"
    )
  )
  
  disease <- disease %>% 
    dplyr::group_by(standard_inchi_key) %>% 
    dplyr::summarise(disease = paste0(efo_term, collapse = ", "))
  
  DBI::dbDisconnect(con)
  
  # Assemble tables
  pubchem$CID <- as.character(pubchem$CID)
  cid$CID <- as.character(cid$CID)
  drug <- drug %>% 
    dplyr::left_join(cid, by = "name_upper") %>% 
    dplyr::left_join(pubchem, by = "CID") %>%
    dplyr::left_join(cross_ref, by = c("InChIKey" = "standard_inchi_key")) %>%
    dplyr::left_join(disease, by = c("InChIKey" = "standard_inchi_key")) %>% 
    dplyr::left_join(target, by = c("InChIKey" = "standard_inchi_key")) %>% 
    dplyr::mutate(
      cross_reference = ifelse(
        is.na(cross_reference), 
        paste0(
          "<a href='https://pubchem.ncbi.nlm.nih.gov/compound/",
          CID,
          "' target='_blank'>PubChem:",
          CID,
          "</a>"
        ),
        paste0(
          "<a href='https://pubchem.ncbi.nlm.nih.gov/compound/",
          CID,
          "' target='_blank'>PubChem:",
          CID,
          "</a><br>",
          cross_reference
        )
      )
    ) %>% 
    dplyr::arrange(Name, desc(max_phase))
  
  drug[drug == "NULL"] <- NA
  drug[drug == ""] <- NA
  
  target <- drug %>% 
    dplyr::select(
      "Drug Name" = "Name",
      "InChIKey",
      "Primary Target Name" = "primary_target_names",
      "Primary Target ID" = "primary_target_ids",
      "Potent Target Name<sup>*</sup>" = "all_potent_target_names"
    )
  drug <- drug %>% 
    dplyr::select(
      "Drug Name" = "Name",
      "InChIKey",
      "Isomeric SMILES" = "IsomericSMILES",
      "Molecular Formula" = "full_molformula",
      "Max Phase" = "max_phase",
      "Cross Reference" = "cross_reference",
      "Disease Indication" = "disease"
    )
  return(list(drug = drug, target = target))
}

