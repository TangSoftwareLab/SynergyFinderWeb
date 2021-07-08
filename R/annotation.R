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
#' AnnotateDrug(c("aspirin", "gefitinib"))
AnnotateDrug <- function(drug_names){
  # PubChem
  message("\nQuerying PubChem...")
  pubchem <- NULL
  url.base <- paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/%s/",
    "property/InChIKey,IsomericSMILES/JSON")
  stepi <- 1
  n <- length(drug_names)
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  for (name in drug_names) {
    tmp <- NULL
    tryCatch({
      url <- sprintf(url.base, utils::URLencode(name))
      
      doc <- jsonlite::fromJSON(url)
      rootNode <- names(doc)
      if (rootNode == "PropertyTable") {
        tmp <- doc$PropertyTable$Properties
      } else {
        tmp <- data.frame(
          CID = NA,
          IsomericSMILES = NA,
          InChIKey = NA,
          stringsAsFactors = FALSE
        )
      }
    }, error = function(e) {
      # if (!quiet) {
      #   print(e)
      # }
      tmp <<- data.frame(
        CID = NA,
        IsomericSMILES = NA,
        InChIKey = NA,
        stringsAsFactors = FALSE
      )
    }, finally = Sys.sleep(0.2) # See usage policy.
    )
    tmp$Name <- name
    pubchem <- rbind.data.frame(pubchem, tmp)
    
    utils::setTxtProgressBar(pb, stepi)
    stepi <- stepi + 1
  }
  
  # MICHA
  ## cross reference
  message("\nQuerying MICHA...")
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "micha",
    host = "195.148.22.119",
    port = "5432",
    password = "F6XDctDx88hC",
    user = "shuyu")
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
    mutate(
      cross_ref = 
        paste0(
          "<a href='", base_id_url, "' target='_blank'>", name_label, ":", compound_id, "</a><br>"
      )
    ) %>% 
    dplyr::group_by(standard_inchi_key) %>% 
    summarise(cross_ref = paste(cross_ref, collapse = ""))
  
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
    summarise(disease = paste0(efo_term, collapse = ", "))
  
  # Assemble tables
  drug <- pubchem %>%
    left_join(cross_ref, by = c("InChIKey" = "standard_inchi_key")) %>%
    left_join(disease, by = c("InChIKey" = "standard_inchi_key")) %>% 
    left_join(target, by = c("InChIKey" = "standard_inchi_key")) %>% 
    mutate(
      cross_ref = paste0(
        "<a href='https://pubchem.ncbi.nlm.nih.gov/compound/",
        CID,
        "' target='_blank'>PubChem:",
        CID,
        "</a><br>",
        cross_ref
      )
    )
  
  drug[which(drug == "NULL", arr.ind = TRUE)] <- NA
  drug[which(drug == "", arr.ind = TRUE)] <- NA
  
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
      "Cross Reference" = "cross_ref",
      "Disease Indication" = "disease"
    )
  return(list(drug = drug, target = target))
}
