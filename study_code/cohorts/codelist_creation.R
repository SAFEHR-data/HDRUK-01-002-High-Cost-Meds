
# Note, this script does not need to be run by data partners
# it is to create the cohort definitions that they will

# packages -----
library(omopgenerics)
library(CDMConnector)
library(here)
library(DBI)
library(CodelistGenerator)
library(dplyr)
library(here)

# connect to a db with vocabularies -----
# db <- DBI::dbConnect(odbc::odbc(),
#                      Driver = "ODBC Driver 18 for SQL Server",
#                      Server = "...",
#                      Database = "...",
#                      UID = "...",
#                      PWD = "...!",
#                      TrustServerCertificate = "yes",
#                      Port = "...")
cdm <- CDMConnector::cdmFromCon(con = db,
                                cdmSchema = c("vocabularies", "vocab_2025_02"),
                                writeSchema = c("vocabularies", "results"),
                                cdmName = "vocab_2025_02")
# getVocabVersion(cdm) # "v5.0 27-FEB-25"

# create codelists -----
# for each high cost ingredient we will get it and all its descendants
hc_meds <- readr::read_csv(here("cohorts", "hc_meds.csv")) |> 
  filter(is.na(exclusion_reason))

# high cost meds -----
# protein kinase inhibitors is on the nhs_england list, so will add all
pki <- cdm$concept_ancestor |> 
  filter(ancestor_concept_id == 947753) |> 
  select(descendant_concept_id) |> 
  inner_join(cdm$concept |> 
               filter(concept_class_id == "Ingredient"), 
             by = c("descendant_concept_id" = "concept_id")) |> 
  pull(descendant_concept_id)

hc_concepts <- c(hc_meds$concept_id, pki) |> unique()

drug_codes <- getDrugIngredientCodes(cdm, 
                                     hc_concepts, 
                                     nameStyle = "{concept_name}")

# also add combinations
drug_codes[["casirivimab_imdevimab"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("casirivimab",
                                     "imdevimab"), 
                       nameStyle = "{concept_name}"))[[1]]
drug_codes[["cilgavimab_tixagevimab"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("cilgavimab",
                                       "tixagevimab"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["elexacaftor_tezacaftor_ivacaftor"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("cilgavimab",
                                       "tezacaftor",
                                       "ivacaftor"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["ketorolac_phenylephrine"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("ketorolac",
                                       "phenylephrine"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["cytarabine_daunorubicin"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("cytarabine",
                                       "daunorubicin"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["lumacaftor_ivacaftor"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("lumacaftor",
                                       "ivacaftor"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["nirmatrelvir_ritonavir"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("nirmatrelvir",
                                       "ritonavir"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["ombitasvir_paritaprevir_ritonavir_dasabuvir_ribavirin"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("ombitasvir",
                                       "paritaprevir",
                                       "ritonavir",
                                       "dasabuvir",
                                       "ribavirin"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["sofosbuvir_velpatasvir"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("sofosbuvir",
                                       "velpatasvir"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["sofosbuvir_velpatasvir_voxilaprevir"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("sofosbuvir",
                                       "velpatasvir",
                                       "voxilaprevir"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["tezacaftor_ivacaftor"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("tezacaftor",
                                       "ivacaftor"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes[["trifluoridine_tipiracil"]]<- intersectCodelists(
  getDrugIngredientCodes(cdm, name = c("trifluoridine",
                                       "tipiracil"), 
                         nameStyle = "{concept_name}"))[[1]]

drug_codes <- subsetOnDomain(drug_codes, cdm = cdm, domain = "drug")

exportCodelist(drug_codes, 
               path = here::here("cohorts", "drug_codelists"), 
               type = "csv")

# get atc equivalence ------
atc_ref <- list()
for(i in seq_along(names(drug_codes))){
  
  codes <- drug_codes[[i]]
  working_name <- names(drug_codes)[i]
  
  cli::cli_inform("Getting ATC grouping for {working_name} ({i} of {length(names(drug_codes))})") 
  
  atc_ref[[i]] <- cdm$concept |> 
    inner_join(data.frame("concept_id" = codes), 
               copy = TRUE) |> 
    select(concept_id) |> 
    left_join(cdm$concept_ancestor, 
              by = c("concept_id" = "descendant_concept_id")) |> 
    distinct() |> 
    inner_join(cdm$concept |> 
                 filter(concept_class_id %in%  
                          c("ATC 1st", "ATC 2nd",
                            "ATC 3rd", "ATC 4th",
                            "ATC 5th")) |>
                 select(ancestor_concept_id = concept_id),
               by = "ancestor_concept_id") |>
    select(concept_id = ancestor_concept_id) |> 
    left_join(cdm$concept, by = "concept_id") |> 
    select("atc_name" = "concept_name",
           "concept_class_id",
           "concept_code") |> 
    distinct() |> 
    collect() |> 
    mutate(name = working_name)
  
}
atc_ref <- bind_rows(atc_ref)

readr::write_csv(atc_ref, "atc_ref.csv")

# icd codes ----
icd_subchapter <- getICD10StandardCodes(cdm,
                                        level = c("ICD10 SubChapter"),
                                        nameStyle = "{concept_code}_{concept_name}")
icd_subchapter <- icd_subchapter[stringr::str_starts(names(icd_subchapter), 
                                                     "a15|b20|c15|c64|c73|d55|d65|n17|q20")]

icd_hierarchy <- getICD10StandardCodes(cdm,
                                       level = c("ICD10 Hierarchy"),
                                       nameStyle = "{concept_code}_{concept_name}")
icd_hierarchy <- icd_hierarchy[stringr::str_starts(names(icd_hierarchy), 
                                                   "a41|b15|b16|b25|c34|c40|c45|c49|c50|c53|c54|c56|c61|c71|c81|c82|c83|c91|c92|d70|e21|e22|e83|e84|g20|g35|i63|j12|j18|j22|j45|j47|k21|k50|k51|l03|l97|m06|m32|m81|O00|q21|r11|r50|r54|t86")]

icd_code <- getICD10StandardCodes(cdm,
                                  level = c("ICD10 Code"),
                                  nameStyle = "{concept_code}_{concept_name}")
icd_code <- icd_code[stringr::str_starts(names(icd_code), 
                                         "a047|a099|c900|d570|e870|e871|e872|e873|e875|e876|e877|g710|g936|j440|j960|j981|n390|i270|i517|q899|u071|z940")]

# add specific
icd_subchapter$b20_b24_human_immunodeficiency_virus_hiv_disease <- getDescendants(cdm, 439727) |> 
  select(concept_id) |> distinct() |> pull()

icd_code$u071_emergency_use_of_u071__covid_19_virus_identified
names(icd_code) <- stringr::str_replace(names(icd_code),
                                        "u071_emergency_use_of_u071__covid_19_virus_identified",
                                        "u071_covid_19_virus_identified")
icd_code$u071_covid_19_virus_identified <- c(icd_code$u071_covid_19_virus_identified,
                                             getDescendants(cdm, 4100065) |> 
                                               select(concept_id) |> pull()) |> unique()
icd_hierarchy$a41_other_sepsis <- c(icd_hierarchy$a41_other_sepsis, 44808681)

icd <- bind(icd_subchapter,
            icd_hierarchy,
            icd_code)
icd <- subsetOnDomain(icd, cdm = cdm, domain = c("condition", "observation"))
omopgenerics::exportCodelist(icd, 
                             path = here::here("cohorts", "icd"), 
                             type = "csv")

# procedures ----
procedures <- list(artificial_ventilation = getDescendants(cdm, 4230167) |> 
                     select(concept_id) |> distinct() |> pull(),
                   cardiopulmonary_bypass = getDescendants(cdm, 4272324) |> 
                     select(concept_id) |> distinct() |> pull(),
                   chemotherapy = getDescendants(cdm, 4273629) |> 
                     select(concept_id) |> distinct() |> pull(),
                   blood_transfusion = getDescendants(cdm, 4024656) |> 
                     select(concept_id) |> distinct() |> pull(),
                   hemodialysis = getDescendants(cdm, 4120120) |> 
                     select(concept_id) |> distinct() |> pull(),
                   general_anaesthetic = getDescendants(cdm, 4174669) |> 
                     select(concept_id) |> distinct() |> pull(),
                   pacemaker_implantation = getDescendants(cdm, 4144921) |> 
                     select(concept_id) |> distinct() |> pull(),
                   stem_cell_transplant = getDescendants(cdm, 37154552) |> 
                     select(concept_id) |> distinct() |> pull(),
                   transplant_of_kidney = getDescendants(cdm, 4322471) |> 
                     select(concept_id) |> distinct() |> pull())

procedures <- subsetOnDomain(procedures, cdm = cdm, domain = "procedure")
omopgenerics::exportCodelist(procedures, 
                             path = here::here("cohorts", "procedures"), 
                             type = "csv")


# organisms ----
organisms <- list(clostridium_difficile = c(4244400, 4055273),
     pseudomonas_aeruginosa = 4198675,
     staphylococcus_aureus = 4149419,
     escherichia_coli = 4011683)
organisms <- subsetOnDomain(organisms, cdm = cdm, domain = c("condition", "observation"))
omopgenerics::exportCodelist(organisms, 
                             path = here::here("cohorts", "organisms"), 
                             type = "csv")

