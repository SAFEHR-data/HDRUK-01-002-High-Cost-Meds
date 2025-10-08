
# high cost drug cohorts -----
cli::cli_inform("Creating high cost medicines cohorts")
drug_codes <- importCodelist(path = here("cohorts", "drug_codelists"), type = "csv")
cdm$high_cost_meds <- conceptCohort(cdm, 
                                    conceptSet = drug_codes, 
                                    name = "high_cost_meds", 
                                    exit = "event_end_date")

# keep first ever, in study period, and at least min cell count
cdm$high_cost_meds <- cdm$high_cost_meds |> 
  requireIsFirstEntry() |> 
  requireInDateRange(dateRange = study_period) |> 
  requireMinCohortCount(minCohortCount =  min_cell_count)
# remove cohorts with zero counts
high_cost_meds_with_count <- cohortCount(cdm$high_cost_meds) |>
  filter(number_subjects > 0) |> 
  dplyr::pull("cohort_definition_id")
if(length(high_cost_meds_with_count) > 0){
  cdm$high_cost_meds <- subsetCohorts(cdm$high_cost_meds,
                                      cohortId = high_cost_meds_with_count, 
                                      name = "high_cost_meds")
} 
if(length(high_cost_meds_with_count) == 0){
  cli::cli_abort("All high cost drugs cohorts are empty")
}

# icd cohorts ----
cli::cli_inform("Creating ICD cohorts")
icd_codes <- importCodelist(path = here("cohorts", "icd"), type = "csv")
cdm$icd <- conceptCohort(cdm, 
                         conceptSet = icd_codes, 
                         name = "icd", 
                         exit = "event_start_date",
                         subsetCohort = "high_cost_meds")
# remove cohorts with zero counts
icd_with_count <- cohortCount(cdm$icd) |>
  filter(number_subjects > 0) |> 
  dplyr::pull("cohort_definition_id")
if(length(icd_with_count) > 0){
  cdm$icd <- subsetCohorts(cdm$icd,
                           cohortId = icd_with_count, 
                           name = "icd")
}

# procedure cohorts ----
cli::cli_inform("Creating procedure cohorts")
procedure_codes <- importCodelist(path = here("cohorts", "procedures"), type = "csv")
cdm$procedures <- conceptCohort(cdm, 
                         conceptSet = procedure_codes, 
                         name = "procedures", 
                         exit = "event_start_date",
                         subsetCohort = "high_cost_meds")
# remove cohorts with zero counts
procedures_with_count <- cohortCount(cdm$procedures) |>
  filter(number_subjects > 0) |> 
  dplyr::pull("cohort_definition_id")
if(length(procedures_with_count) > 0){
  cdm$procedures <- subsetCohorts(cdm$procedures,
                           cohortId = procedures_with_count, 
                           name = "procedures")
}

# organism cohorts ----
cli::cli_inform("Creating organism cohorts")
organism_codes <- importCodelist(path = here("cohorts", "organisms"), type = "csv")
cdm$organisms <- conceptCohort(cdm, 
                                conceptSet = organism_codes, 
                                name = "organisms", 
                                exit = "event_start_date",
                                subsetCohort = "high_cost_meds")
# remove cohorts with zero counts
organisms_with_count <- cohortCount(cdm$organisms) |>
  filter(number_subjects > 0) |> 
  dplyr::pull("cohort_definition_id")
if(length(organisms_with_count) > 0){
  cdm$organisms <- subsetCohorts(cdm$organisms,
                                  cohortId = organisms_with_count, 
                                  name = "organisms")
}


# visit cohorts -----
cdm$inpatient <- conceptCohort(
  cdm = cdm,
  conceptSet = list(inpatient = c(9201, 262, 9203)),
  name = "inpatient",
  subsetCohort = "high_cost_meds"
) |> 
  requireDuration(c(2, Inf)) # at least one night

# only inpatient high cost meds ------
# hospitalised on date of first use of high cost med
cdm$high_cost_meds <- cdm$high_cost_meds |> 
  requireCohortIntersect("inpatient",
                         window = c(0, 0))
