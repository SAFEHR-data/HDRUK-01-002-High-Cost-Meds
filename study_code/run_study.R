results <- list()

# analysis settings -----
study_period <- c(as.Date("2022-01-01"), as.Date(NA))
study_age_groups <- list(c(0, 17), 
                         c(18, 65),
                         c(66, 150))

# CDM manipulations -----
# drop anyone missing sex or year of birth
cdm$person <- cdm$person |>
  filter(
    !is.na(gender_concept_id),
    !is.na(year_of_birth),
    gender_concept_id %in% c(8507,8532)
  )

# cdm summary -----
cli::cli_inform("Getting snapshot and observation period summary")
results[["snapshot"]] <- summariseOmopSnapshot(cdm)
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period, ageGroup = c(0,150), sex=TRUE)

# instantiate cohorts ----
source(here("cohorts", "instantiate_cohorts.R"))

# add inpatient flag -----
cdm$high_cost_meds <- cdm$high_cost_meds |> 
  addCohortIntersectFlag(targetCohortTable = "inpatient", 
                         window = c(0, 0),
                         nameStyle = "inpatient",
                         name = "high_cost_meds")


# 2025-05-23 commenting out as suggested in email from Ed to get interim results
skip_index_codes <- TRUE
if (skip_index_codes){
  cli::cli_inform("temporarily commenting out index codes for high cost meds")
} else
{
  # index codes ----
  cli::cli_inform("Get index codes for high cost meds")
  for(i in seq_along(high_cost_meds_with_count)){
    results[[paste0("high_code_diag_", i)]] <- CodelistGenerator::summariseCohortCodeUse(
      x = omopgenerics::cohortCodelist(cdm$high_cost_meds, 
                                       high_cost_meds_with_count[[i]]),
      cdm = cdm,
      cohortTable = "high_cost_meds",
      cohortId = high_cost_meds_with_count[[i]],
      timing = "entry",
      countBy = c("record", "person"),
      byConcept = TRUE
    )
  }
  
  cli::cli_inform("Get index codes for icd")
  for(i in seq_along(icd_with_count)){
    results[[paste0("icd_code_diag_", i)]] <- CodelistGenerator::summariseCohortCodeUse(
      x = omopgenerics::cohortCodelist(cdm$icd, 
                                       icd_with_count[[i]]),
      cdm = cdm,
      cohortTable = "icd",
      cohortId = icd_with_count[[i]],
      timing = "entry",
      countBy = c("record", "person"),
      byConcept = TRUE
    )
  }
}

# patient characteristics ----
cli::cli_inform("Get summary of characteristics")
results[["chars"]] <- summariseCharacteristics(cdm$high_cost_meds,
                                               ageGroup = study_age_groups,
                                               cohortIntersectFlag = list(targetCohortTable = "icd",
                                                                          window = c(-14, 14)),
                                               otherVariables = "inpatient",
                                               estimates = list(inpatient = c("binary")))  
# large scale characteristics ----
cli::cli_inform("Get large scale characteristics")
results[["lsc"]] <- summariseLargeScaleCharacteristics(cdm$high_cost_meds,
                             eventInWindow = c("condition_occurrence",
                                               "observation",
                                               "procedure_occurrence",
                                               "drug_exposure",
                                               "drug_era"),
                             window = list(c(-14, 14),
                                           c(-14, -1),
                                           c(0, 0),
                                           c(1, 14)))

# export results ----
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results, 
                       minCellCount = minCellCount,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

cli::cli_alert_success("Study finished")
