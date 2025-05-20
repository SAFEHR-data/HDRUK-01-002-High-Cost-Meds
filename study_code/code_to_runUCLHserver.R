# renv ----
renv::restore()

# libraries -----
library(omopgenerics)
library(CDMConnector)
library(OmopSketch)
library(PatientProfiles)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortCharacteristics)
library(visOmopResults)
library(DBI)
library(dplyr)
library(here)
library(readr)
library(RPostgres)
library(odbc)

## START OF SETTINGS copied between benchmarking, characterisation & antibiotics study

# acronym to identify the database
# beware dbName identifies outputs, dbname is UCLH db

dbName <- "UCLH-from-2019"
cdmSchema <- "omop_catalogue_raw"

# create a DBI connection to UCLH database
# using credentials in .Renviron or you can replace with hardcoded values here
user <- Sys.getenv("user")
host <- "uclvldddtaeps02.xuclh.nhs.uk"
port <- 5432
dbname <- "uds"
pwd <- Sys.getenv("pwduds")

# schema in database where you have writing permissions
writeSchema <- "omop_catalogue_analyse"

if("" %in% c(user, host, port, dbname, pwd, writeSchema))
  stop("seems you don't have (all?) db credentials stored in your .Renviron file, use usethis::edit_r_environ() to create")

#pwd <- rstudioapi::askForPassword("Password for omop_db")

con <- DBI::dbConnect(RPostgres::Postgres(),user = user, host = host, port = port, dbname = dbname, password=pwd)

#you get this if not connected to VPN
#Error: could not translate host name ... to address: Unknown host

#list tables
DBI::dbListObjects(con, DBI::Id(schema = cdmSchema))
DBI::dbListObjects(con, DBI::Id(schema = writeSchema))

# created tables will start with this prefix
prefix <- "hdruk_characterisation"

# minimum cell counts used for suppression
minCellCount <- 5

# to create the cdm object
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema =  writeSchema,
  writePrefix = prefix,
  cdmName = dbName,
  #cdmVersion = "5.3",
  .softValidation = TRUE
)

# a patch to remove records where drug_exposure_start_date > drug_exposure_end_date
# ~2.5k rows in 2019 extract
#defail <- cdm$drug_exposure |> dplyr::filter(drug_exposure_start_date > drug_exposure_end_date) |>  collect()

cdm$drug_exposure <- cdm$drug_exposure |> dplyr::filter(drug_exposure_start_date <= drug_exposure_end_date)

########################
# fix observation_period that got messed up in latest extract
op2 <- cdm$visit_occurrence |>
  group_by(person_id) |> 
  summarise(minvis = min(coalesce(date(visit_start_datetime), visit_start_date), na.rm=TRUE),
            maxvis = max(coalesce(date(visit_end_datetime), visit_end_date), na.rm=TRUE)) |> 
  left_join(select(cdm$death,person_id,death_date), by=join_by(person_id)) |> 
  #set maxvisit to death_date if before
  #mutate(maxvis=min(maxvis, death_date, na.rm=TRUE))
  mutate(maxvis = if_else(!is.na(death_date) & maxvis > death_date, death_date, maxvis))

cdm$observation_period <- cdm$observation_period |>    
  left_join(op2, by=join_by(person_id)) |>
  select(-observation_period_start_date) |> 
  select(-observation_period_end_date) |> 
  rename(observation_period_start_date=minvis,
         observation_period_end_date=maxvis)

#trying a small sample but still fails at bit from run_study.R
#cdm$observation_period <- cdm$observation_period |> head(100)
#results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)


############################################################################################
# temporary dmd patch (won't be necessary after next extract because now done within omop_es)
dmdlookup <- read_csv(here::here("dmdnew2old2rxnorm.csv"), col_types = "ccccic")

cdm$drug_exposure <- cdm$drug_exposure |> 
  left_join(select(dmdlookup,drug_source_value=dmd_new,
                   omop_rxnorm), by="drug_source_value", copy=TRUE) |> 
#                  omop_rxnorm), by="drug_source_value") |>   
  mutate(drug_concept_id = if_else(drug_concept_id==0, omop_rxnorm, drug_concept_id)) |> 
  select(-omop_rxnorm)

# 2025-05-19 first attempt failed with :
# Error in `validateGeneratedCohortSet()`:
#   ! cohort_start_date must be <= tham cohort_end_date. There is not the case for 1751 entries where cohort_end_date
# < cohort_start_date for subject_id 392, 709, 1043, 1497, and 1898
#opbad <- cdm$observation_period |> filter(person_id %in% c(392, 709, 1043, 1497, 1898)) |> collect()
#Yes these five were all where observation end - determined by death - was before observation start.

# so to try running quickly filter these out of person & observation period
# in future should check obsperiod itself
# ah actually there were more failures after this
#persremove <- c(392, 709, 1043, 1497, 1898)

# 1747 patients to remove 
persremove <- cdm$observation_period |> 
  filter(observation_period_end_date < observation_period_start_date) |> 
  pull(person_id)

cdm$person              <- cdm$person |> filter(! person_id %in% persremove)
cdm$observation_period  <- cdm$observation_period |> filter(! person_id %in% persremove)
cdm$visit_occurrence    <- cdm$visit_occurrence |> filter(! person_id %in% persremove)
cdm$drug_exposure        <- cdm$drug_exposure |> filter(! person_id %in% persremove)

# cdm$condition_occurrence <- cdm$condition_occurrence |> filter(! person_id %in% persremove)
# cdm$procedure_occurrence <- cdm$procedure_occurrence |> filter(! person_id %in% persremove)
# cdm$device_exposure     <- cdm$device_exposure |> filter(! person_id %in% persremove)
# cdm$observation         <- cdm$observation |> filter(! person_id %in% persremove)
# cdm$measurement         <- cdm$measurement |> filter(! person_id %in% persremove)

# Error in `validateGeneratedCohortSet()`:
#   ! Cohort can't have NA values, there are NA values in 1169155 columns: see subject_id 1, 3, 6, 7, and 10
# maybe thats caused by the location_id=NA bug ?
# try this patch replace all NAs with 1
cdm$person <- cdm$person |> mutate(location_id = ifelse(is.na(location_id),1,location_id)) |> computeQuery()

# Getting snapshot and observation period summary                                                                            
# WARNING:  column "age_group" has type "unknown"
# DETAIL:  Proceeding with relation creation anyway.
# 
# WARNING:  column "sex" has type "unknown"
# DETAIL:  Proceeding with relation creation anyway.
# 
# Attempt 1: An error occurred - Failed to collect lazy table.
# <error/rlang_error>
#   Error in `dplyr::collect()`:
#   ! Failed to collect lazy table.
# Caused by error:
#   ! Failed to prepare query : ERROR:  failed to find conversion function from unknown to text

#happens in
#results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

## END OF SETTINGS copied between benchmarking, characterisation & antibiotics study

max_tries <- 2
attempt <- 1
success <- FALSE

# Rerun study script if it fails, with a maximum number of retries
while (attempt <= max_tries && !success) {
  tryCatch({
    source(here("run_study.R"))
    success <- TRUE  # If successful, exit the loop
    message("Attempt ", attempt, ": Success!")
  }, error = function(e) {
    message("Attempt ", attempt, ": An error occurred - ", e$message)
    print(e)
    if (attempt < max_tries) {
      message("Retrying...")
    } else {
      message("Max attempts reached. Exiting.")
    }
  })
  attempt <- attempt + 1
}


