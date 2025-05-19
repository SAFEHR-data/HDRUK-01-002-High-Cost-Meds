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

# fix observation_period
# that got messed up in latest extract
op2 <- cdm$visit_occurrence |>
  group_by(person_id) |> 
  summarise(minvis = min(coalesce(date(visit_start_datetime), visit_start_date), na.rm=TRUE),
            maxvis = max(coalesce(date(visit_end_datetime), visit_end_date), na.rm=TRUE)) |> 
  left_join(select(cdm$death,person_id,death_date), by=join_by(person_id)) |> 
  #set maxvisit to death_date if before
  #mutate(maxvis=min(maxvis, death_date, na.rm=TRUE))
  mutate(maxvis = if_else(maxvis > death_date, death_date, maxvis))

cdm$observation_period <- cdm$observation_period |>    
  left_join(op2, by=join_by(person_id)) |>
  select(-observation_period_start_date) |> 
  select(-observation_period_end_date) |> 
  rename(observation_period_start_date=minvis,
         observation_period_end_date=maxvis)

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


