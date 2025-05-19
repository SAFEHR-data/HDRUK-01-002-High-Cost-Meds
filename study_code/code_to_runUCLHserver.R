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

# database details -----
db <- DBI::dbConnect("....")

# The name of your database to be used when reporting results
db_name <- "...."

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_schema <- "...."

# The name of the schema where results tables will be created
write_schema <- "...."

# A prefix that will be used when creating any tables during the study execution
write_prefix <- "...."

cdm <- cdmFromCon(db, 
                  cdmName = db_name,
                  cdmSchema = cdm_schema, 
                  writeSchema = write_schema, 
                  writePrefix = write_prefix)

# run study -----
# minimum counts that can be displayed according to data governance
min_cell_count <- 5

# Run the study
source(here("run_study.R"))
# after the study is run you should have a single csv with all your results to share
