# check that latest extract does have all snomed

infolder <- "\\\\sharefs6\\criudata\\Live\\OMOP\\data_catalogue\\output\\uclh_data_catalogue-20250508_194641\\public\\condition_occurrence"

files <- list.files(infolder)

library(arrow)
tst <- read_parquet(here::here(infolder,files[1]))

concept_vocabs <- tst |>
  count(condition_concept_id, sort=TRUE) |> 
  omopcept::omop_join_name_all(columns=c("concept_name","domain_id"))
  