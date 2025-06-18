#uclh_count_drug_dmd.R
#andy south 2025-05-27
#to send to Ed Burn to send to the new dm+d mappers

#I can use the dmdlookup read in in code_to_runUCLHserver.R
#to save having to join on concept names
#this takes a few mins to run
freq_dmd_uclh <- cdm$drug_exposure |> 
  left_join(select(dmdlookup,drug_source_value=dmd_new,
                   drug_source_name=dmd_new_name), by="drug_source_value", copy=TRUE) |> 
  count(drug_source_value, drug_source_name, sort=TRUE) |> 
  collect()

#contains the new dmd codes
#those with names are the ones that were missing in the current ohdsi vocabs  
freq_dmd_uclh_missing <- freq_dmd_uclh |> 
  filter(!is.na(drug_source_name))

sum(freq_dmd_uclh_missing$n)
#[1] 10133956 #agrees with 10 million missing rows

#take off freq column
write_csv(select(freq_dmd_uclh,-n), file="uclh_dmd_codes.csv")