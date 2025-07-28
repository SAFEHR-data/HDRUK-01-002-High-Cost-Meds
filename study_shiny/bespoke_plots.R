library(here)
library(stringr)
library(dplyr)
library(ggplot2)
library(omopgenerics)
library(CohortCharacteristics)
library(tidytext)
library(CodelistGenerator)
library(OmopSketch)
library(tidyr)

atc_ref <- readr::read_csv("atc_ref.csv")
atc_ref <- atc_ref |> 
  mutate(cohort_name_short = stringr::str_replace(name, "^[^_]*_", ""))

res <- importSummarisedResult(here::here("data"))
res <- res |> 
  mutate(cdm_name = if_else(cdm_name == "GOSH_OMOP", "GOSH", cdm_name)) |> 
  mutate(cdm_name = if_else(cdm_name == "IDRIL_1", "IDRIL", cdm_name)) |> 
  mutate(cdm_name = if_else(str_detect(cdm_name, "UCLH"), "UCLH", cdm_name))

tableOmopSnapshot(res)

#distinct meds
tidy(res |> 
       filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))|> 
  filter(variable_name == "Number subjects") |> 
  filter(str_detect(cohort_name, "_first")) |> 
  select(cohort_name_short) |> 
  distinct() |> 
  tally()

tidy(res |> 
       filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))|> 
  filter(variable_name == "Number subjects") |> 
  filter(str_detect(cohort_name, "_first")) |> 
  select(cdm_name, cohort_name_short) |> 
  distinct() |> 
  group_by(cdm_name) |> 
  tally()


# top 5 meds
tidy(res |> 
       filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))|> 
  filter(variable_name == "Number subjects") |> 
  filter(str_detect(cohort_name, "_first")) |> 
  group_by(cdm_name) |> 
  slice_max(count, n = 5) |> 
  mutate(tot = paste0(cohort_name_short, " (", count, ")")) |> 
  select(cdm_name, tot) |> 
  pivot_wider(names_from = "cdm_name", values_from = "tot")



# Plot overall counts
counts <- tidy(res |> 
                 filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))|> 
  filter(variable_name == "Number subjects") |> 
  filter(str_detect(cohort_name, "_first")) |> 
  select("cdm_name", "cohort_name_short", "count") |> 
  arrange(desc(cohort_name_short))
counts %>%
  mutate(cohort_name_short= CodelistGenerator:::tidyWords(cohort_name_short)) |>
  mutate(cohort_name_short= str_to_sentence(cohort_name_short)) |> 
  ungroup() |> 
  ggplot() +
  facet_grid(cdm_name ~ ., scales = "free", space = "free") +
  geom_col(aes(
    x = count, 
    y = cohort_name_short,
  ), width = 1, colour = 'grey30') +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0.15, "lines"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Number of individuals", y = "")

counts %>%
  left_join(
    atc_ref %>% 
      filter(concept_class_id == "ATC 1st")
  ) %>%
  mutate(cohort_name_short= CodelistGenerator:::tidyWords(cohort_name_short)) |>
  mutate(cohort_name_short= str_to_sentence(cohort_name_short)) |> 
  ungroup() |> 
  ggplot() +
  facet_grid(concept_code ~ cdm_name, scales = "free", space = 'free') +
  geom_col(aes(
    x = count, 
    y = reorder_within(cohort_name_short, count, cohort_name_short),
    fill = atc_name
  ), width = 1, colour = 'grey30') +
  scale_y_reordered() +  
  theme_bw() +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0.15, "lines"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Number of individuals", y = "")

ggsave("counts.png", height = 11, width = 9)


counts |> 
  filter(cohort_name_short == "dexamethasone") |> 
  summarise(sum(count))

counts |> 
  filter(cohort_name_short == "ondansetron") |> 
  summarise(sum(count))

counts %>%
  filter(count > 250) %>%
  left_join(
    atc_ref %>% 
      filter(concept_class_id == "ATC 1st")
  ) %>%
  filter(!is.na(concept_class_id)) |> 
  filter(concept_code != "N") |> 
  mutate(cohort_name_short= CodelistGenerator:::tidyWords(cohort_name_short)) |>
  mutate(cohort_name_short= str_to_sentence(cohort_name_short)) |> 
  ungroup() |> 
  ggplot() +
  facet_wrap(vars(concept_code), scales = "free") +
  geom_col(aes(
    x = count, 
    y = reorder_within(cohort_name_short, count, concept_code, fun = sum),
    fill = cdm_name
  ), width = 1, colour = 'grey30') +
  scale_y_reordered() +  
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0.15, "lines"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Number of individuals", y = "")

ggsave("counts_cdm.png", height = 18, width = 21)


counts %>%
  filter(count > 250) %>%
  left_join(
    atc_ref %>% 
      filter(concept_class_id == "ATC 2nd",
             concept_code == "L03")
  ) %>%
  filter(!is.na(concept_class_id)) |> 
  filter(concept_code != "N") |> 
  mutate(cohort_name_short= CodelistGenerator:::tidyWords(cohort_name_short)) |>
  mutate(cohort_name_short= str_to_sentence(cohort_name_short)) |> 
  ungroup() |> 
  ggplot() +
  facet_wrap(vars(concept_code), scales = "free") +
  geom_col(aes(
    x = count, 
    y = reorder_within(cohort_name_short, count, concept_code, fun = sum),
    fill = cdm_name
  ), width = 1, colour = 'grey30') +
  scale_y_reordered() +  
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0.15, "lines"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Number of individuals", y = "")

ggsave("counts_cdm_lo3.png", height = 6, width = 8)









plot_data <- tidy(res |> filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Icd flag -14 to 14"),
         str_detect(variable_level, "C")) |>
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", "")) |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(percentage > 1) |>  
  inner_join(
    atc_ref    %>%
      filter(concept_code == "L03")
  ) |>
  filter(percentage > 5)|> 
  mutate(perc = percentage / 100)
max_est<-ceiling(max(plot_data$percentage)) 
plot_data  |> 
  ggplot()+
  facet_wrap(vars(cohort_name_short, cdm_name), scales = "free")+
  geom_bar(aes(cohort_name_short, 
               perc, 
               fill=variable_level), 
           width = 1, colour="black",
           stat="identity", position=position_dodge())+
  scale_y_continuous( breaks=seq(0, max_est,  length.out = 6)/100,
                      limits=c(-0.075,max_est/100))+
  theme_bw()+
  # scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
  #                              "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6", "#6a3d9a"))+ 
  theme(panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        strip.text = element_text(size=10, face="bold"), 
        panel.grid.major.x = element_blank() ,
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "bottom" ) +
  geom_text(x = 3, y = 0.1,
            size=3,
            label = "10%")+
  geom_text(x = 4, y = 0.6,
            size=3,
            label = "60%")+
  coord_polar(start = 0)

ggsave("lo3_icd.png", height = 11, width = 14)



plot_data <- tidy(res |> filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Icd flag -14 to 14")) |>
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", "")) |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(percentage > 1) |>  
  inner_join(
    atc_ref    %>%
      filter(concept_code == "J05")
  ) |>
  filter(percentage > 5)|> 
  mutate(perc = percentage / 100)
max_est<-ceiling(max(plot_data$percentage)) 
plot_data  |> 
  ggplot()+
  facet_wrap(vars(cohort_name_short, cdm_name), scales = "free")+
  geom_bar(aes(cohort_name_short, 
               perc, 
               fill=variable_level), 
           width = 1, colour="black",
           stat="identity", position=position_dodge())+
  scale_y_continuous( breaks=seq(0, max_est,  length.out = 6)/100,
                      limits=c(-0.075,max_est/100))+
  theme_bw()+
  # scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
  #                              "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6", "#6a3d9a"))+ 
  theme(panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        strip.text = element_text(size=10, face="bold"), 
        panel.grid.major.x = element_blank() ,
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "bottom" ) +
  geom_text(x = 3, y = 0.3,
            size=3,
            label = "30%")+
  geom_text(x = 4, y = 0.6,
            size=3,
            label = "60%")+
  coord_polar(start = 0)






plot_data <- tidy(res |> filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Icd flag -14 to 14")) |>
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", "")) |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(cohort_name_short %in% c("ivacaftor", "colistin")) |> 
  filter(percentage > 1) |>  
  # filter(percentage > 5)|> 
  mutate(perc = percentage / 100)
max_est<-ceiling(max(plot_data$percentage)) 
plot_data  |> 
  ggplot()+
  facet_wrap(vars(cohort_name_short, cdm_name), scales = "free")+
  geom_bar(aes(cohort_name_short, 
               perc, 
               fill=variable_level), 
           width = 1, colour="black",
           stat="identity", position=position_dodge())+
  scale_y_continuous( breaks=seq(0, max_est,  length.out = 6)/100,
                      limits=c(-0.075,max_est/100))+
  theme_bw()


# plot cohort counts
counts <- tidy(res |> 
       filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))

counts |> 
  filter(str_detect(cohort_name, "_all")) |> 
  filter(age_group == "overall"
         ,
         variable_name == "Number subjects"
         ) |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name, scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")



drug_counts_atc_1 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 1st"))
drug_counts_atc_2 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 2nd"))
drug_counts_atc_3 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 3rd"))
drug_counts_atc_4 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 4th"))



drug_counts_atc_1 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cdm_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_2 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name, cdm_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_3 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name, cdm_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_4 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name, cdm_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")


counts |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group != "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name, scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = age_group)) +
  theme_bw() +
  theme(legend.position = "top")



# counts by age group
counts |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name + cohort_name) +
  geom_col(aes(age_group, count, 
               fill = age_group))
