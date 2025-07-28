

plot_atc_2 <- function(starts){
  counts %>%
    left_join(
      atc_ref %>% 
        filter(concept_class_id == "ATC 2nd",
               str_starts(concept_code, starts))) %>%
    filter(!is.na(concept_class_id)) |> 
    mutate(cohort_name_short= CodelistGenerator:::tidyWords(cohort_name_short)) |>
    mutate(cohort_name_short= str_to_sentence(cohort_name_short)) |> 
    ungroup() |> 
    ggplot() +
    # facet_wrap(vars(concept_code), scales = "free", ncol = 1) +
    facet_grid(concept_code ~ ., scales = "free_y", space = "free") +
    geom_col(aes(
      x = count, 
      y = reorder_within(cohort_name_short, count, concept_code, fun = sum),
      fill = cdm_name
    ), width = 1, colour = 'grey30') +
    scale_y_reordered() +  
    theme_bw() +
    theme(legend.position = "top", legend.title = element_blank(),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
      plot.title = element_text(size = 15, face = "bold"),
      strip.text.y = element_text(angle = 0, face = "bold"),
      strip.background = element_rect(fill = "grey95"), 
      axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 10),
      panel.spacing = unit(0.15, "lines"),
      panel.grid.major.y = element_blank(),
    )+
    labs(x = "Number of individuals", y = "")
  
}
plot_atc_2("A") + ggtitle("ATC Group A: ALIMENTARY TRACT AND METABOLISM")
plot_atc_2("B") + ggtitle("ATC Group B: BLOOD AND BLOOD FORMING ORGANS")
plot_atc_2("C") + ggtitle("ATC Group C: CARDIOVASCULAR SYSTEM")
plot_atc_2("D") + ggtitle("ATC Group D: DERMATOLOGICALS")
plot_atc_2("G") + ggtitle("ATC Group G: GENITO URINARY SYSTEM AND SEX HORMONES")
plot_atc_2("H") + ggtitle("ATC Group H: SYSTEMIC HORMONAL PREPARATIONS, EXCL. SEX HORMONES AND INSULINS")
plot_atc_2("J") + ggtitle("ATC Group J: ANTIINFECTIVES FOR SYSTEMIC USE") 
plot_atc_2("L") + ggtitle("ATC Group L: ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS")
plot_atc_2("M") + ggtitle("ATC Group M: MUSCULO-SKELETAL SYSTEM")
plot_atc_2("N") + ggtitle("ATC Group N: NERVOUS SYSTEM")
plot_atc_2("R") + ggtitle("ATC Group R: RESPIRATORY SYSTEM")
plot_atc_2("S") + ggtitle("ATC Group S: SENSORY ORGANS")
plot_atc_2("V") + ggtitle("ATC Group V: VARIOUS")


