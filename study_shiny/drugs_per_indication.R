
plotDrugsPerIndication <- function(working_indications, working_meds){
  tidy(res |> filterSettings(result_type == "summarise_characteristics")) |> 
    filter(!is.na(count)) |> 
    filter(count > 5) |> 
    filter(str_detect(tolower(variable_level),  
                      paste0(tolower(working_indications), collapse = "|"))) |>
    filter(cohort_name %in% working_meds) |>
    select(cdm_name, cohort_name, percentage, variable_name,
           variable_level, count) |> 
    mutate(cohort_name = str_replace_all(cohort_name, "_", " ")) |> 
    mutate(cohort_name = str_to_title(cohort_name)) |> 
    mutate(cohort_name = reorder_within(x= cohort_name, by = count, 
                                        within = list(variable_level),
                                        fun = sum)) |>
    ggplot() +
    facet_wrap(vars(variable_level), scales = "free_y", )+
    geom_bar(
      aes(count, cohort_name, 
          fill = cdm_name),
      colour = "black",
      stat = "identity"
    ) +
    theme_bw()  +
    scale_y_reordered() +
    theme(legend.position = "top", legend.title = element_blank(),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
          plot.title = element_text(size = 15, face = "bold"),
          strip.text = element_text(angle = 0, face = "bold"),
          strip.background = element_rect(fill = "grey95"),
          axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 10),
          panel.spacing = unit(0.15, "lines"),
          panel.grid.major.y = element_blank(),
    )+
    ylab("") +
    xlab("Number of new users")+
    guides(fill = guide_legend(nrow = 1))   +
    scale_fill_manual(values = custom_colors) 
}
plotDrugsPerIndication(c("HIV"),
                       getMeds("J05"))
ggsave(filename = 'jo5_hiv.png', width = 6, height = 7, dpi = 900)



plotDrugsPerIndication(c("cancer"),
                       getMeds("L01"))
ggsave(filename = 'l01_cancer.png', 
       width = 15, height = 11, dpi = 900)

plotDrugsPerIndication(c("lymphoma",
                         "leuk","myeloma"),
                       getMeds("L01"))
ggsave(filename = 'l01_lymphoma_leuk_myeloma.png', 
       width = 12, height = 9, dpi = 900)

plotDrugsPerIndication(c("Crohn's disease",
                         "Ulcerative colitis",
                         "kidney transplant",
                         "stem cell transplant",
                         "multiple sclerosis",
                         "lupus",
                         "vasculitis",
                         "rheumatoid"),
                       getMeds("L04A"))
ggsave(filename = 'l04a_crohns_uc.png', 
       width = 13, height = 11, dpi = 900)

