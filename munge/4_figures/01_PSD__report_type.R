
# How the sleep deprivation changed as a function of the sleep adjustment:
figs[["PSD__report_type"]] <-
  sleeptimes_updated_trans |>
  select(subj, sleepdep, Self.report_Duration.diff, Actigraphy_Duration.diff, Adjusted_Duration.diff) |> 
  pivot_longer(contains("_Duration")) |>
  pivot_wider(names_from = sleepdep, values_from = value) |>
  mutate(name = case_when(
    str_starts(name, "Acti") ~ "Actigraphy", 
    str_starts(name, "Adjust") ~ "Adjusted", 
    str_starts(name, "Self") ~ "Self report") |> 
      fct_relevel(c("Self report", "Actigraphy"))) |>
  ggplot(aes(subj, SD, col = name)) + 
  stat_summary(position=position_dodge(.05)) +
  labs(y = "Sleep deprivation", col = "Source", x = "Participant") + 
  geom_hline(yintercept = -2) +
  geom_hline(yintercept = -1.5, col="red", linetype="dashed") +
  ggrepel::geom_text_repel(aes(label = if_else(SD > -1.5, subj, NA)), box.padding = .4) +
  scale_y_continuous(breaks = seq(-3,3,1)) + 
  theme(legend.position = "top")

conditional_save(
  figs[["PSD__report_type"]]
  , "Sleep deprivation"
  , width = 10
)
