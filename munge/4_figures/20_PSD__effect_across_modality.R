# How the sleep deprivation changed as a function of the sleep adjustment:
figs[["PSD__report_type_summarised"]] <-
  sleeptimes_updated_all_long |>
  filter(sleepdep=="SD") |>
  filter( str_starts(report, "c.") & str_ends(type,".diff") ) |>
  mutate(
    report = str_remove_all(report, "c\\.") |> 
      str_replace_all("\\.", "-") |>
      fct_relevel(c("Self-report", "Actigraphy")) 
  ) |>
  ggplot(aes(report, value, col = report)) +
  geom_hline(yintercept=0, linetype="dashed", alpha=.2) +
  stat_summary(aes(group=subj), fun.data = mean_se, position = position_dodge(.65), alpha = .08) +
  stat_summary(aes(group=subj, col = NULL), fun.data = mean_se, position = position_dodge(.65), alpha = .05, geom="line") +
  stat_summary() +
  stat_summary(geom = "line") +
  scale_color_manual(values=gen_col("rbg")) +
  labs(x ="", y = "Difference to normal sleep") +
  theme(legend.position = "none")

conditional_save(
  figs[["PSD__report_type_summarised"]]
  , "Sleep deprivation summarised"
  , width = 10
)