p1 <- 
  sleeptimes_updated_trans |>
  pivot_longer(c(Self.report_Wake, Self.report_Onset, Actigraphy_Wake, Actigraphy_Onset)) |>
  separate_wider_delim(name, "_", names_sep = "_") |>
  mutate(name_1 = if_else(name_1 == "Self.report", "Self-report", name_1),
         sleepdep = if_else(sleepdep == "SD", "PSD", "NS")) |>
  ggplot(aes(name_1, value, group = subj)) + 
  facet_wrap(name_2 ~ sleepdep, scales = "free") +
  stat_summary(aes(group = interaction(sleepdep, name_2), col = NULL), 
               position = position_nudge(x=.1), col = "red") +
  stat_summary(aes(group = interaction(sleepdep, name_2), col = NULL), 
               position = position_nudge(x=.1), geom = "line", col = "red") +
  labs(x = "Modality", y = "Cumulative sleep/wake time", title = "a)")

p2 <- 
  sleeptimes_updated_trans |>
  pivot_longer(c(Self.report_Wake, Self.report_Onset, Actigraphy_Wake, Actigraphy_Onset)) |>
  separate_wider_delim(name, "_", names_sep = "_") |>
  mutate(name_1 = if_else(name_1 == "Self.report", "Self-report", name_1),
         sleepdep = if_else(sleepdep == "SD", "PSD", "NS")) |>
  ggplot(aes(name_1, value, group = subj)) + 
  facet_wrap(name_2 ~ sleepdep, scales = "free") +
  geom_point(alpha = .2, position = position_dodge(.1)) +
  geom_line(alpha = .2, position = position_dodge(.1)) +
  labs(x = "Modality", y = "Cumulative sleep/wake time", title = "b)")

figs[["onset-wake_difference"]] <- 
  p1 + p2 + patchwork::plot_layout(ncol = 1)

conditional_save(
  figs[["onset-wake_difference"]] 
  , "Onset-wake differences"
)
