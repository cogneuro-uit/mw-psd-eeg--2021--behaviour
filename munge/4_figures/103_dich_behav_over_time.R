figs[["dich_behav_over_time"]] <- 
  data.probe.mood.sleep |>
  mutate(
    condition_exc  = if_else(Adjusted_Duration.diff <= -1.5, TRUE, FALSE),
    Block    = probenum,
    Condition = if_else(sleepdep=="SD", "PSD", "NS") |>
      fct_relevel("PSD", after = 0),
  )  |>
  pivot_longer(c( zlogapen, zlogbv), values_to = "val") |>
  mutate(name = fct_relevel(name, "Mind wandering", after = 0)) |>
  ggplot(aes(Block, val, fill = Condition, col = Condition )) +
  facet_wrap(~name) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # ALL DATA
  stat_summary(aes(col = NULL), geom = "ribbon", alpha = .25) +
  stat_summary(geom = "line", alpha = .7) +
  geom_smooth(method = "lm", alpha = .15, linewidth=.4) +
  labs(y = "Centered probe response", x = "Probe number (trial)") +
  theme(legend.position = "top")

conditional_save(
  figs[["dich_behav_over_time"]]
  , "Dichotomous - Behaviour over time"
)
