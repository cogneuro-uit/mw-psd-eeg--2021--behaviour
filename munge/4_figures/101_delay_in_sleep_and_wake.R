figs[["delay_in_wake-onset"]] <-
  sleeptimes_updated |>
  mutate(
    Condition = case_when(
      pre_control==1 ~ "NS",
      pre_sleepdep==1 ~ "PSD"
    ) |> fct_relevel("PSD") 
  ) |> 
  filter( pre_control==1 | pre_sleepdep==1 ) |> 
  mutate( wake_diff = rise_from_bed - wake_SR ) |>
  pivot_longer(c(sleep_delay, wake_diff)) |>
  mutate(name = if_else(name=="sleep_delay", "Sleep Delay", "Wake Delay")) |>
  ggplot(aes(name, value, fill = Condition)) + 
  labs(y = "Time (hour)", x = "") + 
  gghalves::geom_half_point(aes(col = Condition), alpha = .4, transformation = position_jitter(.1, 0)) +
  gghalves::geom_half_violin(side = "r", alpha = .2, scale = "width", linewidth = .1) +
  gghalves::geom_half_boxplot(nudge = .03, alpha = .2, ) 

conditional_save(
  figs[["delay_in_wake-onset"]]
  , "Delay in wake and onset time"
)
