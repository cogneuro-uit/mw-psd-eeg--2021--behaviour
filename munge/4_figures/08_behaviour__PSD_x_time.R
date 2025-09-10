figs[["behaviour__PSD_x_time"]] <- 
  plot_data |>
  mutate(names_probe = if_else(names_probe == "bv", "Behavioural Variability","Approximate Entropy")) |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  facet_wrap(~ names_probe) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .25) +
  geom_line(linewidth = 1) +
  labs(
    y = "Z-Scored BV"
    , x = "Probe number (Time-on-task)"
    , col = "Condition"
    , linetype = "Condition") +
  scale_x_continuous(breaks = seq(0,1,1/(25/5)), labels = c(1, seq(5,25,5))) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal")

conditional_save(
  figs[["behaviour__PSD_x_time"]]
  , "behaviour - Changes Over Time Across Sleep Loss", 
  width = 4, height = 3
)


figs[["AE__PSD_x_time"]] <- 
  plot_data |>
  filter(names_probe == "ae") |>
  mutate(names_probe = if_else(names_probe == "bv", "Behavioural Variability","Approximate Entropy")) |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  facet_wrap(~ names_probe) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .25) +
  geom_line(linewidth = 1) +
  labs(
    y = "Z-Scored AE"
    , x = "Probe number (Time-on-task)"
    , col = "Condition"
    , linetype = "Condition") +
  scale_x_continuous(breaks = seq(0,1,1/(25/5)), labels = c(1, seq(5,25,5))) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal")

conditional_save(
  figs[["AE__PSD_x_time"]]
  , "AE - Changes Over Time Across Sleep Loss", 
  width = 3, height = 3
)


