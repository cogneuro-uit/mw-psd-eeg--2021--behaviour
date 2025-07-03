figs[["BV__PSD_x_time"]] <- 
  plot_data |>
  filter( probes == "Behavioural variability" ) |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) + 
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
  figs[["BV__PSD_x_time"]]
  , "BV - Changes Over Time Across Sleep Loss"
)
