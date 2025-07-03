
figs[["SMW__PSD_x_pre_pos"]] <- 
  plot_data |> 
  filter(out == "Spontaneous mind wandering") |>
  ggplot(aes(mood_deviation, value, col = cond, linetype = cond)) + 
  geom_line(linewidth = 1) +
  labs( 
    y = "Change in S-MW"
    , x = "Z-scored positive affect"
    , col = "Condition"
    , linetype = "Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "none")


conditional_save(
  figs[["SMW__PSD_x_pre_pos"]]
  , "SMW - Interaction between sleep and affect"
)
