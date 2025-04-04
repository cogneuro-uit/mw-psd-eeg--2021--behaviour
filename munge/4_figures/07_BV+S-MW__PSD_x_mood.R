
# mood -1 <-> 1
# PSD -1 <-> 1
outputs[["figs"]][["BV+SMW__PSD_x_pre_pos"]] <-
  expand_grid(
    sleep_deviation = c(-1,0,1),
    mood_deviation  = c(-1,0,1),
    name = c("Mind wandering"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    mood  = summarised_vals$mood_pos_m + (summarised_vals$mood_pos_sd * mood_deviation),
    
    smw_pos     = mean(c$smw[,"b_pre_pos"]), 
    smw_posXpsd = mean(c$smw[,"b_c.Adjusted_Duration.diff.pos:pre_pos"]), 
    smw_psd     = mean(c$smw[,"b_c.Adjusted_Duration.diff.pos"]),
    
    bv_pos      = mean(c$mw[,"b_pre_pos"]),
    bv_posXpsd  = mean(c$mw[,"b_c.Adjusted_Duration.diff.pos:pre_pos"]),
    bv_psd      = mean(c$mw[,"b_c.Adjusted_Duration.diff.pos:zlogbv"]),
    
    smw1 = smw_pos * mood_deviation, 
    smw2 = smw_posXpsd * sleep * mood ,
    bv1  = bv_pos  * mood_deviation,
    bv2  = bv_posXpsd  * sleep * mood,
    bv = bv1 + bv2,
    smw = smw1 + smw2,
    # sleep = NULL,
    sleep_deviation = factor(sleep_deviation),
  ) |> 
  add_row(
    expand_grid(
      mood_deviation = c(-1,0,1), 
      sleep_deviation = "NS", 
    ) |> mutate(
      smw = mean(c$smw[,"b_pre_pos"]) * mood_deviation,
      bv  = mean(c$bv[,"b_pre_pos"])  * mood_deviation
    )
  ) |>
  pivot_longer(c(smw, bv), names_to="outcome") |>
  mutate(
    probe = if_else(outcome == "bv", "Behavioural Variability", "Spontaneous mind wandering"), 
    PSD = case_when(
      sleep_deviation == "-1" ~ "PSD -1 SD",
      sleep_deviation == "0"  ~ "PSD Mean",
      sleep_deviation == "1"  ~ "PSD +1 SD",
      sleep_deviation == "NS" ~ "NS",
    ) |> fct_relevel(c("PSD +1 SD", "PSD Mean", "PSD -1 SD"))
  )  |>
  ggplot(aes(mood_deviation, value, col = PSD, linetype = PSD)) + 
  facet_wrap(~probe, scales="free") +
  geom_line(linewidth = 1) +
  labs( y = "Change in outcome", x = "Positive mood (z-scored)", 
        col = "Condition", linetype="Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top")


condition_save_figure(
  outputs[["figs"]][["BV+SMW__PSD_x_pre_pos"]],
  "Interaction - Behaviour on MW",
)
