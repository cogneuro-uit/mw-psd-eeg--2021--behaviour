# model

# mood -1 <-> 1
# PSD -1 <-> 1
figs[["BV+SMW__PSD_x_pre_pos"]] <-
  expand_grid(
    sleep_deviation = c(-1,0,1),
    mood_deviation  = c(-1,0,1),
    name = c("Mind wandering"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    mood  = summarised_vals$mood_pos_m + (summarised_vals$mood_pos_sd * mood_deviation),
    
    # PSD x positive on smw 
    , smw_ns = -(mean(c$smw[["b_Intercept[1]"]]) + mean(c$smw[["b_Intercept[2]"]]) + mean(c$smw[["b_Intercept[3]"]]))
    + mean(c$smw[["b_pre_pos"]]) * mood
    , smw_psd = smw_ns
    + mean(c$smw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$smw[["b_c.Adjusted_Duration.diff.pos:pre_pos"]]) * sleep * mood
    
    # psd x positive on BV
    , bv_ns = mean(c$bv[["b_Intercept"]])
    + mean(c$bv[["b_pre_pos"]]) * mood
    , bv_psd = bv_ns
    + mean(c$bv[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$bv[["b_c.Adjusted_Duration.diff.pos:pre_pos"]]) * sleep * mood
  ) |>
  pivot_longer(c(ends_with("psd"), ends_with("ns")), names_to="names") |>
  separate_wider_delim(names, "_", names_sep = "_", names = c("out", "cond")) |>
  mutate(cond = case_when(
    names_cond=="ns" ~ "NS"
    , sleep_deviation==-1 ~ "PSD -1 SD"
    , sleep_deviation==0 ~ "PSD Mean"
    , sleep_deviation==+1 ~ "PSD +1 SD"
  ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
  , out = case_when(
    names_out=="smw" ~ "Spontaneous mind wandering"
    , names_out=="bv" ~ "Behavioural variability"
  )) |>
  ggplot(aes(mood_deviation, value, col = cond, linetype = cond)) + 
  facet_wrap(~out, scales="free") +
  geom_line(linewidth = 1) +
  labs( y = "Change in outcome", x = "Z-scored positive mood", 
        col = "Condition", linetype="Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top")


conditional_save(
  figs[["BV+SMW__PSD_x_pre_pos"]]
  , "Interaction - Behaviour on MW"
)
""