outputs[["figs"]][["Mood__PSD"]] <-
  expand_grid(
    # SD can vary from -1 to 1 (z score)
    sleep_deviation = c(-1, 0, 1),
    when = c(0, 1), # before/after
    name = c("Outcome")
  ) |> 
  mutate(
    # Z-score mood and sleep
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    mood_pos_val = summarised_vals$mood_pos_m + (summarised_vals$mood_pos_sd * sleep_deviation), 
    mood_neg_val = summarised_vals$mood_neg_m + (summarised_vals$mood_neg_sd * sleep_deviation), 
    
    # Estimates
    pos_int      = mean(mood$p[,"b_Intercept"]), 
    pos_when     = mean(mood$p[,"b_prepostpost"]),  
    pos_psd      = mean(mood$p[,"b_c.Adjusted_Duration.diff.pos"]), 
    pos_psd.when = mean(mood$p[,"b_c.Adjusted_Duration.diff.pos:prepostpost"]),
    
    neg_int      = mean(mood$n[,"b_Intercept"]), 
    neg_when     = mean(mood$n[,"b_prepostpost"]),  
    neg_psd      = mean(mood$n[,"b_c.Adjusted_Duration.diff.pos"]), 
    neg_psd.when = mean(mood$n[,"b_c.Adjusted_Duration.diff.pos:prepostpost"]),
    
    # Estimate coeffs
    pos_psd.1 = pos_psd * sleep,
    pos_psd.2 = pos_psd.when * sleep * when,
    neg_psd.1 = neg_psd * sleep,
    neg_psd.2 = neg_psd.when * sleep * when,
    pos_psd.. = pos_int + pos_psd.1 + pos_psd.2 + pos_when * when,  
    neg_psd.. = neg_int + neg_psd.1 + neg_psd.2 + neg_when * when,  
    
    sleep_deviation = factor(sleep_deviation)
  ) |> 
  add_row(
    expand_grid(
      when = c(0, 1), 
      sleep_deviation = "NS"
    ) |> mutate(
      pos_int  = mean(mood$p[,"b_Intercept"]),  
      pos_when = mean(mood$p[,"b_prepostpost"]),
      neg_int  = mean(mood$n[,"b_Intercept"]),
      neg_when = mean(mood$n[,"b_prepostpost"]),
      
      # Estimate coeffs
      pos_psd.. = pos_int + pos_when * when,
      neg_psd.. = neg_int + neg_when * when
    ) |> select(when, sleep_deviation, pos_psd.., neg_psd..)
  ) |>
  pivot_longer(c(pos_psd.., neg_psd..), names_to = "mood") |> 
  mutate(
    mood = if_else(mood == "pos_psd..", "Positive mood", "Negative mood") |>
      fct_relevel("Positive mood"), 
    Condition = case_when(
      sleep_deviation == "-1" ~ "PSD -1 SD",
      sleep_deviation == "0"  ~ "PSD Mean",
      sleep_deviation == "1"  ~ "PSD +1 SD",
      sleep_deviation == "NS" ~ "NS"
    ) |> fct_relevel(c("PSD +1 SD", "PSD Mean", "PSD -1 SD"))
  ) |> 
  ggplot(aes(when, value, col = Condition, linetype = Condition)) + 
  facet_wrap(~ mood, scales="free") +
  geom_line(linewidth = 1) +
  labs(y = "Mood score", x = "Task", col = "Condition", linetype = "Condition") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Pre", "Post")) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal") + 
  coord_cartesian(xlim = c(-.2,1.2))

condition_save_figure(
  outputs[["figs"]][["Mood__PSD"]],
  "Effect of sleep deprivation on mood",
)

