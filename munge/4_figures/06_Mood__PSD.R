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
    
    # positive mood
    , pos_ns = mean(mood$p[,"b_Intercept"])
    + mean(mood$p[,"b_prepostpost"]) * when
    , pos_psd = pos_ns
    + mean(mood$p[,"b_c.Adjusted_Duration.diff.pos"]) * sleep 
    + mean(mood$p[,"b_c.Adjusted_Duration.diff.pos:prepostpost"]) * when * sleep
    
    # negative mood
    , neg_ns = mean(mood$n[,"b_Intercept"])
    + mean(mood$n[,"b_prepostpost"]) * when
    , neg_psd = neg_ns
    + mean(mood$n[,"b_c.Adjusted_Duration.diff.pos"]) * sleep 
    + mean(mood$n[,"b_c.Adjusted_Duration.diff.pos:prepostpost"]) * when * sleep
  ) |>
  pivot_longer(c(ends_with("psd"), ends_with("ns")), names_to="names") |>
  separate_wider_delim(names, "_", names_sep = "_", names = c("mood", "cond")) |>
  mutate(cond = case_when(
    names_cond=="ns" ~ "NS"
    , sleep_deviation==-1 ~ "PSD -1 SD"
    , sleep_deviation==0 ~ "PSD Mean"
    , sleep_deviation==+1 ~ "PSD +1 SD"
  ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
  , mood = case_when(
    names_mood=="pos" ~ "Positive"
    , names_mood=="neg" ~ "Negative"
  ) |> fct_relevel("Positive")
  ) |> 
  ggplot(aes(when, value, col = cond, linetype = cond)) + 
  facet_wrap(~ mood, scales="free") +
  geom_line(linewidth = 1) +
  labs(y = "Mood score", x = "Task", col = "Condition", linetype = "Condition") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Pre", "Post")) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal") + 
  coord_cartesian(xlim = c(-.2,1.2))

condition_save_figure(
  outputs[["figs"]][["Mood__PSD"]]
  , "Effect of sleep deprivation on mood"
)

