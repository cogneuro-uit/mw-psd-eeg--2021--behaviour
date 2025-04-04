
outputs[["figs"]][["behaviour__PSD_x_time"]] <- 
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(0,1),
    name = c("Outcome"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    # Estimates
    bv_time   = mean(c$bv[,"b_probenum_prop"]), 
    bv_time.psd = mean(c$bv[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]),
    ae_time   = mean(c$ae[,"b_probenum_prop"]),
    ae_time.psd = mean(c$ae[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]),
    bv = bv_time * z_score + bv_time.psd * sleep * z_score,
    ae = ae_time * z_score + ae_time.psd * sleep * z_score,
    # sleep = NULL,
    sleep_deviation = factor(sleep_deviation),
  )  |>
  add_row(
    expand_grid(
      z_score = c(0,1), 
      sleep_deviation = "NS", 
    ) |> mutate(
      bv = mean(c$bv[,"b_probenum_prop"]) * z_score,
      ae = mean(c$ae[,"b_probenum_prop"]) * z_score
    )
  ) |>
  pivot_longer(c(bv,ae), names_to="probe") |>
  mutate(
    probe = if_else(probe=="bv", "Behavioural Variability", "Approximate Entropy") |>
      fct_relevel("Behavioural Variability"), 
    Condition = case_when(
      sleep_deviation == "-1" ~ "PSD -1 SD",
      sleep_deviation == "0"  ~ "PSD Mean",
      sleep_deviation == "1"  ~ "PSD +1 SD",
      sleep_deviation == "NS" ~ "NS",
    ) |> fct_relevel(c("PSD +1 SD", "PSD Mean", "PSD -1 SD"))
  )  |>
  ggplot(aes(z_score, value, col = Condition, linetype = Condition)) + 
  facet_wrap(~ probe) +
  geom_line(linewidth = 1) +
  labs( y = "Z-Scored Behaviour", x = "Probe number (Time-on-task)", 
        col = "Condition", linetype = "Condition") +
  scale_x_continuous(breaks = seq(0,1,1/(25/5)), labels = seq(0,25,5)) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal")


condition_save_figure(
  outputs[["figs"]][["behaviour__PSD_x_time"]], 
  "Behaviour - Changes Over Time Across Sleep Loss",
)
