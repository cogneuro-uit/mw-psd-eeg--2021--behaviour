
outputs[["figs"]][["probes__PSD_x_time"]] <-
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(0,1),
    name = c("Outcome"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    # Estimates
    mw_time      = mean(c$mw[,"b_probenum_prop"]), 
    mw_time.psd  = mean(c$mw[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]),
    mb_time      = mean(c$mb[,"b_probenum_prop"]), 
    mb_time.psd  = mean(c$mb[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]),
    smw_time     = mean(c$smw[,"b_probenum_prop"]),
    smw_time.psd = mean(c$smw[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]),
    mw  = mw_time  * z_score + mw_time.psd  * sleep * z_score,
    mb  = mb_time  * z_score + mb_time.psd  * sleep * z_score,
    smw = smw_time * z_score + smw_time.psd * sleep * z_score,
    # sleep = NULL,
    sleep_deviation = factor(sleep_deviation),
  )  |>
  add_row(
    expand_grid(
      z_score = c(0,1), 
      sleep_deviation = "NS", 
    ) |> mutate(
      mw  = mean(c$mw[,"b_probenum_prop"]) * z_score,
      mb  = mean(c$mb[,"b_probenum_prop"]) * z_score,
      smw = mean(c$smw[,"b_probenum_prop"]) * z_score,
    )
  ) |>
  pivot_longer(c(mw,mb,smw), names_to="probe") |>
  mutate(
    probe = case_when(
      probe=="mw" ~ "Mind wandering",
      probe=="mb"~"Mind blanking", 
      T~"Spontaneous mind wandering") |> 
      fct_relevel("Mind wandering"), 
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
  labs( y = "Z-Scored Probe Response", x = "Time-on-task (probe number)", 
        col = "Condition", linetype = "Condition") +
  scale_x_continuous(breaks = seq(0,1,1/(25/5)), labels = seq(0,25,5)) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal")


condition_save_figure(
  outputs[["figs"]][["probes__PSD_x_time"]], 
  "Probes - Changes over time across sleep loss",
)
