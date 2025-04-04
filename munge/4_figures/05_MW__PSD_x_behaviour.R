outputs[["figs"]][["MW__PSD_x_behaviour"]] <-
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(-1,0,1),
    name = c("Mind wandering"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    ae_m     = mean(c$mw[,"b_zlogapen"]), 
    ae_x_psd = mean(c$mw[,"b_c.Adjusted_Duration.diff.pos:zlogapen"]),
    bv_m     = mean(c$mw[,"b_zlogbv"]),
    bv_x_psd = mean(c$mw[,"b_c.Adjusted_Duration.diff.pos:zlogbv"]),
    bv = bv_m* z_score + bv_x_psd * sleep * z_score,
    ae = ae_m* z_score + ae_x_psd * sleep * z_score,
    # sleep = NULL,
    sleep_deviation = factor(sleep_deviation),
  )  |>
  add_row(
    expand_grid(
      z_score = c(-1,0,1), 
      sleep_deviation = "NS", 
    ) |> mutate(
      bv = mean(c$mw[,"b_zlogbv"]) * z_score,
      ae = mean(c$mw[,"b_zlogapen"]) * z_score
    )
  ) |>
  pivot_longer(c(bv,ae), names_to="probe") |>
  mutate(
    probe = if_else(probe=="bv", "Behavioural Variability", "Approximate Entropy"), 
    PSD = case_when(
      sleep_deviation == "-1" ~ "PSD -1 SD",
      sleep_deviation == "0"  ~ "PSD Mean",
      sleep_deviation == "1"  ~ "PSD +1 SD",
      sleep_deviation == "NS" ~ "NS",
    ) |> fct_relevel(c("PSD +1 SD", "PSD Mean", "PSD -1 SD"))
  )  |>
  ggplot(aes(z_score, value, col = PSD, linetype = PSD)) + 
  facet_wrap(~probe) +
  geom_line(linewidth = 1) +
  labs( y = "Association to mind wandering", x = "Z-score Behaviour", 
        col = "Condition", linetype="Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top")


condition_save_figure(
  outputs[["figs"]][["MW__PSD_x_behaviour"]],
  "Interaction - Behaviour on MW",
)
