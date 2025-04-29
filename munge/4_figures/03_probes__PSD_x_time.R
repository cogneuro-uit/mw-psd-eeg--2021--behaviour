# Data:
outputs[["figs"]][["probes__PS_x_time--data"]] <-
  data.probe.mood.sleep |>
  pivot_longer(c(mw, mb, smw)) |>
  ggplot(aes(probenum_prop, as.numeric(value), col = sleepdep)) +
  facet_wrap(~name) +
  stat_summary(geom = "line") +
  geom_smooth(method="lm") +
  scale_color_manual(values=gen_col("br")) +
  coord_cartesian(ylim=c(1,4)) +
  theme(legend.position = "top")

condition_save_figure(
  outputs[["figs"]][["probes__PS_x_time--data"]]
  , "Probes - Changes over time across sleep loss"
)


# Model:
outputs[["figs"]][["probes__PS_x_time"]] <-
  # estimate the growth 
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(0,1),
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation)
    # MW
    , mw_ns = (mean(c$mw[,"b_Intercept[1]"]) + mean(c$mw[,"b_Intercept[2]"]) + mean(c$mw[,"b_Intercept[3]"]))
    + mean(c$mw[,"b_probenum_prop"]) * z_score
    , mw_psd = mw_ns
    + mean(c$mw[,"b_c.Adjusted_Duration.diff.pos"]) * sleep 
    + mean(c$mw[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]) * z_score * sleep
    
    # MB
    , mb_ns = (mean(c$mb[,"b_Intercept[1]"]) + mean(c$mb[,"b_Intercept[2]"]) + mean(c$mb[,"b_Intercept[3]"]))
    + mean(c$mb[,"b_probenum_prop"]) * z_score
    , mb_psd = mb_ns
    + mean(c$mb[,"b_c.Adjusted_Duration.diff.pos"]) * sleep
    + mean(c$mb[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]) * z_score * sleep
    
    # SNW
    , smw_ns    = -(mean(c$smw[,"b_Intercept[1]"]) + mean(c$smw[,"b_Intercept[2]"]) + mean(c$smw[,"b_Intercept[3]"])) 
    + mean(c$smw[,"b_probenum_prop"]) * z_score
    , smw_psd = smw_ns 
    + mean(c$smw[,"b_c.Adjusted_Duration.diff.pos"]) * sleep
    + mean(c$smw[,"b_c.Adjusted_Duration.diff.pos:probenum_prop"]) * z_score * sleep
  ) |>
  pivot_longer(c(ends_with("psd"), ends_with("ns")), names_to="names") |>
  separate_wider_delim(names, "_", names_sep = "_", names = c("probe", "cond")) |>
  mutate(cond = case_when(
    names_cond=="ns" ~ "NS"
    , sleep_deviation==-1 ~ "PSD -1 SD"
    , sleep_deviation==0 ~ "PSD Mean"
    , sleep_deviation==+1 ~ "PSD +1 SD"
  ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
  , probes = case_when(
    names_probe=="mw" ~ "Mind wandering"
    , names_probe=="mb" ~ "Mind blanking"
    , names_probe=="smw" ~ "Spontaneous mind wandering"
  )) |> 
  ggplot(aes(z_score, value, col = cond, linetype=cond)) +
  facet_wrap(~probes) +
  geom_line(linewidth = 1) +
  labs( y = "Probe Response", x = "Time-on-task (probe number)", 
        col = "Condition", linetype = "Condition") +
  scale_x_continuous(breaks = seq(0,1,1/(25/5)), labels = c(1, seq(5,26,5))) + 
  scale_color_manual(    values = name_colour_interactions ) +
  scale_linetype_manual( values = name_line_interactions ) +
  coord_cartesian(ylim = c(1, 4)) + 
  theme(legend.position = "top", legend.direction = "horizontal")

  
condition_save_figure(
  outputs[["figs"]][["probes__PSD_x_time"]] 
  , "Probes - Changes over time across sleep loss"
)
