# Data:
figs[["behaviour__PSD_x_time--data"]] <-
  data.probe.mood.sleep |>
  pivot_longer(c(zlogbv, zlogapen)) |>
  ggplot(aes(probenum_prop, value, col = sleepdep, fill = sleepdep)) +
  facet_wrap(~name) +
  stat_summary(geom = "line") +
  stat_summary(aes(col=NULL), geom = "ribbon", alpha = .2) +
  geom_smooth(method="lm", alpha =.2) +
  scale_color_manual(values = gen_col("br")) +
  scale_fill_manual( values = gen_col("br")) +
  theme(legend.position = "top")

conditional_save(
  figs[["behaviour__PSD_x_time--data"]]
  , "Behaviour - Changes Over Time Across Sleep Loss -- data"
)

# model
figs[["behaviour__PSD_x_time"]] <- 
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(0,1),
    name = c("Outcome"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    
    # BV
    , bv_ns = mean(c$bv[["b_Intercept"]])
    + mean(c$bv[["b_probenum_prop"]]) * z_score
    , bv_psd = bv_ns
    + mean(c$bv[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$bv[["b_c.Adjusted_Duration.diff.pos:probenum_prop"]]) * z_score * sleep
    
    # AE
    , ae_ns = mean(c$ae[["b_Intercept"]])
    + mean(c$ae[["b_probenum_prop"]]) * z_score
    , ae_psd = ae_ns
    + mean(c$ae[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$ae[["b_c.Adjusted_Duration.diff.pos:probenum_prop"]]) * z_score * sleep
  )  |>
  pivot_longer(c(ends_with("psd"), ends_with("ns")), names_to="names") |>
  separate_wider_delim(names, "_", names_sep = "_", names = c("probe", "cond")) |>
  mutate(cond = case_when(
    names_cond=="ns" ~ "NS"
    , sleep_deviation==-1 ~ "PSD -1 SD"
    , sleep_deviation==0 ~ "PSD Mean"
    , sleep_deviation==+1 ~ "PSD +1 SD"
  ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
  , probes = case_when(
    names_probe=="bv" ~ "Behavioural variability"
    , names_probe=="ae" ~ "Approximate entropy"
    ) |> fct_relevel("Behavioural variability")
  ) |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) + 
  facet_wrap(~ probes) +
  geom_line(linewidth = 1) +
  labs( y = "Z-Scored Behaviour", x = "Probe number (Time-on-task)", 
        col = "Condition", linetype = "Condition") +
  scale_x_continuous(breaks = seq(0,1,1/(25/5)), labels = c(1, seq(5,25,5))) + 
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top", legend.direction = "horizontal")


conditional_save(
  figs[["behaviour__PSD_x_time"]]
  , "Behaviour - Changes Over Time Across Sleep Loss"
)
