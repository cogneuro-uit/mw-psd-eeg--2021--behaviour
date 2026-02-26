## Data plot      =======
figs[["data--behaviour~PSD*time"]] <-
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
  figs[["data--behaviour~PSD*time"]]
  , "Behaviour - Changes Over Time Across Sleep Loss -- data"
)

# Model      ======
## Data transf       ======
plot_data <- 
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
  separate_wider_delim(names, "_", names_sep = "_", names = c("behav", "cond")) |>
  mutate(cond = case_when(
    names_cond=="ns" ~ "NS"
    , sleep_deviation==-1 ~ "PSD -1 SD"
    , sleep_deviation==0 ~ "PSD Mean"
    , sleep_deviation==+1 ~ "PSD +1 SD"
  ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
  , behav = case_when(
    names_behav=="bv" ~ "Behavioural variability"
    , names_behav=="ae" ~ "Approximate entropy"
  ) |> fct_relevel("Behavioural variability")
  )


###  Approximate Entropy - plot     ======
figs[["AE~PSD*time"]] <- 
  plot_data |>
  filter(behav=="Approximate entropy") |>
  default_time_plot() +
  time() + 
  z_score("Z-score AE")

conditional_save(
  figs[["AE~PSD*time"]]
  , "AE - Changes Over Time Across Sleep Loss"
  , width = 3, height = 3
)

### Behavioural variability - plot    ======
figs[["BV~PSD*time"]] <- 
  plot_data |>
  filter(behav=="Behavioural variability") |>
  default_time_plot() +
  time() + 
  z_score("Z-score BV", zoom = c(-1,1.1))

conditional_save(
  figs[["BV~PSD*time"]]
  , "BV - Changes Over Time Across Sleep Loss"
)

### Combined plot    ======
figs[["behav~PSD*time"]] <- 
  plot_data |>
  default_time_plot() +
  facet_wrap(~behav) + 
  time() + 
  z_score("Z-score behaviour", zoom = c(-1,1.1))
  
conditional_save(
  figs[["behav~PSD*time"]]
  , "Behaviour - Changes Over Time and across PSD"
)



