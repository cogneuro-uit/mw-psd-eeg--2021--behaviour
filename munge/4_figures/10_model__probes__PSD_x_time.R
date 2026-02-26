# Data       ======
figs[["probes~PSD*time--data"]] <-
  data.probe.mood.sleep |>
  pivot_longer(c(mw, mb, smw)) |>
  ggplot(aes(probenum_prop, as.numeric(value), col = sleepdep, fill = sleepdep)) +
  facet_wrap(~name) +
  stat_summary(geom = "line") +
  stat_summary(aes(col=NULL), geom = "ribbon", alpha = .3) +
  geom_smooth(method="lm", alpha=.2) +
  scale_color_manual(values = gen_col("br")) +
  scale_fill_manual( values = gen_col("br")) +
  coord_cartesian(ylim = c(1,4)) +
  theme(legend.position = "top")

conditional_save(
  figs[["probes~PSD*time--data"]]
  , "Probes - Changes over time across sleep loss--data"
)


# Model     ======
## Data     ======
plot_data <- 
  # estimate the growth 
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(0,1),
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation)
    # MW
    , mw_ns = c$mw_i
    + mean(c$mw[["b_probenum_prop"]]) * z_score
    , mw_psd = mw_ns
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos:probenum_prop"]]) * z_score * sleep
    
    # MB
    , mb_ns = c$mb_i
    + mean(c$mb[["b_probenum_prop"]]) * z_score
    , mb_psd = mb_ns
    + mean(c$mb[["b_c.Adjusted_Duration.diff.pos"]]) * sleep
    + mean(c$mb[["b_c.Adjusted_Duration.diff.pos:probenum_prop"]]) * z_score * sleep
    
    # SNW
    , smw_ns    = c$smw_i
    + mean(c$smw[["b_probenum_prop"]]) * z_score
    , smw_psd = smw_ns 
    + mean(c$smw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep
    + mean(c$smw[["b_c.Adjusted_Duration.diff.pos:probenum_prop"]]) * z_score * sleep
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
    names_probe=="mw" ~ "Mind wandering (MW)"
    , names_probe=="mb" ~ "Mind blanking"
    , names_probe=="smw" ~ "Spontaneous MW"
  ) |> fct_relevel("Mind wandering (MW)"))

## Plot all       ======
figs[["probes~PSD*time"]] <- 
  plot_data |>
  default_time_plot() +
  facet_wrap(~probes) +
  time() +
  probe("Probe Response")

conditional_save(
  figs[["probes~PSD*time"]] 
  , "Probes - Changes over time across sleep loss", width = 6, height = 3
)

## Plot only MW      ======
figs[["MW~PSD*time"]] <- 
  plot_data |>
  filter(names_probe=="mw") |>
  default_time_plot() +
  time() +
  probe("Mind wandering")

conditional_save(
  figs[["MW~PSD*time"]]
  , "MW - Changes over time across sleep loss"
  , width = 3, height = 3
)
