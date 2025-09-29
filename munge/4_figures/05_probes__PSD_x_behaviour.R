# Model
plot_data <- 
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(-1,0,1),
    name = c("Mind wandering"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    
    # MW ~ bv * psd 
    , bv_ns = (mean(c$mw[["b_Intercept[1]"]]) + mean(c$mw[["b_Intercept[2]"]]) + mean(c$mw[["b_Intercept[3]"]]))
    + mean(c$mw[["b_zlogbv"]]) * z_score
    , bv_psd = bv_ns
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos:zlogbv"]]) * z_score * sleep
    
    # mw ~ AE * PSD
    , ae_ns = (mean(c$mw[["b_Intercept[1]"]]) + mean(c$mw[["b_Intercept[2]"]]) + mean(c$mw[["b_Intercept[3]"]]))
    + mean(c$mw[["b_zlogapen"]]) * z_score
    , ae_psd = ae_ns
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos:zlogapen"]]) * z_score * sleep
    
    # MB ~ bv * psd
    , mb_ns = (mean(c$mb[["b_Intercept[1]"]]) + mean(c$mb[["b_Intercept[2]"]]) + mean(c$mb[["b_Intercept[3]"]]))
    + mean(c$mb[["b_zlogbv"]]) * z_score
    , mb_psd = mb_ns
    + mean(c$mb[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mb[["b_c.Adjusted_Duration.diff.pos:zlogbv"]]) * z_score * sleep
    
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
    names_probe=="bv" ~ "Behavioural variability"
    , names_probe=="ae" ~ "Approximate entropy"
    , names_probe=="mb" ~ "Mind blanking"
    ) |> fct_relevel("Behavioural variability")
  ) 

# BV    ======
figs[["MW~PSD*BV"]] <- 
  plot_data |>
  filter(probes=="Behavioural variability") |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  geom_line(linewidth = 1) +
  labs(
    title = "b"
    , y = "Association with mind wandering"
    , x = "Z-score BV"
    , col = "Condition"
    , linetype = "Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top")

conditional_save(
  figs[["MW~PSD*BV"]] + theme(legend.position = "none") + labs (title = NULL)
  , "MW - PSD * BV"
  , width = 3, height = 3
)

# AE        =======
figs[["MW~PSD*AE"]] <-
  plot_data |>
  filter(probes=="Approximate entropy") |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  geom_line(linewidth = 1) +
  labs(y = "Association with mind wandering", x = "Z-score AE", 
       col = "Condition", linetype="Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "none"
        , axis.title.y = element_blank())

conditional_save(
  figs[["MW~PSD*AE"]] + theme(axis.title.y = element_text())
  , "MW - PSD * AE"
  , width = 3, height = 3
)


# COMBINE       =====
figs[["MW~PSD*behaviour"]] <- 
  patchwork::guide_area() +
  figs[["MW~PSD*BV"]]  + figs[["MW~PSD*AE"]] +
  patchwork::plot_layout(ncol = 1, guides = "collect", heights = c(0.10, 1)
  , design = "11
  23")
  

conditional_save(
  figs[["MW~PSD*behaviour"]]
  , "Interaction - Behaviour on MW"
  , width = 5, height = 4
)

## MW+MB ~ BV       ======
figs[["(MW+MB)~PSD*BV"]] <-
  plot_data |>
  filter(!(probes == "Approximate entropy")) |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  facet_wrap(~probes) +
  coord_cartesian(ylim = c(1, 4)) + 
  geom_line(linewidth = 1) +
  labs(y = "Association with mind blanking", x = "Z-score BV", 
       col = "Condition", linetype="Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "none")

conditional_save(
  figs[["(MW+MB)~PSD*BV"]]
  , "MW+MB - Behaviour x BV"
  , width = 5, height = 3
)
