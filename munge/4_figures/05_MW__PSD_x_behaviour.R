
# Model
plot_data <- 
  expand_grid(
    sleep_deviation = c(-1,0,1),
    z_score = c(-1,0,1),
    name = c("Mind wandering"), 
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    
    # BV effect on MW 
    , bv_ns = (mean(c$mw[["b_Intercept[1]"]]) + mean(c$mw[["b_Intercept[2]"]]) + mean(c$mw[["b_Intercept[3]"]]))
    + mean(c$mw[["b_zlogbv"]]) * z_score
    , bv_psd = bv_ns
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos:zlogbv"]]) * z_score * sleep
    
    # AE
    , ae_ns = (mean(c$mw[["b_Intercept[1]"]]) + mean(c$mw[["b_Intercept[2]"]]) + mean(c$mw[["b_Intercept[3]"]]))
    + mean(c$mw[["b_zlogapen"]]) * z_score
    , ae_psd = ae_ns
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos:zlogapen"]]) * z_score * sleep
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
    ) |> fct_relevel("Behavioural variability")
  ) 

# BV
p1 <- 
  plot_data |>
  filter(probes=="Behavioural variability") |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  geom_line(linewidth = 1) +
  labs(
    title = "b"
    , y = "Association to mind wandering"
    , x = "Z-score BV"
    , col = "Condition"
    , linetype = "Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "top")

# AE
p2 <-
  plot_data |>
  filter(probes=="Approximate entropy") |>
  ggplot(aes(z_score, value, col = cond, linetype = cond)) +
  geom_line(linewidth = 1) +
  labs(y = "Association to mind wandering", x = "Z-score AE", 
       col = "Condition", linetype="Condition") +
  scale_color_manual(   values = name_colour_interactions ) +
  scale_linetype_manual(values = name_line_interactions ) +
  theme(legend.position = "none"
        , axis.title.y = element_blank())

figs[["MW__PSD_x_behaviour"]] <- p1 + p2   

conditional_save(
  figs[["MW__PSD_x_behaviour"]]
  , "Interaction - Behaviour on MW"
)
