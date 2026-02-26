# model
#     Data    =====
plot_data <-
  expand_grid(
    sleep_deviation = c(-1,0,1),
    mood_deviation  = c(-1,0,1),
  ) |>
  mutate(
    sleep = summarised_vals$sleep_m + (summarised_vals$sleep_sd * sleep_deviation),
    mood  = summarised_vals$mood_pos_m + (summarised_vals$mood_pos_sd * mood_deviation),
    
    # MW ~ PSD x prePOS
    , mw_ns = c$mw_i
    + mean(c$mw[["b_pre_pos"]]) * mood
    , mw_psd = mw_ns
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$mw[["b_c.Adjusted_Duration.diff.pos:pre_pos"]]) * sleep * mood
    
    # SMW ~ PSD x prePOS
    , smw_ns = c$smw_i
    + mean(c$smw[["b_pre_pos"]]) * mood
    , smw_psd = smw_ns
    + mean(c$smw[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$smw[["b_c.Adjusted_Duration.diff.pos:pre_pos"]]) * sleep * mood
    
    # BV ~ PSD x prePOS
    , bv_ns = mean(c$bv[["b_Intercept"]])
    + mean(c$bv[["b_pre_pos"]]) * mood
    , bv_psd = bv_ns
    + mean(c$bv[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$bv[["b_c.Adjusted_Duration.diff.pos:pre_pos"]]) * sleep * mood
    
    # AE ~ PSD x prePOS
    , ae_ns = mean(c$ae[["b_Intercept"]])
    + mean(c$ae[["b_pre_pos"]]) * mood
    , ae_psd = ae_ns
    + mean(c$ae[["b_c.Adjusted_Duration.diff.pos"]]) * sleep 
    + mean(c$ae[["b_c.Adjusted_Duration.diff.pos:pre_pos"]]) * sleep * mood
    
  ) |>
  pivot_longer(c(ends_with("psd"), ends_with("ns")), names_to="names") |>
  separate_wider_delim(names, "_", names_sep = "_", names = c("out", "cond")) |>
  mutate(cond = case_when(
    names_cond=="ns" ~ "NS"
    , sleep_deviation==-1 ~ "PSD -1 SD"
    , sleep_deviation==0 ~ "PSD Mean"
    , sleep_deviation==+1 ~ "PSD +1 SD"
  ) |> factor(levels = c("PSD +1 SD", "PSD Mean", "PSD -1 SD", "NS"))
  , out = case_when(
    names_out=="smw" ~ "Spontaneous mind wandering"
    , names_out=="bv" ~ "Behavioural variability"
    , names_out=="ae" ~ "Approximate Entropy"
    , names_out == "mw" ~ "Mind wandering"
  )) 


# Plot      ======
## BV       ======
figs[["BV~PSD*pre_pos"]] <- 
  plot_data |> 
  filter(out == "Behavioural variability") |>
  default_time_plot("mood_deviation") +
  z_score(ylabel = "Z-score BV", xlabel = "Z-score pre-positive mood", axis = "xy")

conditional_save(
  figs[["BV~PSD*pre_pos"]]
  , "BV - over PSD * pre_pos"
  , width = 3, height = 3
)

## AE       ======
figs[["AE~PSD*pre_pos"]] <- 
  plot_data |> 
  filter(out == "Approximate Entropy") |>
  default_time_plot("mood_deviation") +
  z_score(ylabel = "Z-score AE", xlabel = "Z-score pre-positive mood", axis = "xy")
  
conditional_save(
  figs[["AE~PSD*pre_pos"]]
  , "AE - over PSD x pre_pos"
  , width = 3, height = 3
)


## SMW      ======
figs[["SMW~PSD*pre_pos"]] <- 
  plot_data |> 
  filter(out == "Spontaneous mind wandering") |>
  default_time_plot("mood_deviation") +
  z_score(xlabel = "Z-score pre-positive mood", axis = "x") + 
  probe("Association with S-MW")

conditional_save(
  figs[["SMW~PSD*pre_pos"]]
  , "SMW - over PSD x pre_pos"
  , width = 3, height = 3
)


##  MW           =======
figs[["MW~PSD*pre_pos"]] <- 
  plot_data |> 
  filter(out == "Mind wandering") |>
  default_time_plot("mood_deviation") +
  z_score(xlabel = "Z-score pre-positive mood", axis = "x") + 
  probe("Association with MW")

conditional_save(
  figs[["MW~PSD*pre_pos"]]
  , "SMW - over PSD x pre_pos"
  , width = 3, height = 3
)

