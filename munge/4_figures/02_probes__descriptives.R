#  Percentage plot      ======
plot_data <- 
  data.probe.mood.sleep |>  
  mutate(
    Condition = if_else(sleepdep == "SD", "PSD","NS") |> fct_relevel("PSD"),
    highlight = if_else(Adjusted_Duration.diff < -1.5, T, F),
    Condition2 = case_when( 
      highlight == T & sleepdep == "SD" ~ "ex_PSD", 
      highlight == T & sleepdep == "control" ~ "ex_NS",
      T ~ NA,)
  ) |>
  summarise(
    .by = c(subj, Condition),
    MW  = 100 * mean(mw > 2),
    MB  = 100 * mean(mb[mw > 2] > 2, na.rm=T),
    SMW = 100 * mean(smw[mw > 2] > 2, na.rm=T),
    sleep = unique(Adjusted_Duration.diff),
    highlight = unique(highlight),
    Condition2 = unique(Condition2),
  ) |>
  pivot_longer(c(MW,MB,SMW), names_to = "name", values_to = "value") |>
  mutate(
    name = ordered(name, levels = c("MW", "MB", "SMW")),
    name = fct_recode(name, `% MW` = "MW", `% MB` = "MB", `% Spontaneous` = "SMW")
  )

figs[["probes__descriptives_percentage"]] <- 
  plot_data |>
  ggplot(aes(x = Condition |> fct_relevel("NS"), y = value, col = Condition, shape = Condition, alpha = highlight, size = highlight)) +
  facet_wrap(~name) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide="none") + 
  scale_size_manual(values = c("TRUE" = .3, "FALSE" = .18), guide="none") +
  # all lines
  stat_summary(aes(group = subj), col = "darkgrey", geom ="line", # line
               position = position_jitter(.2, seed=389)) +
  # all data points
  stat_summary(aes(group = subj),  
               position = position_jitter(.2, seed=389)) +
  # Cross
  # GRAND mean point - CONTINOUS
  stat_summary(aes(
    group = name), fun.data = mean_se, 
    geom = "pointrange", position = position_nudge(.35),
    color = "black", alpha = .65, size = .4 ,shape = 4, 
  ) +
  # GRAND mean line - CONTINOUS
  stat_summary(aes(
    group = name, shape = highlight), fun = mean, 
    geom = "line", position = position_nudge(.35),
    color = "black", alpha = .65, linewidth = .5,
  ) +
  # --- EXCLUDED DATA ---
  # Square shape
  # GRAND mean point
  stat_summary(aes(
    group = name, size = NULL, alpha = NULL, shape=NULL), plot_data |> filter(highlight), 
    fun.data = mean_se, geom = "pointrange", position = position_nudge(-.27), 
    color = "black", alpha = .65, size = .4, shape = 0,
  ) +
  # GRAND mean line
  stat_summary(aes(
    group = name, size = NULL, alpha = NULL, shape=NULL), plot_data |> filter(highlight), 
    fun = mean, geom = "line", position = position_nudge(-.27),
    color = "black", alpha = .65, linewidth = .5,
  ) +
  labs(y = "Percentage", x = "Condition", title="a)") +
  theme(legend.position = "none")

# COUNT PLOT          ======
plot2_data <- 
  data.probe.mood.sleep |>  
  mutate(
    .before = 4,
    Condition = if_else(sleepdep == "SD", "PSD","NS") |> fct_relevel("PSD"),
    excluded = if_else(Adjusted_Duration.diff.pos < 1.5, T, F),
    Condition2 = case_when( 
      excluded == F & sleepdep == "SD" ~ "ex_PSD", 
      excluded == F & sleepdep == "control" ~ "ex_NS",
      T ~ NA ),
    across(c(mw,mb,smw), ~as.numeric(.x))
    # # relative resp
    # mb = if_else(mw>2, mb, NA),
    # smw = if_else(mw>2, smw, NA)
  ) |>
  pivot_longer(c(mw,mb,smw), names_to="probe_type", values_to="probe_value") 

plot2_data_sum <- 
  plot2_data |>
  summarise(
    .by = c(Condition, probe_type, probe_value),
    cont_n = n(),
  ) |>
  left_join(
    plot2_data |>
      summarise(
        .by = c(Condition2, probe_type, probe_value),
        exc_n = n(),
      ) |> 
      mutate(Condition = if_else(str_ends(Condition2, "PSD"), "PSD", "NS"))
    , by = c("Condition", "probe_type", "probe_value")
  ) |> 
  pivot_longer(c(cont_n, exc_n), values_to="count", names_to="dataset") |>
  mutate(
    probe_type = case_when(
      probe_type=="mw"  ~ "Mind wandering",
      probe_type=="mb"  ~ "Mind blanking",
      probe_type=="smw" ~ "Spontaneous mind wandering",
    ) |> fct_relevel("Mind wandering"), 
    dataset = if_else(dataset=="exc_n", "Dichotomous", "Continuous"),
    x_interaction = interaction(dataset, probe_value),
    col_interaction = interaction(dataset, Condition, sep = " "),
    Condition2 = NULL
  ) 

# Diff geom
figs[["probes__descriptives_count"]] <- 
  plot2_data_sum |> 
  filter(dataset=="Continuous") |>
  ggplot(aes(x = probe_value, y = count, fill = col_interaction, group = Condition)) + 
  facet_wrap(~probe_type) +
  geom_bar(stat = "identity", color="black", position = position_dodge(), linewidth=0.2) +
  geom_bar(data = plot2_data_sum |> filter(dataset=="Dichotomous"), 
           stat = "identity", color="black", position = position_dodge(.9), linewidth=0.2,
           width = .45) +
  scale_fill_manual(values = gen_col("/bb/rr") ) + 
  labs(x = "Thought probe response", y = "Count", title = "b)", fill = "Condition") +
  theme(legend.position = "top", legend.direction = "horizontal")


# COMBINE       ======
figs[["probes__COMBINED_descriptives"]] <- 
  figs[["probes__descriptives_percentage"]] +
  figs[["probes__descriptives_count"]] +
  plot_layout(nrow=2,heights = 8, widths = 7)


## SAVE    =====
conditional_save(
  figs[["probes__COMBINED_descriptives"]]
  , "Descriptive statistics of the thought probes"
  , width = 7, height = 8,
)


# CLEANUP ======
rm(plot2_data)
rm(plot2_data_sum)