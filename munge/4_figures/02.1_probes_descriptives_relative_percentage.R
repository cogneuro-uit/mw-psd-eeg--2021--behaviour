# Visualize relative percentage of each response option for each probe    =====
## Data transformation        =====
plot2_data <- 
  data.probe.mood.sleep |>  
  mutate(
    .by = subj
    , excluded = if_else(!any(c.Adjusted_Duration.diff.pos > 1.5), T, F)
  ) |>
  mutate(
    .before = 4
    , Condition2 = case_when( 
      excluded == F & sleepdep == "SD" ~ "ex_PSD" 
      , excluded == F & sleepdep == "control" ~ "ex_NS"
      , T ~ NA )
    , Condition = if_else(sleepdep == "SD", "PSD","NS") |> fct_relevel("PSD")
    , across(c(mw,mb,smw), ~as.numeric(.x))
    # relative resp
    , mb  = if_else(mw > 2, mb, NA),
    , smw = if_else(mw > 2, smw, NA)
  ) |> 
  pivot_longer(c(mw,mb,smw), names_to="probe_type", values_to="probe_value") |>
  # exclude MB/S-MW that are not preceeded by a MW response
  filter(!is.na(probe_value))
  

plot2_data_sum <- 
  plot2_data |>
  summarise(
    .by = c(Condition, probe_type, probe_value),
    cont_count = n(),
  ) |>
  mutate(
    .by = c(Condition, probe_type)
    , cont_total_count = sum(cont_count)
  ) |> 
  mutate(
    cont_percentage = (cont_count / cont_total_count) * 100
  ) |>
  # adds the compliance DF
  left_join(
    plot2_data |>
      summarise(
        .by = c(Condition2, probe_type, probe_value),
        exc_count = n(),
      ) |> 
      mutate(
        .by = c(Condition2, probe_type)
        , exc_total_count = sum(exc_count)
      ) |> 
      mutate(
        exc_percentage = (exc_count / exc_total_count) * 100
      ) |>
      mutate(Condition = if_else(str_ends(Condition2, "PSD"), "PSD", "NS"))
    , by = c("Condition", "probe_type", "probe_value")
  ) |>
  pivot_longer(c(cont_percentage, exc_percentage), values_to="perc", names_to="dataset") |>
  mutate(
    probe_type = case_when(
      probe_type=="mw"  ~ "Mind wandering (MW)"
      , probe_type=="mb"  ~ "Mind blanking"
      , probe_type=="smw" ~ "Spontaneous MW"
    ) |> fct_relevel("Mind wandering (MW)") 
    , Condition = if_else(Condition=="PSD", "Partial sleep deprivation", "Normal Sleep") |> fct_relevel("Normal Sleep")
    , dataset = if_else(dataset=="exc_percentage", "Compliance", "Full")
    , x_interaction = interaction(dataset, probe_value)
    , col_interaction = interaction(dataset, Condition, sep = " ")
    , Condition2 = NULL
  ) 

##  Plot           =====
figs[["desc_percent_response_option_for_probes"]] <- 
  plot2_data_sum |>
  # Reverse probe_value order:
  mutate(probe_value = factor(probe_value, levels =  c(4, 3, 2, 1))) |>
  ggplot(aes(x = dataset, y = perc, fill = probe_value)) +
  facet_wrap(probe_type~Condition, nrow=3) + 
  geom_col(width = 0.9) +
  scale_fill_manual(values = gen_col("+rrb+b")) +
  labs(x = "Condition and Sample", y = "Relative Frequency (%)", fill = "Probe value") +
  theme(legend.position = "top")

conditional_save(
  figs[["desc_percent_response_option_for_probes"]]
  , "Descriptive - Relative percentage of probe responses across conditions "
)

## Visualise summary states per subject over probe values     ======
plot2_data_count <-
  plot2_data |>  
  summarise(
    .by = c(subj, Condition, probe_type, probe_value)
    , count = n()
    , excluded=unique(excluded)
  ) |>
  mutate(
    probe_type = case_when(
      probe_type=="mw"  ~ "Mind wandering (MW)"
      , probe_type=="mb"  ~ "Mind blanking"
      , probe_type=="smw" ~ "Spontaneous MW"
    ) |> fct_relevel("Mind wandering (MW)")
    , Condition = if_else(Condition=="PSD", "Partial sleep deprivation", "Normal Sleep") |> fct_relevel("Normal Sleep")
    , data_treatment = if_else(excluded, "Only Strict", "Excluded & Strict")
  )

figs[["desc_probe_count_over_subj_&_condition"]] <- 
  plot2_data_count |>
  ggplot(aes(probe_value, count)) +
  facet_wrap(probe_type~Condition, nrow=3) +
  stat_summary(aes(group=subj, colour = data_treatment), alpha =.12, position=position_jitter(.26,0)) + 
  stat_summary() +
  stat_summary(geom="line") +
  scale_colour_manual(values = gen_col("Gr")) +
  # The above is everyone.
  # The below is the REMAINING participants, AFTER exclusion
  stat_summary(data = plot2_data_count |> filter(excluded == FALSE), color = gen_col("r")
               , position = position_nudge(.2)) +
  stat_summary(data = plot2_data_count |> filter(excluded == FALSE), color = gen_col("r"), geom="line"
               , position = position_nudge(.2)) +
  theme(legend.position = "top") +
  labs(x = "Probe value", y = "Count", colour = "Sample")

conditional_save(
  figs[["desc_probe_count_over_subj_&_condition"]]
  , "Descriptives - Average probe response across probe type and condition"
)

## Combined   =====
figs[["desc_probes_additional_comparisons"]] <- 
  figs[["desc_percent_response_option_for_probes"]]  + labs(title="a)")+ 
  figs[["desc_probe_count_over_subj_&_condition"]]   + labs(title="b)")

conditional_save(
  figs[["desc_probes_additional_comparisons"]]
  , "Descriptives - Probes - Additional comparison"
  , width = 8, height = 8.5,
)
