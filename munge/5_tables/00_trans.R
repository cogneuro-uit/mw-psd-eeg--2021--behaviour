# TABLE       =====
## Sleep      =====
sleep_time_session_condition <-
  sleeptimes_updated |>
  pivot_longer(c(
    wake_SR, onset_SR, duration_SR,
    wake_AG, onset_AG, duration_AG,
    wake_final, onset_final, duration_final,
    sleep_quality,
  )) |>
  mutate(
    modality = case_when(
      str_ends(name, "_AG") ~ "Actigraphy", 
      str_ends(name, "_SR") ~ "Self-report",
      str_ends(name, "_final") ~ "Adjusted"),
    name = case_when(
      str_starts(name, "wake_") ~ "Wake time",
      str_starts(name, "onset_") ~ "Sleep onset",
      str_starts(name, "duration_") ~ "Sleep duration",
      name=="sleep_quality" ~ ""),
    sleep_cond = case_when(
      pre_control  == 1 ~ "ns", 
      pre_sleepdep == 1 ~ "sd",
      T ~ NA ) ) |>
  filter(!is.na(sleep_cond)) |>
  summarise(
    .by  = c(subj, sleep_cond, modality, name),
    mean = mean(value, na.rm = T),
    sd   = sd(value, na.rm = T),
    min  = min(value, na.rm = T),
    max  = max(value, na.rm = T)
  ) |>
  pivot_wider(names_from = sleep_cond, values_from = c(mean, sd, min, max)) |>
  summarise(
    .by     = c(modality, name),
    psd_m   = mean(mean_sd, na.rm=T),
    psd_sd  = sd(  mean_sd, na.rm=T),
    ns_m    = mean(mean_ns, na.rm=T),
    ns_sd   = sd(  mean_ns, na.rm=T),
    diff_m  = psd_m - ns_m,
    diff_sd = sd(mean_sd - mean_ns, na.rm=T),
    bf      = extractBF( ttestBF(
      mean_sd[!is.na(mean_sd) & !is.na(mean_ns)], 
      mean_ns[!is.na(mean_sd) & !is.na(mean_ns)], paired=T) )$bf,
    psd_m   = if_else(psd_m > 24, psd_m - 24, psd_m),
    ns_m    = if_else(ns_m  > 24, ns_m  - 24, ns_m),
  ) |> 
  pivot_longer(c(ends_with("_m"), ends_with("_sd")), 
               names_sep = "_", names_to = c("cond", "value2")) |>
  pivot_wider(names_from = c(cond, value2)) |>
  mutate(
    modality = if_else(is.na(modality), "Sleep quality", modality)
    , across(c(ends_with("_sd"), diff_m)
             , ~if_else(modality != "Sleep quality"
                        , clock_h_m(.x)
                        , fmt_APA_numbers(.x, .chr = T) ) )
    , across(c(psd_m, ns_m)
             , ~if_else(modality != "Sleep quality"
                        , clock_24(.x)
                        , fmt_APA_numbers(.x, .chr = T) ) )
    , bf = if_else(bf>1000, format(bf, scientific=T, digits=2)
                   , fmt_APA_numbers(bf, .chr=T))
  ) |>
  select(c(everything(), -bf), bf) 

## Bayesian thought probes transformations   =====
# Function
mutate_bayes_mod_probe <- function(data){
  data |>
    mutate(
      var = fct_recode(
        var, 
        Threshold1 = "Intercept[1]", 
        Threshold2 = "Intercept[2]", 
        Threshold3 = "Intercept[3]", 
        Trial = "probenum_prop", 
        Trial = "probenum",
        BV = "zlogbv",
        AE = "zlogapen", 
        `Pre-positive` = "pre_pos", 
        `Pre-negative` = "pre_neg", 
        
        # Continuous
        `PSD` = "c.Adjusted_Duration.diff.pos", 
        `PSD x Trial` = "c.Adjusted_Duration.diff.pos:probenum",
        `PSD x Trial` = "c.Adjusted_Duration.diff.pos:probenum_prop",
        `PSD x AE`    = "c.Adjusted_Duration.diff.pos:zlogapen",
        `PSD x BV`    = "c.Adjusted_Duration.diff.pos:zlogbv",
        `PSD x Pre-positive` = "c.Adjusted_Duration.diff.pos:pre_pos", 
        `PSD x Pre-negative` = "c.Adjusted_Duration.diff.pos:pre_neg",
        # Excluded
        #' This part is used below
        `PSD` = "sleepdepSD", 
        `PSD x Trial` = "sleepdepSD:probenum",
        `PSD x Trial` = "sleepdepSD:probenum_prop",
        `PSD x AE`    = "sleepdepSD:zlogapen",
        `PSD x BV`    = "sleepdepSD:zlogbv",
        `PSD x Pre-positive` = "sleepdepSD:pre_pos", 
        `PSD x Pre-negative` = "sleepdepSD:pre_neg"),
      var = ordered(var, levels = c(
        "Threshold1", "Threshold2", "Threshold3",
        "Trial", "BV", "AE", "Pre-positive", "Pre-negative", 
        "PSD", "PSD x Trial", "PSD x BV", "PSD x AE",
        "PSD x Pre-positive", "PSD x Pre-negative",
        "Sigma (subj)", # "Sigma (subjects)", 
        "LOOIC",
        "R2"
      ))
    )  |>
    arrange(var)
}

probe_tbl_cont <- 
  bayes_tbl_sum(mod_bay_sleep_cont$mw, add_sigma = T, fmt_md = T,
                add_loo = T, add_R2 = T, apa_table = T)  |>
  bayes_tbl_add_sig() |>
  mutate_bayes_mod_probe() |> 
  rename_with(~paste0("cont_mw_",.x), 3:6) |>
  left_join(
    bayes_tbl_sum(mod_bay_sleep_cont$mb, add_sigma = T, fmt_md = T,
                  add_loo = T, add_R2 = T, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("cont_mb_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_sleep_cont$smw, add_sigma = T, fmt_md = T,
                  add_loo = T, add_R2 = T, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("cont_smw_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$mw, add_sigma = T, fmt_md = T,
                  add_loo = T, add_R2 = T, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("exc_mw_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$mb, add_sigma = T, fmt_md = T,
                  add_loo = T, add_R2 = T, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("exc_mb_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  left_join(
    bayes_tbl_sum(mod_bay_split_sleep$smw, add_sigma = T, fmt_md = T,
                  add_loo = T, add_R2 = T, apa_table = T)  |>
      bayes_tbl_add_sig() |>
      mutate_bayes_mod_probe() |> 
      rename_with(~paste0("exc_smw_",.x), 3:6),
    by = c("group", "var") 
  ) |>
  mutate(
    c_mw_p    = if_else(as.numeric(cont_mw_p) >= .95, TRUE, FALSE),
    c_mb_p    = if_else(as.numeric(cont_mb_p) >= .95, TRUE, FALSE),
    c_smw_p   = if_else(as.numeric(cont_smw_p) >= .95, TRUE, FALSE),
    e_mw_p    = if_else(as.numeric(exc_mw_p) >= .95, TRUE, FALSE),
    e_mb_p    = if_else(as.numeric(exc_mb_p) >= .95, TRUE, FALSE),
    e_smw_p    = if_else(as.numeric(exc_smw_p) >= .95, TRUE, FALSE),
    diff_mw   = if_else(c_mw_p  != e_mw_p, TRUE, FALSE),
    diff_mb   = if_else(c_mb_p  != e_mb_p, TRUE, FALSE),
    diff_smw  = if_else(c_smw_p != e_smw_p, TRUE, FALSE),
    across(c(starts_with("c_"), starts_with("e_")), ~NULL),
    mw_e="", mb_e="",smw_e=""
  )
## Bayesian BEHAVIOUR   ======
mutate_bayes_mod_beh <- function(data){
  data |>
    mutate(var = fct_recode(
      var, 
      Intercept            = "Intercept", 
      Trial                = "probenum_prop",
      Trial                = "probenum",
      `Pre-positive`       = "pre_pos", 
      `Pre-negative`       = "pre_neg", 
      # continuous
      PSD                  = "c.Adjusted_Duration.diff.pos",
      `PSD x Trial`        = "c.Adjusted_Duration.diff.pos:probenum_prop",
      `PSD x Trial`        = "c.Adjusted_Duration.diff.pos:probenum",
      `PSD x Pre-positive` = "c.Adjusted_Duration.diff.pos:pre_pos",
      `PSD x Pre-negative` = "c.Adjusted_Duration.diff.pos:pre_neg",
      # Dichotomous
      PSD                  = "sleepdepSD",
      `PSD x Trial`        = "sleepdepSD:probenum_prop",
      `PSD x Trial`        = "sleepdepSD:probenum",
      `PSD x Pre-positive` = "sleepdepSD:pre_pos",
      `PSD x Pre-negative` = "sleepdepSD:pre_neg"),
      var = ordered(var, levels = c(
        "Intercept", "Trial", "Pre-positive", "Pre-negative", "PSD", 
        "PSD x Trial", "PSD x Pre-positive", "PSD x Pre-negative", 
        "Sigma (subj)", "LOOIC", "R2"
      ) )
    ) |>
    arrange(var) 
}
