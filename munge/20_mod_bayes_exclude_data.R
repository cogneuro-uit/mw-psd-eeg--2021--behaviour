
if( project[["bayes"]][["run_models"]] ){

  #' Select only data related to our sleep deprivation condition
  selected_data.probe.mood.sleep <- 
    data.probe.mood.sleep |>
    filter(Adjusted_Duration.diff < -1.5) 
  
  mod_mood_full <- list()

  # MW  
  mod_mood_full$mw <- brm(
    mw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$mw)
    # Summary:
    bayes_tbl_sum(mod_mood_full$mw) |> gt()
  }
  
  # MB
  mod_mood_full$mb <- brm(
    mb ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$mb)
    # Summary:
    bayes_tbl_sum(mod_mood_full$mb)
  }
  
  # SMW  
  mod_mood_full$smw <- brm(
    smw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$smw)
    # Summary:
    bayes_tbl_sum(mod_mood_full$smw)
  }
  
  # BV  
  mod_mood_full$bv <- brm(
    zlogbv ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$bv)
    # Summary:
    bayes_tbl_sum(mod_mood_full$bv)
  }
  
  # AE  
  mod_mood_full$ae <- brm(
    zlogapen ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$ae)
    # Summary:
    bayes_tbl_sum(mod_mood_full$ae) |> gt()
  }
  
  # Criteria 
  mod_mood_full$mw  <- brms::add_criterion(mod_mood_full$mw,  c("bayes_R2", "loo")) 
  mod_mood_full$mb  <- brms::add_criterion(mod_mood_full$mb,  c("bayes_R2", "loo")) 
  mod_mood_full$smw <- brms::add_criterion(mod_mood_full$smw, c("bayes_R2", "loo")) 
  mod_mood_full$ae  <- brms::add_criterion(mod_mood_full$ae,  c("bayes_R2", "loo")) 
  mod_mood_full$bv  <- brms::add_criterion(mod_mood_full$bv,  c("bayes_R2", "loo")) 
  
  
  ### Save      =======
  if( project[["bayes"]][["save"]][["to_file"]] ){
    save(mod_mood_full, file = paste0(
      "data/mod_bayes_adjusted_sleep_exclude-1.5h", 
      project[["bayes"]][["save"]][["date_time"]],".Rdata")
    )
  }
} 

