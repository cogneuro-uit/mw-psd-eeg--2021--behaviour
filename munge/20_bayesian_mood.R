
if( project[["bayes"]][["run_models"]] ){

  mod_mood_full <- list()

  # MW  
  mod_mood_full$mw <- brm(
    mw ~ sleepdep * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood, 
    family=cumulative("probit"), cores = 6, backend = "cmdstanr",
    init = 0, chains = 6, iter = 6000)
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$mw)
    # Summary:
    bayes_tbl_sum(mod_mood_full$mw) |> gt()
  }
  
  # MB
  mod_mood_full$mb <- brm(
    mb ~ sleepdep * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood |> filter(mw > 2), family=cumulative("probit"), 
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$mb)
    # Summary:
    bayes_tbl_sum(mod_mood_full$mb)
  }
  
  # SMW  
  mod_mood_full$smw <- brm(
    smw ~ sleepdep * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood |> filter(mw > 2), family=cumulative("probit"),
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$smw)
    # Summary:
    bayes_tbl_sum(mod_mood_full$smw)
  }
  
  # BV  
  mod_mood_full$bv <- brm(
    zlogbv ~ sleepdep * (probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood, 
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_mood_full$bv)
    # Summary:
    bayes_tbl_sum(mod_mood_full$bv)
  }
  
  # AE  
  mod_mood_full$ae <- brm(
    zlogapen ~ sleepdep * (probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood,
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
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
      "data/bayes_full_model_SR_exclude", 
      project[["bayes"]][["save"]][["date_time"]],".Rdata")
    )
  }
} 

