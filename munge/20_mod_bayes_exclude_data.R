
if( getOption("project_bayes_run_models") ){

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
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_mood_full$mw, ndraws=50) # Predictive check
    bayes_chain_stab(mod_mood_full$mw) # chain stability
    bayes_diag(mod_mood_full$mw) # general model fit and coefficients
    bayes_coef_plot(mod_mood_full$mw) # general model fit and coefficients
    # bayes_tbl_sum(mod_mood_full$mw) # coefficients (table)
  }
  
  # MB
  mod_mood_full$mb <- brm(
    mb ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_mood_full$mb, ndraws=50) # Predictive check
    bayes_chain_stab(mod_mood_full$mb) # chain stability
    bayes_diag(mod_mood_full$mb) # general model fit and coefficients
    # bayes_tbl_sum(mod_mood_full$mb) # coefficients (table)
  }
  
  # SMW  
  mod_mood_full$smw <- brm(
    smw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_mood_full$smw, ndraws=50) # Predictive check
    bayes_chain_stab(mod_mood_full$smw) # chain stability
    bayes_diag(mod_mood_full$smw) # general model fit and coefficients
    # bayes_tbl_sum(mod_mood_full$smw) # coefficients (table)
  }
  
  # BV  
  mod_mood_full$bv <- brm(
    zlogbv ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_mood_full$bv, ndraws=50) # Predictive check
    bayes_chain_stab(mod_mood_full$bv) # chain stability
    bayes_diag(mod_mood_full$bv) # general model fit and coefficients
    # bayes_tbl_sum(mod_mood_full$bv) # coefficients (table)
  }
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_mood_full$bv, ndraws=50) # Predictive check
    bayes_chain_stab(mod_mood_full$bv) # chain stability
    bayes_diag(mod_mood_full$bv) # general model fit and coefficients
    # bayes_tbl_sum(mod_mood_full$bv) # coefficients (table)
  }
  
  # AE  
  mod_mood_full$ae <- brm(
    zlogapen ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(mod_mood_full$ae, ndraws=50) # Predictive check
    bayes_chain_stab(mod_mood_full$ae) # chain stability
    bayes_diag(mod_mood_full$ae) # general model fit and coefficients
    # bayes_tbl_sum(mod_mood_full$ae) # coefficients (table)
  }
  
  # Criteria 
  mod_mood_full$mw  <- brms::add_criterion(mod_mood_full$mw,  c("bayes_R2", "loo")) 
  mod_mood_full$mb  <- brms::add_criterion(mod_mood_full$mb,  c("bayes_R2", "loo")) 
  mod_mood_full$smw <- brms::add_criterion(mod_mood_full$smw, c("bayes_R2", "loo")) 
  mod_mood_full$ae  <- brms::add_criterion(mod_mood_full$ae,  c("bayes_R2", "loo")) 
  mod_mood_full$bv  <- brms::add_criterion(mod_mood_full$bv,  c("bayes_R2", "loo")) 
  
  
  ### Save      =======
  if( getOption("project_bayes_save_to_file")  ){
    time <- ""
    if( getOption("project_bayes_save_with_date_time") ) time <- getOption("project_date_timne")
    save(mod_mood_full, file = paste0(
      "data/mod_bayes_exclude-1.5h", time,".Rdata")
    )
  }
} 

