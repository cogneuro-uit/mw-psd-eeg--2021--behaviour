
if( getOption("project_bayes_run_models") ){

  #' Select only data related to our sleep deprivation condition
  selected_data.probe.mood.sleep <- 
    data.probe.mood.sleep |>
    filter(Adjusted_Duration.diff < -1.5) 
  
  mod_bay_split_sleep <- list()

  # MW  
  mod_bay_split_sleep$mw <- brm(
    mw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_split_sleep$mw, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_split_sleep$mw) # chain stability
    bayes_diag(mod_bay_split_sleep$mw) # general model fit and coefficients
    bayes_coef_plot(mod_bay_split_sleep$mw) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_split_sleep$mw) # coefficients (table)
  }
  
  # MB
  mod_bay_split_sleep$mb <- brm(
    mb ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_split_sleep$mb, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_split_sleep$mb) # chain stability
    bayes_diag(mod_bay_split_sleep$mb) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_split_sleep$mb) # coefficients (table)
  }
  
  # SMW  
  mod_bay_split_sleep$smw <- brm(
    smw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_split_sleep$smw, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_split_sleep$smw) # chain stability
    bayes_diag(mod_bay_split_sleep$smw) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_split_sleep$smw) # coefficients (table)
  }
  
  # BV  
  mod_bay_split_sleep$bv <- brm(
    zlogbv ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_split_sleep$bv, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_split_sleep$bv) # chain stability
    bayes_diag(mod_bay_split_sleep$bv) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_split_sleep$bv) # coefficients (table)
  }
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_split_sleep$bv, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_split_sleep$bv) # chain stability
    bayes_diag(mod_bay_split_sleep$bv) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_split_sleep$bv) # coefficients (table)
  }
  
  # AE  
  mod_bay_split_sleep$ae <- brm(
    zlogapen ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){
    brms::pp_check(mod_bay_split_sleep$ae, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_split_sleep$ae) # chain stability
    bayes_diag(mod_bay_split_sleep$ae) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_split_sleep$ae) # coefficients (table)
  }
  
  # Criteria 
  mod_bay_split_sleep$mw  <- brms::add_criterion(mod_bay_split_sleep$mw,  c("bayes_R2", "loo")) 
  mod_bay_split_sleep$mb  <- brms::add_criterion(mod_bay_split_sleep$mb,  c("bayes_R2", "loo")) 
  mod_bay_split_sleep$smw <- brms::add_criterion(mod_bay_split_sleep$smw, c("bayes_R2", "loo")) 
  mod_bay_split_sleep$ae  <- brms::add_criterion(mod_bay_split_sleep$ae,  c("bayes_R2", "loo")) 
  mod_bay_split_sleep$bv  <- brms::add_criterion(mod_bay_split_sleep$bv,  c("bayes_R2", "loo")) 
  
  
  ### Save      =======
  if( getOption("project_bayes_save_to_file")  ){
    time <- ""
    if( getOption("project_bayes_save_with_date_time") ) time <- getOption("project_date_timne")
    save(mod_bay_split_sleep, file = paste0(
      "data/mod_bayes_exclude-1.5h", time,".Rdata")
    )
  }
} 

