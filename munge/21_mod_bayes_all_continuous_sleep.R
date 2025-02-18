
if( getOption("project_bayes_run_models") ){
  
  mod_bay_sleep_cont <- list()
  
  # MW
  mod_bay_sleep_cont$mw <- brm(
    mw ~ c.Adjusted_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep, 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_sleep_cont$mw, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_sleep_cont$mw) # chain stability
    bayes_diag(mod_bay_sleep_cont$mw) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_sleep_cont$mw) # coefficients (table)
  }
  
  # MB
  mod_bay_sleep_cont$mb <- brm(
    mb ~ c.Adjusted_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_sleep_cont$mb, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_sleep_cont$mb) # chain stability
    bayes_diag(mod_bay_sleep_cont$mb) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_sleep_cont$mb) # coefficients (table)
  }
  
  # SMW
  mod_bay_sleep_cont$smw <- brm(
    smw ~ c.Adjusted_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_sleep_cont$smw, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_sleep_cont$smw) # chain stability
    bayes_diag(mod_bay_sleep_cont$smw) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_sleep_cont$smw) # coefficients (table)
  }
  
  # BV
  mod_bay_sleep_cont$bv <- brm(
    zlogbv ~ c.Adjusted_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_sleep_cont$bv, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_sleep_cont$bv) # chain stability
    bayes_diag(mod_bay_sleep_cont$bv) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_sleep_cont$bv) # coefficients (table)
  }
  
  # AE
  mod_bay_sleep_cont$ae <- brm(
    zlogapen ~ c.Adjusted_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000)
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mod_bay_sleep_cont$ae, ndraws=50) # Predictive check
    bayes_chain_stab(mod_bay_sleep_cont$ae) # chain stability
    bayes_diag(mod_bay_sleep_cont$ae) # general model fit and coefficients
    # bayes_tbl_sum(mod_bay_sleep_cont$ae) # coefficients (table)
  }
  
  # Criteria 
  mod_bay_sleep_cont[["mw"]] <- brms::add_criterion(mod_bay_sleep_cont[["mw"]],  c("bayes_R2", "loo")) 
  mod_bay_sleep_cont[["mb"]] <- brms::add_criterion(mod_bay_sleep_cont[["mb"]],  c("bayes_R2", "loo")) 
  mod_bay_sleep_cont[["smw"]] <- brms::add_criterion(mod_bay_sleep_cont[["smw"]], c("bayes_R2", "loo")) 
  mod_bay_sleep_cont[["ae"]] <- brms::add_criterion(mod_bay_sleep_cont[["ae"]],  c("bayes_R2", "loo")) 
  mod_bay_sleep_cont[["bv"]] <- brms::add_criterion(mod_bay_sleep_cont[["bv"]],  c("bayes_R2", "loo")) 
  
  
  ### Save      =======
  if( getOption("project_bayes_save_to_file")  ){
    time <- ""
    if( getOption("project_bayes_save_with_date_time") ) time <- getOption("project_date_timne")
    save(mod_bay_sleep_cont, file = paste0(
      "data/mod_bayes_no_exclud_continous_SD", time, ".rdata")
    )
  }
} 

