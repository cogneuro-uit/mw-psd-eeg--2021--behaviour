
if( getOption("project_bayes_run_models") ){

  #' Select only data related to our sleep deprivation condition
  selected_data.probe.mood.sleep <- 
    data.probe.mood.sleep |>
    filter(Adjusted_Duration.diff < -1.5) 
  
  mod.dich <- list()

  # MW          ======
  mod.dich$mw <- brm(
    mw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.dich$mw, ndraws=50)
    bayes_chain_stab(mod.dich$mw)
    bayes_diag(      mod.dich$mw)
    # bayes_tbl_sum(mod.dich$mw)
  }
  
  # MB        ======
  mod.dich$mb <- brm(
    mb ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.dich$mb, ndraws=50)
    bayes_chain_stab(mod.dich$mb)
    bayes_diag(      mod.dich$mb)
    # bayes_tbl_sum(mod.dich$mb)
  }
  
  # SMW   =======
  mod.dich$smw <- brm(
    smw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.dich$smw, ndraws=50)
    bayes_chain_stab(mod.dich$smw)
    bayes_diag(      mod.dich$smw)
    # bayes_tbl_sum(mod.dich$smw)
  }
  
  # BV  =======
  mod.dich$bv <- brm(
    zlogbv ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.dich$bv, ndraws=50)
    bayes_chain_stab(mod.dich$bv)
    bayes_diag(      mod.dich$bv)
    # bayes_tbl_sum(mod.dich$bv)
  }
  
  # AE  =======
  mod.dich$ae <- brm(
    zlogapen ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = selected_data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.dich$ae, ndraws=50)
    bayes_chain_stab(mod.dich$ae)
    bayes_diag(      mod.dich$ae) 
    # bayes_tbl_sum(mod.dich$ae)
  }

  
  # Save      =======
  conditional_save(mod.dich, "Mod-Bayes-Dichotomous")  
} 

