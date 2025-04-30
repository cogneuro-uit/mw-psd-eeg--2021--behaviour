
if( getOption("project_bayes_run_models") ){
  
  mod.cont <- list()
  
  # MW    ======
  mod.cont$mw <- brm(
    mw ~ c.Adjusted_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep, 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.cont$mw, ndraws=50)
    bayes_chain_stab(mod.cont$mw)
    bayes_diag(      mod.cont$mw)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # MB        =====
  mod.cont$mb <- brm(
    mb ~ c.Adjusted_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.cont$mb, ndraws=50)
    bayes_chain_stab(mod.cont$mb)
    bayes_diag(      mod.cont$mb)
    # bayes_tbl_sum(mod.cont$mb)
  }
  
  # SMW       ======
  mod.cont$smw <- brm(
    smw ~ c.Adjusted_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep |> filter(mw > 2), 
    family = cumulative("probit"), init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.cont$smw, ndraws=50)
    bayes_chain_stab(mod.cont$smw)
    bayes_diag(      mod.cont$smw)
    # bayes_tbl_sum(mod.cont$smw)
  }
  
  # BV      =======
  mod.cont$bv <- brm(
    zlogbv ~ c.Adjusted_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2","loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.cont$bv, ndraws=50)
    bayes_chain_stab(mod.cont$bv)
    bayes_diag(      mod.cont$bv)
    # bayes_tbl_sum(mod.cont$bv)
  }
  
  # AE        =======
  mod.cont$ae <- brm(
    zlogapen ~ c.Adjusted_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo","loo_R2"))
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.cont$ae, ndraws=50)
    bayes_chain_stab(mod.cont$ae)
    bayes_diag(      mod.cont$ae)
    # bayes_tbl_sum(mod.cont$ae)
  }
  

  ### Save      =======
  conditional_save(mod.cont, "Mod-Bayes-Continuous")  
} 

