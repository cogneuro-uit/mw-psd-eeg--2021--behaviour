
if( getOption("project_bayes_run_models") ){
  f_call <- list()
  mod.all_pars <- list()
  
  # MW    ======
  f_call$mw <- future({
    d <- brm(
      mw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , family = cumulative("probit")
      , chains = 6
      , cores = 6 
      , iter = 6000)
    d <- add_criterion(d, c("bayes_R2", "loo","loo_R2"))
    d
  }, seed = T)
  
  resolved(f_call$mw)
  call_resp <- value(f_call$mw)
  mod.all_pars$mw <- call_resp
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    # SR
    brms::pp_check(  mod.all_pars$mw, ndraws=50)
    bayes_chain_stab(mod.all_pars$mw)
    bayes_diag(      mod.all_pars$mw)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # MB        =====
  f_call$mb <- future({
    d <- brm(
      mb ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep |> filter(mw>2)
      , family = cumulative("probit")
      , chains = 6
      , cores = 6 
      , iter = 6000) 
    d <- add_criterion(d, c("bayes_R2", "loo","loo_R2"))
    d    
  }, seed = T)
  
  resolved(f_call$mb)
  call_resp <- value(f_call$mb)
  mod.all_pars$mb <- call_resp
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    # bayes_tbl_sum(mod.cont$mb)
    brms::pp_check(  mod.all_pars$mb, ndraws=50)
    bayes_chain_stab(mod.all_pars$mb)
    bayes_diag(      mod.all_pars$mb)
  }
  
  # SMW       ======
  f_call$smw <- future({
    d <- brm(
      smw ~ sleepdep * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep |> filter(mw>2)
      , family = cumulative("probit")
      , chains = 6
      , cores = 6
      , iter = 6000)
    d <- add_criterion(d, c("bayes_R2", "loo","loo_R2"))
    d
  }, seed = T)
  
  resolved(f_call$smw)
  call_resp <- value(f_call$smw)
  mod.all_pars$smw <- call_resp
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.all_pars$smw, ndraws=50)
    bayes_chain_stab(mod.all_pars$smw)
    bayes_diag(      mod.all_pars$smw)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # BV      =======
  f_call$bv <- future({
    d <- brm(
      zlogbv ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , cores = 6 
      , chains = 6
      , iter = 6000)
    d <- add_criterion(d, c("bayes_R2", "loo","loo_R2"))
    d
  }, seed = T)
  
  resolved(f_call$bv)
  call_resp <- value(f_call$bv)
  mod.all_pars$bv <- call_resp
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.all_pars$bv, ndraws=50)
    bayes_chain_stab(mod.all_pars$bv)
    bayes_diag(      mod.all_pars$bv)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # AE        =======
  f_call$ae <- future({
    d <- brm(
      zlogapen ~ sleepdep * (probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , chains = 6
      , cores = 6 
      , iter = 6000)
    d <- add_criterion(d, c("bayes_R2", "loo","loo_R2"))
    d
  }, seed = T)
  
  resolved(f_call$ae)
  call_resp <- value(f_call$ae)
  mod.all_pars$ae <- call_resp
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.all_pars$ae, ndraws=50)
    bayes_chain_stab(mod.all_pars$ae)
    bayes_diag(      mod.all_pars$ae)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  
  ### Save      =======
  conditional_save(mod.all_pars, "Bayes_models--dich_include_ALL_participants") 
} 

