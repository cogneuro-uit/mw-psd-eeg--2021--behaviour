
if( getOption("project_bayes_run_models") ){
  grp_call <- list()
  mod.cont_ag <- list()
  mod.cont_sr <- list() 
  
  # MW    ======
  grp_call$mw <- future({
    l <- list() 
    l$sr <- brm(
      mw ~ c.Self.report_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , family = cumulative("probit")
      , chains = 6
      , cores = 6
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l$ag <- brm(
      mw ~ c.Actigraphy_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , family = cumulative("probit")
      , chains = 6
      , cores = 6 
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l
  }, seed = T)
  
  resolved(grp_call$mw)
  call_resp <- value(grp_call$mw)
  mod.cont_ag$mw <- call_resp$ag
  mod.cont_sr$mw <- call_resp$sr
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    # AG
    brms::pp_check(  mod.cont_ag$mw, ndraws=50)
    bayes_chain_stab(mod.cont_ag$mw)
    bayes_diag(      mod.cont_ag$mw)
    # SR
    brms::pp_check(  mod.cont_sr$mw, ndraws=50)
    bayes_chain_stab(mod.cont_sr$mw)
    bayes_diag(      mod.cont_sr$mw)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # MB        =====
  grp_call$mb <- future({
    l <- list() 
    l$sr <- brm(
      mb ~ c.Self.report_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep |> filter(mw>2)
      , family = cumulative("probit")
      , chains = 6
      , cores = 6
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l$ag <- brm(
      mb ~ c.Actigraphy_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep |> filter(mw>2)
      , family = cumulative("probit")
      , chains = 6
      , cores = 6 
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l
  }, seed = T)
  
  resolved(grp_call$mb)
  call_resp <- value(grp_call$mb)
  mod.cont_ag$mb <- call_resp$ag
  mod.cont_sr$mb <- call_resp$sr
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.cont_ag$mb, ndraws=50)
    bayes_chain_stab(mod.cont_ag$mb)
    bayes_diag(      mod.cont_ag$mb)
    # bayes_tbl_sum(mod.cont$mb)
    brms::pp_check(  mod.cont_sr$mb, ndraws=50)
    bayes_chain_stab(mod.cont_sr$mb)
    bayes_diag(      mod.cont_sr$mb)
  }
  
  # SMW       ======
  grp_call$smw <- future({
    l <- list() 
    l$sr <- brm(
      smw ~ c.Self.report_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep |> filter(mw>2)
      , family = cumulative("probit")
      , chains = 6
      , cores = 6
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l$ag <- brm(
      smw ~ c.Actigraphy_Duration.diff.pos * (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep |> filter(mw>2)
      , family = cumulative("probit")
      , cores = 6 
      , chains = 6
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l
  }, seed = T)
  
  resolved(grp_call$smw)
  call_resp <- value(grp_call$smw)
  mod.cont_ag$smw <- call_resp$ag
  mod.cont_sr$smw <- call_resp$sr
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    # AG
    brms::pp_check(  mod.cont_ag$smw, ndraws=50)
    bayes_chain_stab(mod.cont_ag$smw)
    bayes_diag(      mod.cont_ag$smw)
    # SR
    brms::pp_check(  mod.cont_sr$smw, ndraws=50)
    bayes_chain_stab(mod.cont_sr$smw)
    bayes_diag(      mod.cont_sr$smw)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # BV      =======
  grp_call$bv <- future({
    l <- list() 
    l$sr <- brm(
      zlogbv ~ c.Self.report_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , cores = 6
      , chains = 6
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l$ag <- brm(
      zlogbv ~ c.Actigraphy_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , cores = 6 
      , chains = 6
      , iter = 6000) |>
      add_criterion(c("bayes_R2", "loo","loo_R2"))
    
    l
  }, seed = T)
  
  resolved(grp_call$bv)
  call_resp <- value(grp_call$bv)
  mod.cont_ag$bv <- call_resp$ag
  mod.cont_sr$bv <- call_resp$sr
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    # AG
    brms::pp_check(  mod.cont_ag$bv, ndraws=50)
    bayes_chain_stab(mod.cont_ag$bv)
    bayes_diag(      mod.cont_ag$bv)
    # SR
    brms::pp_check(  mod.cont_sr$bv, ndraws=50)
    bayes_chain_stab(mod.cont_sr$bv)
    bayes_diag(      mod.cont_sr$bv)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  # AE        =======
  grp_call$ae <- future({
    l <- list() 
    l$sr <- brm(
      zlogapen ~ c.Self.report_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , chains = 6
      , cores = 6
      , iter = 6000) 
    l$sr <- add_criterion(l$sr, c("bayes_R2", "loo","loo_R2"))
    
    l$ag <- brm(
      zlogapen ~ c.Actigraphy_Duration.diff.pos * (probenum_prop + pre_pos + pre_neg) + (1|subj)
      , data = data.probe.mood.sleep
      , chains = 6
      , cores = 6 
      , iter = 6000)
    l$ag <- add_criterion(l$ag, c("bayes_R2", "loo","loo_R2"))
    
    l
  }, seed = T)
  
  resolved(grp_call$ae)
  call_resp <- value(grp_call$ae)
  mod.cont_ag$ae <- call_resp$ag
  mod.cont_sr$ae <- call_resp$sr
  
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    # AG
    brms::pp_check(  mod.cont_ag$ae, ndraws=50)
    bayes_chain_stab(mod.cont_ag$ae)
    bayes_diag(      mod.cont_ag$ae)
    # SR
    brms::pp_check(  mod.cont_sr$ae, ndraws=50)
    bayes_chain_stab(mod.cont_sr$ae)
    bayes_diag(      mod.cont_sr$ae)
    # bayes_tbl_sum(mod.cont$mw) # coefficients (table)
  }
  
  
  ### Save      =======
  conditional_save(mod.cont_ag, "Bayes_models--PSD-AG_predictor")  
  conditional_save(mod.cont_sr, "Bayes_models--PSD-SR_predictor") 
} 

