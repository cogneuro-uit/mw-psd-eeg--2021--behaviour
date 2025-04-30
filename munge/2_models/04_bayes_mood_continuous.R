if( getOption("project_bayes_run_models") ){
  
  mod.mood.cont <- list()
  
  # DV: Positive mood        =======
  
  # Model
  mod.mood.cont[["bayes"]][["pos"]] <- brm(
    PANASsum_0 ~ c.Adjusted_Duration.diff.pos * prepost + (c.Adjusted_Duration.diff.pos * prepost | subj),
    data = test_panas |> filter(valence=="pos"),
    init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.mood.cont[["bayes"]][["pos"]], ndraws=50)
    bayes_chain_stab(mod.mood.cont[["bayes"]][["pos"]]) 
    bayes_diag(      mod.mood.cont[["bayes"]][["pos"]]) 
    # bayes_tbl_sum(mod.mood.cont[["pos"]][["bayes"]][["cont"]]) # coefficients (table)
  }
  
  # DV: Negative mood         =======
  # Model
  mod.mood.cont[["bayes"]][["neg"]] <- brm(
    PANASsum_0 ~ c.Adjusted_Duration.diff.pos * prepost + (c.Adjusted_Duration.diff.pos * prepost | subj),
    data = test_panas |> filter(valence=="neg"),
    init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.mood.cont[["bayes"]][["neg"]], ndraws=50)
    bayes_chain_stab(mod.mood.cont[["bayes"]][["neg"]])
    bayes_diag(      mod.mood.cont[["bayes"]][["neg"]])
    # bayes_tbl_sum(mod.mood.cont[["neg"]][["bayes"]][["cont"]])
  }

  conditional_save(mod.mood.cont, "PSD-on-Mood_Continuous")  
}
