
if( getOption("project_bayes_run_models") ){
  
  # List
  mod.mood.dich <- list()
  
  #     Frequentist        ======
  ## DV: Negative       ======
  mod.mood.dich[["freq"]][["neg"]] <- afex::aov_4(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj), 
    test_panas_dict |> filter(valence=="neg"))
  mod.mood.dich[["freq"]][["neg"]] |> summary()
  
  ## DV: Positive       ======
  mod.mood.dich[["freq"]][["pos"]] <- afex::aov_4(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj), 
    test_panas_dict |> filter(valence=="pos"))
  mod.mood.dich[["freq"]][["pos"]] |> summary()
  
  
  
  #     Bayesian         ======
  
  ## DV: Positive mood       ====== 
  
  # Model
  mod.mood.dich[["bayes"]][["pos"]] <- brm(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj),
    data = test_panas_dict |> filter(valence=="pos"),
    iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.mood.dich[["bayes"]][["pos"]], ndraws=50)
    bayes_chain_stab(mod.mood.dich[["bayes"]][["pos"]]) 
    bayes_diag(      mod.mood.dich[["bayes"]][["pos"]]) 
      #' Generally does not succesfully converge, which leads to weak ESS
    # bayes_tbl_sum(mod.mood.dich[["pos"]][["bayes"]][["aov"]])
  }
  
  ## DV: Negative mood       ======
  
  # Model
  mod.mood.dich[["bayes"]][["neg"]] <- brm(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj),
    data = test_panas_dict |> filter(valence == "neg"),
    init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "bayes_R2"))
  
  # Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    #' 1. Predictive check, to check the fit of the outcome 
    #' 2. Chain stability
    #' 3. Model fit + coefficients 
    brms::pp_check(  mod.mood.dich[["bayes"]][["neg"]], ndraws=50)
    bayes_chain_stab(mod.mood.dich[["bayes"]][["neg"]])
    bayes_diag(      mod.mood.dich[["bayes"]][["neg"]])
    # bayes_tbl_sum(mod.mood.dich[["neg"]][["bayes"]][["aov"]])
  }
  
  # Save
  conditional_save( mod.mood.dich, "Mod-Bayes-PSD-on-Mood_Dichotomous")
}