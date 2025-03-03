
if( getOption("project_bayes_run_models") ){
  
  #' Transform the PANAS to factor: 
  test_panas <- 
    panas_session |>
    mutate(
      sleepdep = factor(sleepdep),
      prepost = factor(prepost) |> fct_relevel("pre"),
      subj = factor(subj)
    ) |>
    left_join(
      sleeptimes_updated_trans,
      by = c("subj", "sleepdep")
    )

  # list  
  mood_test <- list()
  
  #         Continuous model           ======
  #' Dependent variable: positive mood
  ## Model
  mood_test[["pos"]][["bayes"]][["cont"]] <- brm(
    PANASsum_0 ~ c.Adjusted_Duration.diff.pos * prepost + (c.Adjusted_Duration.diff.pos * prepost | subj),
    data = test_panas |> filter(valence=="pos"),
    init = 0, iter = 6000, chains = 6)
  ## Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mood_test[["pos"]][["bayes"]][["cont"]], ndraws=50) # Predictive check
    bayes_chain_stab(mood_test[["pos"]][["bayes"]][["cont"]]) # chain stability
    bayes_diag(mood_test[["pos"]][["bayes"]][["cont"]]) # general model fit and coefficients
    # bayes_tbl_sum(mood_test[["pos"]][["bayes"]][["cont"]]) # coefficients (table)
  }
  ## Criterion
  mood_test[["pos"]][["bayes"]][["cont"]] <- 
    mood_test[["pos"]][["bayes"]][["cont"]] |>
    add_criterion(c("loo", "bayes_R2"))
  
  # Dependent variable: Negative mood
  ## Model
  mood_test[["neg"]][["bayes"]][["cont"]] <- brm(
    PANASsum_0 ~ c.Adjusted_Duration.diff.pos * prepost + (c.Adjusted_Duration.diff.pos * prepost | subj),
    data = test_panas |> filter(valence=="neg"),
    init = 0, iter = 6000, chains = 6)
  ## Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mood_test[["neg"]][["bayes"]][["cont"]], ndraws=50) # Predictive check
    bayes_chain_stab(mood_test[["neg"]][["bayes"]][["cont"]]) # chain stability
    bayes_diag(mood_test[["neg"]][["bayes"]][["cont"]]) # general model fit and coefficients
    # bayes_tbl_sum(mood_test[["neg"]][["bayes"]][["cont"]]) # coefficients (table)
  }
  ## Criterion
  mood_test[["neg"]][["bayes"]][["cont"]] <- 
    mood_test[["neg"]][["bayes"]][["cont"]] |>
    add_criterion(c("loo", "bayes_R2"))
  
  #  Dichotomous model       =====
  #' **EXCLUDE**
  test_panas_dict <- test_panas |>
    filter( Adjusted_Duration.diff <= -1.5 )
  
  ##     Frequentist        ======
  #' Although not bayesian, I put it in the Bayesian block to recalculate 
  #' it together with the Bayesian model (if anything were to change)
  
  mood_test[["neg"]][["freq"]] <- 
    afex::aov_4(PANASsum_0 ~ sleepdep * prepost + (sleepdep*prepost|subj), 
                test_panas_dict |> filter(valence=="neg"))
  mood_test[["neg"]][["freq"]] |> summary()
  
  mood_test[["pos"]][["freq"]] <- 
    afex::aov_4(PANASsum_0 ~ sleepdep * prepost + (sleepdep*prepost|subj), 
                test_panas_dict |> filter(valence=="pos"))
  mood_test[["pos"]][["freq"]] |> summary()
    
  
  ##     Bayesian         ======
  #' Use Bayesian linear models to check whether mood (positive and negative) 
  #' was influenced by Sleep, Pre-post test, and their interaction. 
  
  # Dependent variable: Positive mood
  ## Model
  mood_test[["pos"]][["bayes"]][["aov"]] <- brm(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj),
    data = test_panas_dict |> filter(valence=="pos"),
    init = 0, iter = 6000, chains = 6)
  ## Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mood_test[["pos"]][["bayes"]][["aov"]], ndraws=50) # Predictive check
    bayes_chain_stab(mood_test[["pos"]][["bayes"]][["aov"]]) # chain stability
    bayes_diag(mood_test[["pos"]][["bayes"]][["aov"]]) # general model fit and coefficients
    # bayes_tbl_sum(mood_test[["pos"]][["bayes"]][["aov"]]) # coefficients (table)
  }
  ## Criterion
  mood_test[["pos"]][["bayes"]][["aov"]] <- 
    mood_test[["pos"]][["bayes"]][["aov"]] |>
    add_criterion(c("loo", "bayes_R2"))
  
  # Dependent variable: Negative mood
  ## Model
  mood_test[["neg"]][["bayes"]][["aov"]] <- brm(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj),
    data = test_panas_dict |> filter(valence == "neg"),
    init = 0, iter = 6000, chains = 6)
  ## Diagnostics
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(mood_test[["neg"]][["bayes"]][["aov"]], ndraws=50) # Predictive check
    bayes_chain_stab(mood_test[["neg"]][["bayes"]][["aov"]]) # chain stability
    bayes_diag(mood_test[["neg"]][["bayes"]][["aov"]]) # general model fit and coefficients
    # bayes_tbl_sum(mood_test[["neg"]][["bayes"]][["aov"]]) # coefficients (table)
  }
  ## Criterion
  mood_test[["neg"]][["bayes"]][["aov"]] <- 
    mood_test[["neg"]][["bayes"]][["aov"]] |>
    add_criterion(c("loo", "bayes_R2"))
  
  
  # Save models         =====
  if( getOption("project_bayes_save_to_file") ){
    time <- ""
    if( getOption("project_bayes_save_with_date_time") ) time <- getOption("project_date_time")
    save( mood_test, file = paste0(
      "data/mod_test_mood_freq+bay", time, ".RData"))
  }
}




