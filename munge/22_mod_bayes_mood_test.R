
if( project[["bayes"]][["run_models"]] ){
  
  #' Transform the PANAS to factor: 
  test_panas <- 
    panas_session |>
    mutate(
      sleepdep = factor(sleepdep),
      prepost = factor(prepost) |> fct_relevel("pre"),
      subj = factor(subj)
    )
  
  
  #  Dichotomous model       =====
  simple_mood_test <- list()
  
  ##     Frequentist        ======
  #' Although not bayesian, I put it in the Bayesian block to recalculate 
  #' it together with the Bayesian model (if anything were to change)
  
  simple_mood_test[["neg"]][["freq"]] <- 
    afex::aov_4(PANASsum_0 ~ sleepdep * prepost + (sleepdep*prepost|subj), 
                panas_session |> filter(valence=="neg"))
  simple_mood_test[["neg"]][["freq"]] |> summary()
  
  simple_mood_test[["pos"]][["freq"]] <- 
    afex::aov_4(PANASsum_0 ~ sleepdep * prepost + (sleepdep*prepost|subj), 
                panas_session |> filter(valence=="pos"))
  simple_mood_test[["pos"]][["freq"]] |> summary()
    
  
  ##     Bayesian         ======
  #' Use Bayesian linear models to check whether mood (positive and negative) 
  #' was influenced by Sleep, Pre-post test, and their interaction. 
  
  # Dependent variable: Positive mood
  simple_mood_test[["pos"]][["bayes"]][["aov"]] <- brm(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj),
    data = test_panas |> filter(valence=="pos"),
    init = 0, iter = 6000, chains = 6)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag( simple_mood_test[["pos"]][["bayes"]][["aov"]] )
    # Summary:
    bayes_tbl_sum( simple_mood_test[["pos"]][["bayes"]][["aov"]] ) |> gt()
  }
  
  # Dependent variable: Negative mood
  simple_mood_test[["neg"]][["bayes"]][["aov"]] <- brm(
    PANASsum_0 ~ sleepdep * prepost + (sleepdep * prepost | subj),
    data = test_panas |> filter(valence == "neg"),
    init = 0, iter = 6000, chains = 6)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag( simple_mood_test[["neg"]][["bayes"]][["aov"]] )
    # Summary:
    bayes_tbl_sum( simple_mood_test[["neg"]][["bayes"]][["aov"]] ) |> gt()
  }
  
  
  
  
  #         Continuous model           ======
  test_panas_sleep <- test_panas |> 
    left_join(
      sleeptimes_updated_trans,
      c("subj","sleepdep")
    )
  #' Dependent variable: positive mood
  simple_mood_test[["pos"]][["bayes"]][["cont"]] <- brm(
    PANASsum_0 ~ c.Adjusted_Duration.diff.pos * prepost + (c.Adjusted_Duration.diff.pos * prepost | subj),
    data = test_panas_sleep |> filter(valence=="pos"),
    init = 0, iter = 6000, chains = 6)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag( simple_mood_test[["pos"]][["bayes"]][["cont"]] )
    # Summary:
    bayes_tbl_sum( simple_mood_test[["pos"]][["bayes"]][["cont"]] ) |> gt()
  }
  
  # Dependent variable: Negative mood
  simple_mood_test[["neg"]][["bayes"]][["cont"]] <- brm(
    PANASsum_0 ~ c.Adjusted_Duration.diff.pos * prepost + (c.Adjusted_Duration.diff.pos * prepost | subj),
    data = test_panas_sleep |> filter(valence=="neg"),
    init = 0, iter = 6000, chains = 6)
  
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag( simple_mood_test[["neg"]][["bayes"]][["cont"]] )
    # Summary:
    bayes_tbl_sum( simple_mood_test[["neg"]][["bayes"]][["cont"]] ) |> gt()
  }
  
  
  # Save models         =====
  if( project[["bayes"]][["save"]][["to_file"]] ){
    save( simple_mood_test, file = paste0(
      "data/mod_bayes_mood_simple_test",
      project[["bayes"]][["save"]][["date_time"]], ".RData"))
  }
}




