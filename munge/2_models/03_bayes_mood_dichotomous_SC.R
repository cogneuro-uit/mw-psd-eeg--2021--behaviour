
if( getOption("project_bayes_run_models") ){
  
  # List
  mod.mood.dich.sc <- list()
  
  # DV: Positive mood        =======
  ## PSD on pre-task mood       ======
  mod.mood.dich.sc[["pos"]][["pre"]] <- brm(
    PANASsum_0_pre ~ sleepdep + (1 | subj)
    , data = test_panas_diff |> 
      filter(!(subj %in% exclude_sleep_adjusted)) |>
      filter(valence=="pos")
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  brms::pp_check(   mod.mood.dich.sc[["pos"]][["pre"]], ndraws = 50)
  bayes_chain_stab( mod.mood.dich.sc[["pos"]][["pre"]]) 
  bayes_diag(       mod.mood.dich.sc[["pos"]][["pre"]]) 
  
  
  
  ## PSD x MW on mood change      =====
  mod.mood.dich.sc[["pos"]][["change"]] <- brm(
    pos_diff ~ sleepdep * mw + (1 | subj)
    , data = mood_diff_test |>
      filter(!(subj %in% exclude_sleep_adjusted))
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  brms::pp_check(   mod.mood.dich.sc[["pos"]][["change"]], ndraws = 50)
  bayes_chain_stab( mod.mood.dich.sc[["pos"]][["change"]]) 
  bayes_diag(       mod.mood.dich.sc[["pos"]][["change"]]) 
  
  
  # DV: Negative mood        =======
  ## PSD on mood      =====
  mod.mood.dich.sc[["neg"]][["pre"]] <- brm(
    PANASsum_0_pre ~ sleepdep + (1 | subj)
    , data = test_panas_diff |> 
      filter(!(subj %in% exclude_sleep_adjusted)) |>
      filter(valence=="neg")
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  brms::pp_check(   mod.mood.dich.sc[["neg"]][["pre"]], ndraws=50)
  bayes_chain_stab( mod.mood.dich.sc[["neg"]][["pre"]]) 
  bayes_diag(       mod.mood.dich.sc[["neg"]][["pre"]]) 
  
  ## PSD x MW on mood change      ======
  mod.mood.dich.sc[["neg"]][["change"]] <- brm(
    neg_diff ~ sleepdep * mw + (1 | subj)
    , data = mood_diff_test |>
      filter(!(subj %in% exclude_sleep_adjusted))
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  brms::pp_check(   mod.mood.dich.sc[["neg"]][["change"]], ndraws=50)
  bayes_chain_stab( mod.mood.dich.sc[["neg"]][["change"]]) 
  bayes_diag(       mod.mood.dich.sc[["neg"]][["change"]]) 
  
  
  # Save  =====
  conditional_save( mod.mood.dich.sc, "Mod-Bayes--Mood--Dichotomous-SC")
}