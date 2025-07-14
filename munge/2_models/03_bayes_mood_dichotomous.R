
if( getOption("project_bayes_run_models") ){
  
  # List
  mod.mood.dich <- list()
  
  # DV: Positive mood        =======
  ## PSD on pre-task mood       ======
  mod.mood.dich[["pos"]][["psd"]] <- brm(
    PANASsum_0_pre ~ sleepdep + (1 | subj)
    , data = test_panas_diff |> filter(valence=="pos")
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  brms::pp_check(   mod.mood.dich$pos$psd, ndraws = 50)
  bayes_chain_stab( mod.mood.dich$pos$psd) 
  bayes_diag(       mod.mood.dich$pos$psd) 
  
  ## PSD x MW on mood change      =====
  mod.mood.dich[["pos"]][["psdXmw"]] <- brm(
    pos_diff ~ sleepdep * mw + (1 | subj)
    , data = mood_diff_test
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(   mod.mood.dich$pos$psdXmw, ndraws = 50)
  bayes_chain_stab( mod.mood.dich$pos$psdXmw) 
  bayes_diag(       mod.mood.dich$pos$psdXmw) 
  
  
  # DV: Negative mood        =======
  ## PSD on mood      =====
  mod.mood.dich[["neg"]][["psd"]] <- brm(
    PANASsum_0_pre ~ sleepdep + (1 | subj)
    , data = test_panas_diff |> filter(valence=="neg")
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(   mod.mood.dich$neg$psd, ndraws=50)
  bayes_chain_stab( mod.mood.dich$neg$psd) 
  bayes_diag(       mod.mood.dich$neg$psd) 
  
  ## PSD x MW on mood change      ======
  mod.mood.dich[["neg"]][["psdXmw"]] <- brm(
    neg_diff ~ sleepdep * mw + (1 | subj)
    , data = mood_diff_test
    , iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(   mod.mood.dich$neg$psdXmw, ndraws=50)
  bayes_chain_stab( mod.mood.dich$neg$psdXmw) 
  bayes_diag(       mod.mood.dich$neg$psdXmw) 
  
  
  # Save  =====
  conditional_save( mod.mood.dich, "Mod-Bayes--Mood--Dichotomous")
}