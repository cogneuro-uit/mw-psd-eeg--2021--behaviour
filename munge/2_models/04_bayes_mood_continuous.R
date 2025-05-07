if( getOption("project_bayes_run_models") ){
  
  mod.mood.cont <- list()
  
  # DV: Positive mood        =======
  ## PSD on pre-task mood       ======
  mod.mood.cont[["pos"]][["psd"]] <- brm(
    PANASsum_0_pre ~ c.Adjusted_Duration.diff.pos + (1 | subj)
    , data = test_panas_diff |> filter(valence=="pos")
    , init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(  mod.mood.cont$pos$psd, ndraws=50)
  bayes_chain_stab(mod.mood.cont$pos$psd) 
  bayes_diag(      mod.mood.cont$pos$psd) 
  
  ## PSD x MW on mood change      =====
  mod.mood.cont[["pos"]][["psdXmw"]] <- brm(
    pos_diff ~ c.Adjusted_Duration.diff.pos * mw + (1 | subj)
    , data = mood_diff_test
    , init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(  mod.mood.cont$pos$psdXmw, ndraws=50)
  bayes_chain_stab(mod.mood.cont$pos$psdXmw) 
  bayes_diag(      mod.mood.cont$pos$psdXmw) 
  
  
  # DV: Negative mood        =======
  ## PSD on mood      =====
  mod.mood.cont[["neg"]][["psd"]] <- brm(
    PANASsum_0_pre ~ c.Adjusted_Duration.diff.pos + (1 | subj)
    , data = test_panas_diff |> filter(valence=="neg")
    , init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(  mod.mood.cont$neg$psd, ndraws=50)
  bayes_chain_stab(mod.mood.cont$neg$psd) 
  bayes_diag(      mod.mood.cont$neg$psd) 
  
  ## PSD x MW on mood change      ======
  mod.mood.cont[["neg"]][["psdXmw"]] <- brm(
    neg_diff ~ c.Adjusted_Duration.diff.pos * mw + (1 | subj)
    , data = mood_diff_test
    , init = 0, iter = 6000, chains = 6) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  brms::pp_check(  mod.mood.cont$neg$psdXmw, ndraws=50)
  bayes_chain_stab(mod.mood.cont$neg$psdXmw) 
  bayes_diag(      mod.mood.cont$neg$psdXmw) 
  
  
  # Save  =====
  conditional_save(mod.mood.cont, "Mod-Bayes--Mood--PSDxMW")  
}
