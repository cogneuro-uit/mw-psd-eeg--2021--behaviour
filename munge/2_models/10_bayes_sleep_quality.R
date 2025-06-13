if( getOption("project_bayes_run_models") ){
  
  m.PSD_SQ <- list()
  
  m.PSD_SQ[["adj"]] <- brm(
    sleep_quality_SD ~ sleep, 
    PSD_sleep_quality |> filter(name == "Adjusted sleep"), 
    chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  pp_check(        m.PSD_SQ$adj, ndraws = 50)
  bayes_chain_stab(m.PSD_SQ$adj)
  bayes_diag(      m.PSD_SQ$adj)
  
  
  m.PSD_SQ[["sr"]] <- brm(
    sleep_quality_SD ~ sleep, 
    PSD_sleep_quality |> filter(name == "Self-reported sleep"), 
    chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  pp_check(        m.PSD_SQ$sr, ndraws = 50)
  bayes_chain_stab(m.PSD_SQ$sr)
  bayes_diag(      m.PSD_SQ$sr)
  
  
  m.PSD_SQ[["ag"]] <- brm(
    sleep_quality_SD ~ sleep, 
    PSD_sleep_quality |> filter(name == "Actigraphy sleep"), 
    chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  
  pp_check(        m.PSD_SQ$ag, ndraws = 50)
  bayes_chain_stab(m.PSD_SQ$ag)
  bayes_diag(      m.PSD_SQ$ag)

  conditional_save(m.PSD_SQ, "bayes_model_sleep_quality_sleep")
}