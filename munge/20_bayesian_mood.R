## Bayes models      ======
if( project[["bayes"]][["run_models"]] ){

  mod_mood_full <- list()
  
  mod_mood_full$mw <- brm(
    mw ~ sleepdep * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood, 
    family=cumulative("probit"), cores = 6, backend = "cmdstanr",
    init = 0, chains = 6, iter = 6000)
  # bayes_diag(mod_mood_full$mw)
  bayes_tbl_sum(mod_mood_full$mw) |> gt()
  
  mod_mood_full$mb <- brm(
    mb ~ sleepdep * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood |> filter(mw > 2), family=cumulative("probit"), 
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  # bayes_diag(mod_mood_full$mb)
  bayes_tbl_sum(mod_mood_full$mb)
  
  mod_mood_full$smw <- brm(
    smw ~ sleepdep * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood |> filter(mw > 2), family=cumulative("probit"),
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  # bayes_diag(mod_mood_full$smw)
  bayes_tbl_sum(mod_mood_full$smw)
  
  mod_mood_full$bv <- brm(
    zlogbv ~ sleepdep * (probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood, 
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  # bayes_diag(mod_mood_full$bv)
  bayes_tbl_sum(mod_mood_full$bv)
  
  mod_mood_full$ae <- brm(
    zlogapen ~ sleepdep * (probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood,
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  # bayes_diag(mod_mood_full$ae)
  bayes_tbl_sum(mod_mood_full$ae) |> gt()
  
  
  # Criteria 
  mod_mood_full$mw  <- brms::add_criterion(mod_mood_full$mw,  c("bayes_R2", "loo")) 
  mod_mood_full$mb  <- brms::add_criterion(mod_mood_full$mb,  c("bayes_R2", "loo")) 
  mod_mood_full$smw <- brms::add_criterion(mod_mood_full$smw, c("bayes_R2", "loo")) 
  mod_mood_full$ae  <- brms::add_criterion(mod_mood_full$ae,  c("bayes_R2", "loo")) 
  mod_mood_full$bv  <- brms::add_criterion(mod_mood_full$bv,  c("bayes_R2", "loo")) 
  
  ### Save      =======
  if( project[["bayes"]][["save"]][["to_file"]] ){
    save(
      mod_mood_full,
      
      file = paste0(
        "data/bayes_full_model_SR_exclude", 
        project[["bayes"]][["save"]][["date_time"]],".Rdata")
    )
  }
} 

