
if( project[["bayes"]][["run_models"]] ){
  
  mod_bay_sleep_cont <- list()
  
  # MW
  mod_bay_sleep_cont$mw <- brm(
    mw ~ c.Duration.SD.pos * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep, 
    family = cumulative("probit"), backend = "cmdstanr",
    init = 0, chains = 6, iter = 6000)
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_bay_sleep_cont$mw)
    # Summary:
    bayes_tbl_sum(mod_bay_sleep_cont$mw) |> gt()
  }
  
  # MB
  mod_bay_sleep_cont$mb <- brm(
    mb ~ c.Duration.SD.pos * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep |> filter(mw > 2), family = cumulative("probit"), 
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_bay_sleep_cont$mb)
    # Summary:
    bayes_tbl_sum(mod_bay_sleep_cont$mb)
  }
  
  # SMW
  mod_bay_sleep_cont$smw <- brm(
    smw ~ c.Duration.SD.pos * (zlogapen + zlogbv + probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep |> filter(mw > 2), family = cumulative("probit"),
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_bay_sleep_cont$smw)
    # Summary:
    bayes_tbl_sum(mod_bay_sleep_cont$smw)
  }
  
  # BV
  mod_bay_sleep_cont$bv <- brm(
    zlogbv ~ c.Duration.SD.pos * (probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep, 
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_bay_sleep_cont$bv)
    # Summary:
    bayes_tbl_sum(mod_bay_sleep_cont$bv)
  }
  
  # AE
  mod_bay_sleep_cont$ae <- brm(
    zlogapen ~ c.Duration.SD.pos * (probenum + pre_pos + pre_neg) + (1|subj), 
    data = data.probe.mood.sleep,
    init = 0, chains = 6, iter = 6000, cores = 6, backend = "cmdstanr")
  if( project[["bayes"]][["set"]][["diagnostic_feedback"]] ){
    # Diagnostics
    bayes_diag(mod_bay_sleep_cont$ae)
    # Summary:
    bayes_tbl_sum(mod_bay_sleep_cont$ae) |> gt()
  }
  
  # Criteria 
  mod_bay_sleep_cont$mw  <- brms::add_criterion(mod_bay_sleep_cont$mw,  c("bayes_R2", "loo")) 
  mod_bay_sleep_cont$mb  <- brms::add_criterion(mod_bay_sleep_cont$mb,  c("bayes_R2", "loo")) 
  mod_bay_sleep_cont$smw <- brms::add_criterion(mod_bay_sleep_cont$smw, c("bayes_R2", "loo")) 
  mod_bay_sleep_cont$ae  <- brms::add_criterion(mod_bay_sleep_cont$ae,  c("bayes_R2", "loo")) 
  mod_bay_sleep_cont$bv  <- brms::add_criterion(mod_bay_sleep_cont$bv,  c("bayes_R2", "loo")) 
  
  
  ### Save      =======
  if( project[["bayes"]][["save"]][["to_file"]] ){
    save(mod_bay_sleep_cont, file = paste0(
      "data/bayes_model_cont_SD", 
      project[["bayes"]][["save"]][["date_time"]] , ".rdata")
    )
  }
} 

