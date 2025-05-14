# MW
test <- list()
test$mw <- brm(
  mw ~ I(c.Adjusted_Duration.diff.pos^2) + 
    c.Adjusted_Duration.diff.pos * 
    (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + 
    (1|subj), 
  data = data.probe.mood.sleep, 
  family = cumulative("probit"), init = 0, chains = 6, iter = 6000)

if( getOption("project_bayes_diagnostics") ){ 
  brms::pp_check(  test$mw, ndraws=50) # Predictive check
  bayes_chain_stab(test$mw) # chain stability
  bayes_diag(      test$mw) # general model fit and coefficients
  # bayes_tbl_sum(mod_bay_sleep_cont$mw) # coefficients (table)
}



# MW
mod.test <- list()
mod.test$mw.poly <- brm(
  mw ~ I(c.Adjusted_Duration.diff.pos^2) + 
    c.Adjusted_Duration.diff.pos * 
    (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + 
    (1|subj), 
  data = data.probe.mood.sleep, 
  family = cumulative("probit"), init = 0, chains = 6, iter = 6000)

if( getOption("project_bayes_diagnostics") ){ 
  brms::pp_check(  mod.test$mw.poly, ndraws=50) # Predictive check
  bayes_chain_stab(mod.test$mw.poly) # chain stability
  bayes_diag(      mod.test$mw.poly) # general model fit and coefficients
  # bayes_tbl_sum(mod_bay_sleep_cont$mw) # coefficients (table)
}

mod.test$mw.mood.scaled <- brm(
  mw ~ c.Adjusted_Duration.diff.pos * 
    (zlogapen + zlogbv + probenum_prop + pre_pos + pre_neg) + 
    (1|subj)
  , data = data.probe.mood.sleep |> mutate(
    pre_pos = pre_pos/40,
    pre_neg = pre_neg/40 )
  , family = cumulative("probit"), init = 0, chains = 6, iter = 6000)

if( getOption("project_bayes_diagnostics") ){ 
  brms::pp_check(  mod.test$mw.mood.scaled, ndraws=50) # Predictive check
  bayes_chain_stab(mod.test$mw.mood.scaled) # chain stability
  bayes_diag(      mod.test$mw.mood.scaled) # general model fit and coefficients
  # bayes_tbl_sum(mod_bay_sleep_cont$mw) # coefficients (table)
}
