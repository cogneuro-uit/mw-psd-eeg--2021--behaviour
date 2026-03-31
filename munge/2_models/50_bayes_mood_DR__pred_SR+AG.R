if( getOption("project_bayes_run_models") ){
  
  
  mod.mood.ext <- list()
  f_call <- list()
  
  # PRE-TASK MOOD            ======
  ## DV: POSITIVE mood        =======
  ###  Adjusted               ======
  f_call[["pre_pos~ADJ_PSD"]] <- future({
    model <- brm(
      PANASsum_0_pre ~ c.Adjusted_Duration.diff.pos + (1 | subj)
      , data = test_panas_diff |> filter(valence=="pos")
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000 
      )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"))
  }, seed = TRUE)
  
  resolved( f_call[["pre_pos~ADJ_PSD"]] )
  mod.mood.ext[["pre_pos~ADJ_PSD"]] <- value( f_call[["pre_pos~ADJ_PSD"]] )

  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["pre_pos~ADJ_PSD"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["pre_pos~ADJ_PSD"]] ) 
    bayes_diag(       mod.mood.ext[["pre_pos~ADJ_PSD"]] ) 
  }
  
  ###  Actigraphy               ======
  f_call[["pre_pos~AG_PSD"]] <- future({
    model <- brm(
      PANASsum_0_pre ~ c.Actigraphy_Duration.diff.pos + (1 | subj)
      , data = test_panas_diff |> filter(valence=="pos")
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000
      , save_pars = save_pars(all = TRUE)
      )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["pre_pos~AG_PSD"]] )
  mod.mood.ext[["pre_pos~AG_PSD"]] <- value( f_call[["pre_pos~AG_PSD"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["pre_pos~AG_PSD"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["pre_pos~AG_PSD"]] ) 
    bayes_diag(       mod.mood.ext[["pre_pos~AG_PSD"]] ) 
  }
  
  ###  Self-report               ======
  f_call[["pre_pos~SR_PSD"]] <- future({
    model <- brm(
      PANASsum_0_pre ~ c.Self.report_Duration.diff.pos + (1 | subj)
      , data = test_panas_diff |> filter(valence=="pos")
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000 
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"))
  }, seed = TRUE)
  
  resolved( f_call[["pre_pos~SR_PSD"]] )
  mod.mood.ext[["pre_pos~SR_PSD"]] <- value( f_call[["pre_pos~SR_PSD"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["pre_pos~SR_PSD"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["pre_pos~SR_PSD"]] ) 
    bayes_diag(       mod.mood.ext[["pre_pos~SR_PSD"]] ) 
  }
  
  
  
  ## DV: NEGATIVE MOOD                  =====
  ###  Adjusted               ======
  f_call[["pre_neg~ADJ_PSD"]] <- future({
    model <- brm(
      PANASsum_0_pre ~ c.Adjusted_Duration.diff.pos + (1 | subj)
      , data = test_panas_diff |> filter(valence=="neg")
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000 
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE, reloo = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["pre_neg~ADJ_PSD"]] )
  mod.mood.ext[["pre_neg~ADJ_PSD"]] <- value( f_call[["pre_neg~ADJ_PSD"]] )

  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["pre_neg~ADJ_PSD"]], ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["pre_neg~ADJ_PSD"]]) 
    bayes_diag(       mod.mood.ext[["pre_neg~ADJ_PSD"]]) 
  }
  
  ###  Actigraphy               ======
  f_call[["pre_neg~AG_PSD"]] <- future({
    model <- brm(
      PANASsum_0_pre ~ c.Actigraphy_Duration.diff.pos + (1 | subj)
      , data = test_panas_diff |> filter(valence=="neg")
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000 
      , save_pars = save_pars(all = TRUE)
      )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE, reloo = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["pre_neg~AG_PSD"]] )
  mod.mood.ext[["pre_neg~AG_PSD"]] <- value( f_call[["pre_neg~AG_PSD"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["pre_neg~AG_PSD"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["pre_neg~AG_PSD"]] ) 
    bayes_diag(       mod.mood.ext[["pre_neg~AG_PSD"]] ) 
  }
  
  ###  Self-report               ======
  f_call[["pre_neg~SR_PSD"]] <- future({
    model <- brm(
      PANASsum_0_pre ~ c.Self.report_Duration.diff.pos + (1 | subj)
      , data = test_panas_diff |> filter(valence=="neg")
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000 
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE, reloo = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["pre_neg~SR_PSD"]] )
  mod.mood.ext[["pre_neg~SR_PSD"]] <- value( f_call[["pre_neg~SR_PSD"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["pre_neg~SR_PSD"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["pre_neg~SR_PSD"]] ) 
    bayes_diag(       mod.mood.ext[["pre_neg~SR_PSD"]] ) 
  }
  
  
  
  
  # MOOD CHANGE        ======
  #' PSD x MW
  ## DV: POSITIVE DIFF          =======
  ### Adjusted      =====
  f_call[["diff_pos~ADJ_PSD*MW"]] <- future({
    model <- brm(
      pos_diff ~ c.Adjusted_Duration.diff.pos * mw + (1 | subj)
      , data = mood_diff_test
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"))
  }, seed = TRUE)
  
  resolved( f_call[["diff_pos~ADJ_PSD*MW"]] )
  mod.mood.ext[["diff_pos~ADJ_PSD*MW"]] <- value( f_call[["diff_pos~ADJ_PSD*MW"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["diff_pos~ADJ_PSD*MW"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["diff_pos~ADJ_PSD*MW"]] ) 
    bayes_diag(       mod.mood.ext[["diff_pos~ADJ_PSD*MW"]] ) 
  }
  
  
  
  ### Actigraphy      =====
  f_call[["diff_pos~AG_PSD*MW"]] <- future({
    model <- brm(
      pos_diff ~ c.Actigraphy_Duration.diff.pos * mw + (1 | subj)
      , data = mood_diff_test
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["diff_pos~AG_PSD*MW"]] )
  mod.mood.ext[["diff_pos~AG_PSD*MW"]] <- value( f_call[["diff_pos~AG_PSD*MW"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["diff_pos~AG_PSD*MW"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["diff_pos~AG_PSD*MW"]] ) 
    bayes_diag(       mod.mood.ext[["diff_pos~AG_PSD*MW"]] ) 
  }
  
  

  ### Self-report      =====
  f_call[["diff_pos~SR_PSD*MW"]] <- future({
    model <- brm(
      pos_diff ~ c.Self.report_Duration.diff.pos * mw + (1 | subj)
      , data = mood_diff_test
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["diff_pos~SR_PSD*MW"]] )
  mod.mood.ext[["diff_pos~SR_PSD*MW"]] <- value( f_call[["diff_pos~SR_PSD*MW"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["diff_pos~SR_PSD*MW"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["diff_pos~SR_PSD*MW"]] ) 
    bayes_diag(       mod.mood.ext[["diff_pos~SR_PSD*MW"]] ) 
  }
  
  
  
  
  ## DV: NEGATIVE diff          =======
  ### Adjusted      =====
  f_call[["diff_neg~ADJ_PSD*MW"]] <- future({
    model <- brm(
      neg_diff ~ c.Adjusted_Duration.diff.pos * mw + (1 | subj)
      , data = mood_diff_test
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["diff_neg~ADJ_PSD*MW"]] )
  mod.mood.ext[["diff_neg~ADJ_PSD*MW"]] <- value( f_call[["diff_neg~ADJ_PSD*MW"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["diff_neg~ADJ_PSD*MW"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["diff_neg~ADJ_PSD*MW"]] ) 
    bayes_diag(       mod.mood.ext[["diff_neg~ADJ_PSD*MW"]] ) 
  }
  
  
  ### Actigraphy      =====
  f_call[["diff_neg~AG_PSD*MW"]] <- future({
    model <- brm(
      neg_diff ~ c.Actigraphy_Duration.diff.pos * mw + (1 | subj)
      , data = mood_diff_test
      , core = 6
      , chains = 6
      , iter = 6000
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["diff_neg~AG_PSD*MW"]] )
  mod.mood.ext[["diff_neg~AG_PSD*MW"]] <- value( f_call[["diff_neg~AG_PSD*MW"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["diff_neg~AG_PSD*MW"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["diff_neg~AG_PSD*MW"]] ) 
    bayes_diag(       mod.mood.ext[["diff_neg~AG_PSD*MW"]] ) 
  }
  
  
  ### Self-report      =====
  f_call[["diff_neg~SR_PSD*MW"]] <- future({
    model <- brm(
      neg_diff ~ c.Self.report_Duration.diff.pos * mw + (1 | subj)
      , data = mood_diff_test
      , family = student()
      , core = 6
      , chains = 6
      , iter = 6000
      , save_pars = save_pars(all = TRUE)
    )
    add_criterion(model, c("bayes_R2", "loo", "loo_R2"), moment_match = TRUE)
  }, seed = TRUE)
  
  resolved( f_call[["diff_neg~SR_PSD*MW"]] )
  mod.mood.ext[["diff_neg~SR_PSD*MW"]] <- value( f_call[["diff_neg~SR_PSD*MW"]] )
  
  if( getOption("project_bayes_diagnostics") ){ 
    brms::pp_check(   mod.mood.ext[["diff_neg~SR_PSD*MW"]] , ndraws = 50)
    bayes_chain_stab( mod.mood.ext[["diff_neg~SR_PSD*MW"]] ) 
    bayes_diag(       mod.mood.ext[["diff_neg~SR_PSD*MW"]] ) 
  }

  
  
  # Save  =====
  conditional_save(mod.mood.ext, "Mod_Bayes_Mood__PSD_&_MW")  
}
