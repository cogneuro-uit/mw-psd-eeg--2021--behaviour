# simple tests        =====
effect_of_test_time <- 
  data.probe.mood.sleep.quiz.demo |>
  filter(sleepdep=="control") |>
  summarise(
    .by = c(subj, test_time)
    , dinural     = mean(value_dinural_avg) 
  ) |>
  summarise(
    , m_9   = mean(dinural[test_time=="9"], na.rm=T)
    , sd_9  = sd(  dinural[test_time=="9"], na.rm=T)
    , m_11  = mean(dinural[test_time=="11"], na.rm=T)
    , sd_11 = sd(  dinural[test_time=="11"], na.rm=T)
    
    , tmp  = list(  t.test(dinural[test_time=="9"], dinural[test_time=="11"]) ) 
    , tmp2 = list( ttestBF(dinural[test_time=="9" & !is.na(dinural)], dinural[test_time=="11" & !is.na(dinural)]) ) 
  ) |>
  mutate(
    diff    = m_9 - m_11
    , t     = map(tmp, ~ unlist(.x)[["statistic.t"]] |> as.double() )
    , df    = map(tmp, ~ unlist(.x)[["parameter.df"]] |> as.double() )
    , p     = map(tmp, ~ unlist(.x)[["p.value"]] |> fmt_APA_numbers(.p=T) )
    , bf    = map(tmp2, ~ extractBF(.x)[["bf"]])
    , tmp   = NULL 
    , tmp2  = NULL
  ) |>
  unnest( where(is.list) ) |>
  mutate( across(where(is.double), ~fmt_APA_numbers(.x)) )

#' simple bayes does not find an effect of test time on chronotype but welch t-test does. 


## brms model    =====
if( getOption("project_bayes_run_models") ){
  call_grp <- list()
  test_time <- list() 
  
  ## chronotype      ======
  call_grp$chronotype <- future({ 
    data <- brm(
      value_dinural_avg ~ c.Adjusted_Duration.diff.pos * test_time
      , data.probe.mood.sleep.quiz.demo |> filter(.by = c(subj, sleepdep), row_number() == 1)
      , chains = 6
      , init = 0
      , prior = set_prior("normal(0, 1)", class = "b"),
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  # Check state
  resolved(call_grp$chronotype)
  # Retrieve
  test_time$chronotype <- value(call_grp$chronotype)
  pp_check(        test_time$chronotype, ndraws = 50)
  bayes_chain_stab(test_time$chronotype)
  bayes_diag(      test_time$chronotype)
  
  
  ### WIDER   =======
  call_grp$chronotype_wide <- future({ 
    data <- brm(
      value_dinural_avg ~ c.Adjusted_Duration.diff.pos * test_time
      , data.probe.mood.sleep.quiz.demo |> filter(.by = c(subj, sleepdep), row_number() == 1)
      , chains = 6
      , init = 0
      , prior = set_prior("normal(0, 2)", class = "b"),
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  # Check state
  resolved(call_grp$chronotype_wide)
  # Retrieve
  test_time$chronotype_wide <- value(call_grp$chronotype_wide)
  pp_check(        test_time$chronotype_wide, ndraws = 50)
  bayes_chain_stab(test_time$chronotype_wide)
  bayes_diag(      test_time$chronotype_wide)
  
  ### SHALLOWER =======
  call_grp$chronotype_small <- future({ 
    data <- brm(
      value_dinural_avg ~ c.Adjusted_Duration.diff.pos * test_time
      , data.probe.mood.sleep.quiz.demo |> filter(.by = c(subj, sleepdep), row_number() == 1)
      , chains = 6
      , init = 0
      , prior = set_prior("normal(0, 0.5)", class = "b"),
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  # Check state
  resolved(call_grp$chronotype_small)
  # Retrieve
  test_time$chronotype_small <- value(call_grp$chronotype_small)
  pp_check(        test_time$chronotype_small, ndraws = 50)
  bayes_chain_stab(test_time$chronotype_small)
  bayes_diag(      test_time$chronotype_small)

    
  ## MW      ======
  call_grp$mw <- future({ 
    data <- brm(
      mw ~ c.Adjusted_Duration.diff.pos * test_time + (1|subj)
      , data.probe.mood.sleep.quiz.demo |>
        summarise(
          .by = c(subj, sleepdep)
          , mw = mean(as.numeric(mw))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , test_time = unique(test_time) )
      , chains = 6
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  # Check state
  resolved(call_grp$mw)
  # Retrieve
  test_time$mw <- value(call_grp$mw)
  pp_check(        test_time$mw, ndraws = 50)
  bayes_chain_stab(test_time$mw)
  bayes_diag(      test_time$mw)
  
  ## MB        =======
  call_grp$mb <- future({ 
    data <- brm(
      mb ~ c.Adjusted_Duration.diff.pos * test_time + (1|subj)
      , data.probe.mood.sleep.quiz.demo |> 
        filter(mw > 2) |>
        summarise(
          .by = c(subj, sleepdep)
          , mb = mean(as.numeric(mb))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , test_time = unique(test_time) )
      , chains = 6
      , cores = 6
      , iter = 6000 )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(call_grp$mb)
  test_time$mb <- value(call_grp$mb)
  pp_check(        test_time$mb, ndraws = 50)
  bayes_chain_stab(test_time$mb)
  bayes_diag(      test_time$mb)
  
  
  ##  SMW         =======
  call_grp$smw <- future({ 
    # Run model
    data <- brm(
      smw ~ c.Adjusted_Duration.diff.pos * test_time + (1|subj)
      , data.probe.mood.sleep.quiz.demo |> 
        filter(mw > 2) |>
        summarise(
          .by = c(subj, sleepdep)
          , smw = mean(as.numeric(smw))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , test_time = unique(test_time) )
      , chains = 6
      , cores = 6
      , iter = 6000)
    # Add criterion
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(test_time$smw)
  test_time$smw <- value(call_grp$smw)
  pp_check(        test_time$smw, ndraws = 50)
  bayes_chain_stab(test_time$smw)
  bayes_diag(      test_time$smw)
  
  
  ##  BV         =======
  call_grp$bv <- future({ 
    data <- brm(
      zlogbv ~ c.Adjusted_Duration.diff.pos * test_time + (1|subj)
      , data.probe.mood.sleep.quiz.demo |> 
        summarise(
          .by = c(subj, sleepdep)
          , zlogbv = mean(zlogbv)
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , test_time = unique(test_time) )
      , chains = 6
      , cores = 6
      , iter = 6000  ) 
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(call_grp$bv)
  test_time$bv <- value(call_grp$bv)
  pp_check(        test_time$bv, ndraws = 50)
  bayes_chain_stab(test_time$bv)
  bayes_diag(      test_time$bv)
  
  
  ##  AE       ======
  call_grp$ae <- future({ 
    data <- brm(
      zlogapen ~ c.Adjusted_Duration.diff.pos * test_time + (1|subj)
      , data.probe.mood.sleep.quiz.demo |> 
        summarise(
          .by = c(subj, sleepdep)
          , zlogapen = mean(zlogapen)
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , test_time = unique(test_time) )
      , chains = 6
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(call_grp$ae)
  test_time$ae <- value(call_grp$ae)
  pp_check(        test_time$ae, ndraws = 50)
  bayes_chain_stab(test_time$ae)
  bayes_diag(      test_time$ae)
  
  
  # 
  conditional_save(test_time, "Bayes - test time effects")
} else {
  # load("Bayes - group assigment test_2026-01-15_13-43-15_.RData")
}
