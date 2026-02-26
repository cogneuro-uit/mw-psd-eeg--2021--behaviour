# Simple t-test across NS condition       ======
simple_test_order_effects <- 
  data.probe.mood.sleep |>
  summarise(
    .by = c(subj, sleepdep, session)
    , mw = mean(as.numeric(mw))
    , mb = mean(as.numeric(mb))
    , smw = mean(as.numeric(smw))
    , bv = mean(zlogbv)
    , ae = mean(zlogapen)
    , pre_pos = mean(pre_pos)
    , pre_neg = mean(pre_neg)
  ) |>
  pivot_longer(c(mw,mb,smw,bv,ae,pre_pos,pre_neg)) |>
  summarise(
    .by = c(sleepdep, name)
    , test = "S1 vs. S2"
    
    , m_s1     = mean(value[session=="S1"])
    , sd_s1    = sd(  value[session=="S1"])
    , m_s2     = mean(value[session=="S2"])
    , sd_s2    = sd(  value[session=="S2"])
    
    , int  = list(    t.test(value[session=="S1"], value[session=="S2"]))
    , bf   = extractBF(ttestBF(value[session=="S1"], value[session=="S2"]))$bf
  ) |> 
  mutate(
    m_diff     = m_s1 - m_s2
    , t  = map(int, ~ unlist(.x)[["statistic.t"]])
    , df = map(int, ~ unlist(.x)[["parameter.df"]])
    , p  = map(int, ~ unlist(.x)[["p.value"]]) |> fmt_APA_numbers(.p=T)
    , across(c(t,df,bf), ~fmt_APA_numbers(.x))
  )  |>
  unnest(t,df,p) |>
  select(-int)


# Bayesian          =====
#' test the overall interaction between group and sleep cond

# According to the simple t-test, there is no effect of group assignment
if( getOption("project_bayes_run_models") ){
  call_grp <- list()
  order_effects <- list() 
  
## MW      ======
  call_grp$mw <- future({ 
    data <- brm(
      mw ~ c.Adjusted_Duration.diff.pos * session + (1|subj)
      , data.probe.mood.sleep |> 
        summarise(
          .by = c(subj, sleepdep)
          , mw = mean(as.numeric(mw))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , session = unique(session) )
      , chains = 6
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  # Check state
  resolved(call_grp$mw)
  # Retrieve
  order_effects$mw <- value(call_grp$mw)
  pp_check(        order_effects$mw, ndraws = 50)
  bayes_chain_stab(order_effects$mw)
  bayes_diag(      order_effects$mw)
    
## MB        =======
  call_grp$mb <- future({ 
    data <- brm(
      mb ~ c.Adjusted_Duration.diff.pos * session + (1|subj)
      , data.probe.mood.sleep |> 
        filter(mw > 2) |>
        summarise(
          .by = c(subj, sleepdep)
          , mb = mean(as.numeric(mb))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , session = unique(session) )
      , chains = 6
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(call_grp$mb)
  order_effects$mb <- value(call_grp$mb)
  pp_check(        order_effects$mb, ndraws = 50)
  bayes_chain_stab(order_effects$mb)
  bayes_diag(      order_effects$mb)
  
  
##  SMW         =======
  call_grp$smw <- future({ 
    # Run model
    data <- brm(
      smw ~ c.Adjusted_Duration.diff.pos * session + (1|subj)
      , data.probe.mood.sleep |> 
        filter(mw > 2) |>
        summarise(
          .by = c(subj, sleepdep)
          , smw = mean(as.numeric(smw))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , session = unique(session) )
      , chains = 6
      , cores = 6
      , iter = 6000)
    # Add criterion
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(order_effects$smw)
  order_effects$smw <- value(call_grp$smw)
  pp_check(        order_effects$smw, ndraws = 50)
  bayes_chain_stab(order_effects$smw)
  bayes_diag(      order_effects$smw)
  
  
##  BV         =======
  call_grp$bv <- future({ 
    data <- brm(
      zlogbv ~ c.Adjusted_Duration.diff.pos * session + (1|subj)
      , data.probe.mood.sleep |>
        summarise(
          .by = c(subj, sleepdep)
          , zlogbv = mean(as.numeric(zlogbv))
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , session = unique(session) )
      , chains = 6
      , cores = 6
      , iter = 6000  ) 
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(call_grp$bv)
  order_effects$bv <- value(call_grp$bv)
  pp_check(        order_effects$bv, ndraws = 50)
  bayes_chain_stab(order_effects$bv)
  bayes_diag(      order_effects$bv)
  
  
##  AE       ======
  call_grp$ae <- future({ 
    data <- brm(
      zlogapen ~ c.Adjusted_Duration.diff.pos * session + (1|subj)
      , data.probe.mood.sleep |> 
        summarise(
          .by = c(subj, sleepdep)
          , zlogapen = mean(zlogapen)
          , c.Adjusted_Duration.diff.pos = unique(c.Adjusted_Duration.diff.pos)
          , session = unique(session) )
      , chains = 6
      , cores = 6
      , iter = 6000  )
    add_criterion(data, c("bayes_R2", "loo","loo_R2"))
  }, seed = TRUE)
  
  resolved(call_grp$ae)
  order_effects$ae <- value(call_grp$ae)
  pp_check(        order_effects$ae, ndraws = 50)
  bayes_chain_stab(order_effects$ae)
  bayes_diag(      order_effects$ae)


  
  conditional_save(order_effects, "Bayes - order effects test")
} else {
  # load("Bayes - group assigment test_2026-01-15_13-43-15_.RData")
}

# 
# data.probe.mood.sleep |>
#   summarise(
#     .by = c(subj, sleepdep, group)
#     , mw = mean(as.numeric(mw))
#     , bv = mean(as.numeric(zlogbv))
#     , ae = mean(as.numeric(zlogapen))
#   ) |> pivot_longer(c(mw,bv,ae)) |>
#   ggplot(aes(sleepdep, value, col = group)) + 
#   facet_wrap(~name)+ 
#   stat_summary() +
#   stat_summary(aes(group=subj), alpha=.1) +
#   stat_summary(aes(group=subj), alpha=.1, geom="line") 
