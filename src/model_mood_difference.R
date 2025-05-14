library(ProjectTemplate)
relative_path=""
load.project()

# model list
mod.mood <- list()

# data transformaiton
# kept? 
mood_diff_data <-
  data.probe.mood.sleep |>
  mutate(
    .before = 1,
    , pos_diff =  post_pos - pre_pos
    , neg_diff =  post_neg - pre_neg
    , mw  = as.numeric(mw)
    , mb  = as.numeric(mb)
    , smw = as.numeric(smw)
  ) |> 
  summarise(
    .by = c("subj", "c.Adjusted_Duration.diff.pos")
    , mw       = mean(as.numeric(mw))
    , mb       = mean(as.numeric(mb))
    , smw      = mean(as.numeric(smw))
    , pre_pos  = unique(pre_pos)
    , pre_neg  = unique(pre_neg)
    , neg_diff = unique(neg_diff)
    , pos_diff = unique(pos_diff)
  )

##  PSD     ======
#' PSD on pre-task positive mood
mod.mood$p$psd <- 
  brm(pre_pos ~ c.Adjusted_Duration.diff.pos
      , mood_diff_data)

#' PSD on pre-task negative mood
mod.mood$n$psd <- 
  brm(pre_neg ~ c.Adjusted_Duration.diff.pos
      , mood_diff_data)

##  PSD x  MW     =====
# DV: pos
mod.mood$p$psd.mw <- brm(
  pos_diff ~ c.Adjusted_Duration.diff.pos * mw
  , mood_diff_data, init = 0, iter = 6000, chains = 6)
brms::pp_check(  mod.mood$p$psd.mw, ndraws = 50)
bayes_chain_stab(mod.mood$p$psd.mw)
bayes_diag(      mod.mood$p$psd.mw)

# DV: Neg
mod.mood$n$psd.mw <- brm(
  neg_diff ~ c.Adjusted_Duration.diff.pos * mw
  , mood_diff_data, init = 0, iter = 6000, chains = 6)
brms::pp_check(  mod.mood$n$psd.mw, ndraws = 50)
bayes_chain_stab(mod.mood$n$psd.mw)
bayes_diag(      mod.mood$n$psd.mw)
