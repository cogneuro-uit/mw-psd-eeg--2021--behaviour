# Data transformations 
test_panas <- 
  panas_session |>
  mutate(
    sleepdep = factor(sleepdep),
    prepost = factor(prepost) |> fct_relevel("pre"),
    subj = factor(subj)
  ) |>
  left_join(
    sleeptimes_updated_trans,
    by = c("subj", "sleepdep")
  ) 

# EXCLUDE (Dichotomous analyses data)
test_panas_dict <- test_panas |>
  filter( Adjusted_Duration.diff <= -1.5 )

#' Mood difference 
test_panas_diff <- 
  test_panas |>
  pivot_wider(names_from=prepost, values_from = c(PANASsum, PANASsum_0)) |>
  mutate(mood_diff = PANASsum_0_post - PANASsum_0_pre) 

mood_diff_test <- 
  data.probe.mood.sleep |>
  summarise(
    .by = c(subj, c.Adjusted_Duration.diff.pos)
    , mw = mean(as.numeric(mw))
    , mb = mean(as.numeric(mb))
    , smw = mean(as.numeric(smw))
    , pos_diff = unique(post_pos - pre_pos)
    , neg_diff = unique(post_neg - pre_neg)
    , 
  ) 


