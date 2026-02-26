# Attach sleep questionnaires to data
data.probe.mood.sleep.quiz <-
  data.probe.mood.sleep |>
  left_join(
    sleep_quiz_summary_subject
    , by = "subj"
  )



# attach demographics to the data
data.probe.mood.sleep.quiz.demo <- 
  data.probe.mood.sleep.quiz |>
  left_join(
    demographics
    , by = "subj"
  ) |>
  mutate(
    test_time = fct_relevel(test_time, "9")
    , test_time_cond = fct_relevel(test_time_cond, "Early")
  )
