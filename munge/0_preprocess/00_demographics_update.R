demographics <- 
  demographics |>
  mutate(
    test_time = if_else(S1_test_time==S2_test_time, S1_test_time, "NOT EQUAL")
    , test_time = if_else(test_time=="8.5", "9", test_time)
    , test_time_cond = if_else(test_time=="9", "Early", "Late")
  ) 
