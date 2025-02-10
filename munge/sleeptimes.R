sleeptimes <- 
  sleep_diary |>
  left_join(actigraphy, by=c("subj","date"="date_end_ag"))
