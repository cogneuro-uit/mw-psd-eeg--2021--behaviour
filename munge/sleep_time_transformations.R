# bind self-report (SR) and actigraphy (AG) 
sleeptimes <- 
  sleep_diary |>
  left_join(actigraphy, by=c("subj","date"="date_end_ag"))

# Bind adjusted sleep times: 
