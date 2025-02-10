fnames = list.files("data/actigraphy", pattern="*.csv", full.names = T) 

subjs <- map_chr(fnames, ~str_split(.x, "[/_]")[[1]][[3]]) |>  unique() 
subjs <- subjs[!subjs=="030"]
  #' Subject 030 **does not** have actigraphy data


##' combine files per subj because some files have duplicate days
actigraphy_activity <- 
  map_df(subjs, \(l_subj){
    fnames = list.files("data/actigraphy", pattern = sprintf("%s_.*.csv", l_subj), full.names = T) 
    
    #' Get data from sleep diary, that contain the relevant sleep nights. 
    #' This is restricted to the three (four) nights before each respective test
    #' (i.e., before their NS and PSD nights). 
    #' We add one (lead) to the nights, due to how sleep is estimated in the 
    #' actigraphy. I.e., if participants went to sleep 23:00, they went to sleep 
    #' the *prior* night, and hence we need data from the preceding night. 
    
    get_sleep_nights <- 
      sleep_diary |>
      filter(subj==l_subj) |>
      mutate(
        .before=1,
        pre_control_lead = if_else(pre_control==1 | lead(pre_control==1), 1, 0),
        pre_sleepdep_lead = if_else(pre_sleepdep==1 | lead(pre_sleepdep==1), 1, 0),
      ) |> 
      filter(pre_control_lead==1 | pre_sleepdep_lead==1)
    
    # Get activity events
    map_df(fnames, \(fname){
      print(fname)
      
      d2 <- suppressWarnings( suppressMessages(
        read_csv(fname, locale = readr::locale(encoding = "UTF-16"), col_names = "sing", col_types = cols(.default = "c"))))
      ix <- which(str_starts(d2$sing, "Line,Date"))[2]
  
      # Get name of the columns
      c_names <- d2[ix,] |> 
        separate_wider_delim(sing, ",", names_sep = "t") |> 
        select(-singt13) |>
        unlist()
      
      # Get the relevant columns and apply column name
      d2 |>
        filter(c(rep(FALSE, ix), rep(TRUE, nrow(d2)-ix))) |>
        separate_wider_delim(sing, ",", names_sep="t") |>
        select(-singt13) |>
        rename_with( ~ map(.x, \(x){
          x <- c_names[x]
          }) |> list_c() 
        ) |>
        mutate(
          .before = 1, 
          subj    = l_subj, 
          date2   = if_else(str_detect(Date, "/"), mdy(Date), ymd(Date)),
        )
    }) |>
      # Add condition days & fiter others out
      mutate(
        .before = 3, 
        cond = case_when(
          date2 %in% get_sleep_nights$date[get_sleep_nights$pre_control_lead==1]  ~ "NS",
          date2 %in% get_sleep_nights$date[get_sleep_nights$pre_sleepdep_lead==1] ~ "PSD",
        ) 
      ) |>
      filter(!is.na(cond))
    
  }) |> 
  mutate(
    .by = c(subj, date2),
    .after = 6, 
    across((7:14)-2, ~as.numeric(.x)),
    time = case_when(
      #' IF it is AM/PM
      #' 12 is equal to 0
      str_detect(Time, "AM") & str_detect(Time, "^12:") ~ 
        str_split(Time, " ") |> map_chr(1) |> hms() - hours(12),
      str_detect(Time, "AM") ~
        str_split(Time, " ") |> map_chr(1) |> hms(),
      str_detect(Time, "PM") & str_detect(Time, "^12:") ~
        str_split(Time, " ") |> map_chr(1) |> hms(),
      str_detect(Time, "PM") ~
        str_split(Time, " ") |> map_chr(1) |> hms() + hours(12),
      T ~ Time |> hms(),
    )
  ) 


