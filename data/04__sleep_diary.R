fnames = list.files(
  paste0(relative_path, "data/baseline+diary"), pattern="*.xlsx", full.names = T) 
fnames = fnames[!str_detect(fnames, "~")]

read_xlsx <- function(...){
  suppressMessages(readxl::read_xlsx(...))
}
read_xlsx2 <- function(...){
  d <- read_xlsx(...)
  if(sum(dim(d))==0){
    return(tibble(`...1`=NA)) 
  } else {
    return(d)
  }
}

# old data reading
#' read_sheet <- function(fname) {
#'   print(fname)
#'   subj = str_split(fname, "/")[[1]][3] |> str_split("_")[[1]][1]
#' 
#'   ##' section 1: Generell sovn
#'   diary <- read_xlsx2(fname, sheet="Sleep Diary", range="A3:R20", col_names = T)
#'   diary |> mutate(date=as_date(Dato), dayno=dag,
#'                   sleep_onset_h=Q2_Time+(Q2_Min+`Q3 _Min`)/60.,
#'                   wake_onset_h =Q6_Time+(Q6_Min)/60.,
#'                   sleep_duration=wake_onset_h-(sleep_onset_h-24),
#'                   sleep_quality = S_Quality) |>
#'     select(date, sleep_onset_h, wake_onset_h, sleep_duration, sleep_quality) |>
#'     filter(!is.na(date)) |>
#'     mutate(group=if_else(n()==14, "LSD", "ESD"),
#'            pre_control=0,
#'            pre_sleepdep=0)-> dc
#'   if(first(dc$group)=="LSD"){
#'     dc$pre_control[5:7]=1
#'     dc$pre_sleepdep[12:14]=1
#'   } else {
#'     dc$pre_control[15:17]=1
#'     dc$pre_sleepdep[8:10]=1
#'   }
#' 
#'   #' subj 31 was missing SR for last 3 nights of normal sleep, substitute with
#'   #' previous 4 nights
#'   if(subj=="031"){
#'     dc$pre_control[11:17]=1
#'   }
#' 
#'   tibble(subj=subj, dc)
#' }


# New diary: "sleepdiary2"
sleep_diary <- map(fnames, \(fname) {
  print(fname)
  subj <- strsplit(fname, "\\/|\\_")[[1]][3]
  
  ##' section 1: Generell sovn
  diary <- read_xlsx2(fname, sheet="sleepdiary2", range="A1:J20", col_names = T)
  
  if(isFALSE("notes" %in% names(diary))){
    diary$notes <- NA
  }
  
  dc <- diary |> mutate(
    date = as.Date(Dato),
    # Convert to only time -> time code (hms) -> seconds -> hours.
    entered_bed = period_to_seconds(hms(strftime(q1, "%H:%M:%S", tz="UTC")))/60/60,
    
    # Cumulative time  
    entered_bed_cum = if_else(entered_bed<=7.3, entered_bed+24, entered_bed),
    # latest bed we found is 7h 10min
    tried_to_sleep = period_to_seconds(hms(strftime(q2, "%H:%M:%S", tz="UTC")))/60/60,
    tried_to_sleep_cum = if_else(tried_to_sleep<=7.7, tried_to_sleep+24, tried_to_sleep),
    # latest sleep we found is 7h 15m 
    sleep_delay = ifelse(!is.na(q3), period_to_seconds(minutes(round(q3)))/60/60, 0), 
    #' If it is "NA" then add 0, b/c, calculation problems.  
    #' Round because some par. have ".x" (some value that does not convert)
    night_wake_ups = q4,
    night_wake_up_time = ifelse(!is.na(q5), period_to_seconds(minutes(round(q5)))/60/60, 0), 
    #' to hours. If missing, to 0 
    last_awaking = period_to_seconds(hms(strftime(q6, "%H:%M:%S", tz="UTC")))/60/60,
    rise_from_bed = period_to_seconds(hms(strftime(q7, "%H:%M:%S", tz="UTC")))/60/60,
    sleep_quality = q8,
    # Sleep time is tried to sleep + delay (but not over 24!)
    sleep_time = ifelse(is.na(sleep_delay), 
                        tried_to_sleep,
                        ifelse(tried_to_sleep + sleep_delay > 24, 
                               (tried_to_sleep + sleep_delay) - 24,
                               tried_to_sleep + sleep_delay)),
    sleep_time_cum = ifelse(is.na(sleep_delay), 
                            tried_to_sleep_cum, 
                            tried_to_sleep_cum + sleep_delay),
    
    # last awake fix: 
    # if the difference between rise_from_bed and last_awaking is more than 2 hours, 
    # use rise_from_bed instead. The reason is that some participants has reported 
    # their last awaking as 1, while they rose from bed at 9. 
    # 3 Hours has been decided based on being more reasonable estimate. We 
    # Believe that if more than 3 hours pass, it is more likely that participants 
    # fell asleep than laying in bed for that amount of time.
    diff_awake_bed = rise_from_bed-last_awaking,
    last_awaking_fix = ifelse(is.na(diff_awake_bed), rise_from_bed,
                              ifelse(diff_awake_bed>3, rise_from_bed, last_awaking)),
    
    # Duration calculate from sleep_time (w/delay) and to last_awaking
    sleep_duration = last_awaking_fix-(sleep_time_cum-24),
    sleep_duration_sub = ifelse(is.na(night_wake_up_time), 
                                sleep_duration, 
                                sleep_duration-night_wake_up_time),
    #' Subtracts away night wake ups (similar to what the watch does for "sleep time")
    # Add participants notes
    notes = notes, 
  ) |> 
    select(date:sleep_duration, notes) |> 
    filter(!is.na(date)) |>
    mutate(group=if_else(n()==14, "LSD", "ESD"),
           pre_control=0,
           pre_sleepdep=0)
  
  
  if(first(dc$group)=="LSD"){
    dc$pre_control[5:7]=1
    dc$pre_sleepdep[12:14]=1
  } else {
    dc$pre_control[15:17]=1
    dc$pre_sleepdep[8:10]=1
  }
  
  #' subj 31 was missing SR for last 3 nights of normal sleep, substitute with
  #' previous 4 nights
  if(subj=="031"){
    dc$pre_control[11:17]=1
  }
  
  tibble(subj=subj, dc)
}) |> list_rbind()

rm(fnames)
rm(read_xlsx)
rm(read_xlsx2)