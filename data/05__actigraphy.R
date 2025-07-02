fnames = list.files(
  paste0(relative_path, "data/actigraphy"), pattern="*.csv", full.names = T) 
subjs = map_chr(fnames, ~str_split(.x, "/") |> pluck(1) |> pluck(-1) |>
                  str_split("_") |> pluck(1) |> pluck(1)
                ) |> unique() 

as_hour_time <- function(stime){
  map_dbl(str_split(stime,":"), \(x){
    h <- as.integer(x[1])
    if(str_detect(x[3], "PM")){ # all PM times get +12
      h <- h+12
    }
    if(str_detect(x[3], "AM")){ # for AM, only the 12 gets set to 0
      if(h==12)
        h=0
    }
    h <- ifelse(h<8, h+24, h)
    m <- as.integer(x[2])
    if(str_detect(x[3], "M")){
      s <- as.integer(str_split(x[3], " ")[[1]][1])
    } else {
      s <- as.integer(x[3])
    }
    h + m/60 + s/60/60
  })
}

# wake is different?
as_hour_time_wake <- function(stime){ 
  map_dbl(str_split(stime,":"), \(x){
    h <- as.integer(x[1])
    if(str_detect(x[3], "PM")){ # all PM times get +12
      h <- h+12
    }
    
    m <- as.integer(x[2])
    if(str_detect(x[3], "M")){
      s <- as.integer(str_split(x[3], " ")[[1]][1])
    } else {
      s <- as.integer(x[3])
    }
    h + m/60 + s/60/60
  })
} 
# 19 special --------------------------------------------- --
h_slp_19 <- function(stime){ # sleep onset
  map_dbl(str_split(stime,":"), \(x){
    # Subj 19 seems to have a 12 hours discrepancy between sleep onset & wake
    h <- as.numeric(x[1])+12 # hour
    #' + 12 to correct for "early" sleep. 
    m <- as.numeric(x[2])/60 # min
    s <- as.numeric(x[3])/60/60 # sec
    h + m + s
  })
}

h_wk_19 <- function(stime){ # wake 
  map_dbl(str_split(stime,":"), \(x){
    # Subj 19 seems to have a 12 hours discrepancy between sleep onset & wake
    h <- as.numeric(x[1])-12 # hour
    #' -12 to correct for "late wake"
    m <- as.numeric(x[2])/60 # min
    s <- as.numeric(x[3])/60/60 # sec
    h + m + s
  })
}

##' combine files per subj because some files have duplicate days
actigraphy <- map(subjs, \(subj){
  # fnames=list.files(
  #   paste0(relative_path, "data/actigraphy"),
  #   pattern=sprintf("%s_.*.csv",subj), full.names = T)
  
  r_fname <- fnames[str_detect(fnames, sprintf("/%s", subj))]

  map_df(r_fname, \(fname){
    print(fname)
    d <- suppressMessages(
      read_csv(fname, locale = readr::locale(encoding = "UTF-16"), skip=64,col_types = cols(.default = "c"))) 
    ix <- which(d[,1]=="--------------------- Marker/Score List --------------------")
    
    ## sometimes the "Marker/Score list" thing is missing
    if(length(ix)==0){
      ix=dim(d)[1]}
    d <- d[1:(ix-1),]
    d |> filter(!is.na(`Interval Type`)) |>
      filter(!str_detect(`Interval Type`, ".*Summary.*")) |>
      filter(`Interval Type` %in% c("DAILY", "SLEEP")) |>
      select(1:15) |> 
      mutate(subj=subj)
  }) -> dd
  
  dd <- distinct(dd) |> filter(`Interval Type`=="SLEEP")
  
  #' there are two different date formats:
  #'  yyyy-mm-dd
  #'  mm/dd/yyyy
  #'  
  dd |>     #rename(date=`End Date`) |>
    mutate(
    #' It appears as though participant 17 has flipped AM/PM 
    #' (seems like 12h change in sleep/wake timings)?
    #'  - [ ]
    #' 19 has 12 h change in sleep/wake timings
    #'  - [x] 
      date_start_ag = if_else(str_detect(`Start Date`, "/"), mdy(`Start Date`), ymd(`Start Date`)),
        #' Using "ifelse" does NOT give out date formate. 
      sleep_onset_ag_h = if_else(subj=="019", h_slp_19(`Start Time`), as_hour_time(`Start Time`)),
      date_end_ag = if_else(str_detect(`End Date`, "/"), mdy(`End Date`), ymd(`End Date`)),
      sleep_wake_ag_h = if_else(subj=="019", h_wk_19(`End Time`), as_hour_time_wake(`End Time`)), 
        #' Added wake time to investigate self-report vs. actigraphy
      full_sleep_duration_ag_h = as.numeric(Duration)/60, 
       #' There is something called "duration" and something called 
       #' "sleep time". I believe "sleep time" is the amount of sleep 
       #' subtracting the estimated time awake that participants had during 
       #' the night.
      sleep_wake_time_ag_h = as.numeric(`Wake Time`)/60,  
       #' Measure of amount of time awake during the night.
      sleep_pct_wake_ag_h = as.numeric(`%Wake`),
        #' Percentage of wake (during sleep)
      sleep_duration_ag_h = as.numeric(`Sleep Time`)/60,
        # Sleep amount minus wake (during sleep) 
      sleep_pct_duration_ag_h = as.numeric(`%Sleep`),
        #' Percentage of sleep
      sleep_efficiency = as.numeric(Efficiency),
       #' A score of sleep efficiency (sleep time / wake time * 100 [I think]) 
     ) |>
    select(subj,date_start_ag:sleep_efficiency) #contains("sleep", ignore.case=F))|> View()
}) |> list_rbind()


# clear:
rm(fnames)
rm(subjs)
rm(as_hour_time)
rm(as_hour_time_wake)
rm(h_slp_19)
rm(h_wk_19)
