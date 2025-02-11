fnames=list.files("data/behaviour", pattern="*.csv", full.names = T) 
fnames=fnames[str_detect(fnames, "day")]

data <- map_df(fnames, \(fname){
  print(fname)
  read_csv(fname, comment = "#", col_types = cols(
    subj = col_character(),
    trial = col_double(),
    time = col_double(),
    stimulus = col_character(),
    response = col_character())
  ) |> 
    mutate(day = str_split(fname, "[/_]")[[1]][4])
})

# add probenum variable
data <- 
  data %>% 
  group_by(subj,day) %>% 
  do({
    d=.
    dd=d[d$stimulus=="probe1",]
    dd$trial[-length(dd$trial)]
    dd$trial1=c(0,dd$trial[-length(dd$trial)])
    d$probenum=0
    for(i in 1:dim(dd)[1]){
      d$probenum[d$trial>dd$trial1[i] & d$trial<=dd$trial[i]]<-i
    }
    d
})

data <- 
  data |> 
  left_join(
    demographics |> select(subj, group), 
    by="subj"
  ) |>
  mutate(
    sleepdep = case_when(
      group=="ESD" & day=="day1" ~ "SD",
      group=="ESD" & day=="day2" ~ "control",
      group=="LSD" & day=="day1" ~ "control",
      group=="LSD" & day=="day2" ~ "SD",
      T ~ "unddef"),
  ) 

rm(fnames)