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

sleep_quiz <- map(fnames, \(fname) {
  print(fname)
  
  ##' section 1: Generell sovn
  q1 <- read_xlsx2(fname, sheet="Sleep Quiz", range="B3", col_names = F) |> 
    pull(`...1`)
  
  read_xlsx(fname, sheet="Sleep Quiz", range="B5:C6", col_names = F) |>
    mutate(hms=sprintf("%i:%i:00", `...1`, `...2`)) |>
    pull(hms) -> q23
  
  read_xlsx(fname, sheet="Sleep Quiz", range="B7:B9", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S1_Q%i", 4:6)) |>
    spread(Q, val) -> q46
  
  read_xlsx(fname, sheet="Sleep Quiz", range="B11:B12", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S1_Q%i", 7:8)) |>
    spread(Q, val) -> q78
  
  ##' section 2: Fatique
  read_xlsx(fname, sheet="Sleep Quiz", range="F3:F11", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S2_Q%i", 1:9)) |>
    spread(Q, val) -> s2q19
  
  ##' section 3: Sleepiness
  read_xlsx(fname, sheet="Sleep Quiz", range="I3:I10", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S3_Q%i", 1:8)) |>
    spread(Q, val) -> s3q18
  
  ##' section 4: Sleep (counting questions 1-N instead of Q1.1 etc)
  read_xlsx(fname, sheet="Sleep Quiz", range="L3:L9", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S4_Q%i", 1:7)) |>
    spread(Q, val) -> s4q17
  
  read_xlsx(fname, sheet="Sleep Quiz", range="O3:P3", col_names = F) |>
    mutate(hms=sprintf("%i:%i:00", `...1`, `...2`)) |>
    pull(hms) -> s4q8
  
  s4q9 <- read_xlsx2(fname, sheet="Sleep Quiz", range="O5", col_names = F) |> 
    pull(`...1`)
  
  read_xlsx(fname, sheet="Sleep Quiz", range="O7:P8", col_names = F) |>
    replace_na(list(`...2`=0)) |>
    mutate(val=sprintf("%i:%i:00", `...1`, `...2`), Q=sprintf("S4_Q%i", 10:11)) |>
    select(Q,val) |> spread(Q, val) -> s4q1011
  
  read_xlsx(fname, sheet="Sleep Quiz", range="O10:O23", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S4_Q%i", 12:25)) |>
    spread(Q, val) -> s4q1225
  
  ##' section 5: day rhythm
  read_xlsx(fname, sheet="Sleep Quiz", range="U3:U9", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S5_Q%i", 1:7)) |>
    spread(Q, val) -> s5q17
  
  ##' section 6: PANAS retrospective from T0
  read_xlsx(fname, sheet="Sleep Quiz", range="Y3:Y22", col_names = F) |>
    rename(val=`...1`) |> mutate(Q=sprintf("S6_Q%i", 1:20)) |>
    spread(Q, val) -> s6q120
  
  ##' section 7: Alcohol
  read_xlsx(fname, sheet="Sleep Quiz", range="AB3:AC5", col_names = F, na="NA") |>
    rename(val=`...2`) |> mutate(Q=sprintf("S7_Q%i", 1:3)) |> 
    select(-`...1`) |>
    spread(Q, val) -> s7q13
  
  ##' Putting it together
  tibble(
    subj = str_split(fname, "[/_]")[[1]][3],
    S1_Q1 = q1, S1_Q2 = hms(q23[1]), S1_Q3 = hms(q23[2])
  ) |>
    bind_cols(
      q46, q78, s2q19, s3q18, s4q17, S4_Q8=hms(s4q8), S4_Q9=s4q9, 
      S4_Q10 = hms(s4q1011[1]), S4_Q11=hms(s4q1011[2]) , s4q1225,
      s5q17, s6q120, s7q13)
  
}) |> list_rbind()

rm(fnames)
rm(read_xlsx)
rm(read_xlsx2)