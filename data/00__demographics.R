fnames = list.files("data/baseline+diary", pattern="*.xlsx", full.names = T)
fnames = fnames[!str_detect(fnames, "~")]


read_xlsx <- function(...){
  suppressMessages(readxl::read_xlsx(...))
}

read_sheet <- function(fname) {
  
  datasheet <- read_xlsx(fname, sheet="DataSheet")
  
  read_xlsx(fname, sheet="DataSheet", range="B2:D6", col_names = F) |>
    setNames(c("var","drop","val")) |> select(-drop) |> spread(var,val) |>
    mutate(subj=sprintf("%03i",Code)) |> select(-Code) |>
    mutate(group=case_when(Group=="0" ~ "ESD",
                           Group=="1" ~ "LSD",
                           T ~ "NA")) |> select(-Group) -> d
  t1date <- read_xlsx(fname, sheet="DataSheet", range="D7", col_names = F) |> pull(`...1`)
  t2date <- read_xlsx(fname, sheet="DataSheet", range="D9", col_names = F) |> pull(`...1`)
  t1daylight <- read_xlsx(fname, sheet="DataSheet", range="D8:E8", col_names = F) |>
    mutate(val=sprintf("%i:%i:00", `...1`, `...2`)) |> pull(val)
  t2daylight <- read_xlsx(fname, sheet="DataSheet", range="D10:E10", col_names = F) |>
    mutate(val=sprintf("%i:%i:00", `...1`, `...2`)) |> pull(val)
  
  instruments <- read_xlsx(fname, sheet="DataSheet", range="B13:D14", col_names = F, col_types="text") |>
    setNames(c("var","drop","val")) |> select(-drop) |> spread(var,val)
  
  d |> bind_cols(t1date=t1date, t2date=t2date, t1daylight=hms(t1daylight),
                 t2daylight=hms(t2daylight), instruments)

}

demographics <- map_df(fnames, read_sheet)

rm(fnames)