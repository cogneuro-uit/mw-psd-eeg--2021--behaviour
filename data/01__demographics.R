fnames = list.files("data/baseline+diary", pattern="*.xlsx", full.names = T)
fnames = fnames[!str_detect(fnames, "~")] 


read_xlsx <- function(...){
  suppressMessages(readxl::read_xlsx(...))
}

demographics <- map(fnames, \(fname){
  print(fname)
  
  datasheet <- read_xlsx(fname, sheet="DataSheet")
  
  datasheet |>
    select(...2, ...3, VALUE, ...5 ) |> 
    rename(var = ...2, value = VALUE) |>
    mutate(
      filter = case_when(
        row_number() %in% 1:13 ~ T,
        lag(str_starts(var, "No ")) ~ T,
        lag(lag(str_starts(var, "No "))) ~ T,
        )
      ) |> 
    fill(var) |>
    filter(filter) |>
    mutate(
      value = as.character(value),
      var = if_else(!is.na(...3), paste0(...3, "_", var), var),
      var = str_replace_all(var, "[()]", "") |>
        str_replace_all("/", ".") |>
        str_replace_all(" ", "_") |>
        str_replace_all("^T", "S"),
      value = if_else(str_ends(var, "_daylight"), 
                      sprintf("%2s:%2s", value, ...5),
                      value),
   ) |>
    select(var, value) |>
    pivot_wider(names_from = var, values_from = value) |>
    rename(subj = Code, group = Group) |>
    mutate(
      subj = sprintf("%03i", as.numeric(subj)),
      group = case_when(group=="0" ~ "ESD", group=="1" ~ "LSD", T ~ "NA"), 
      S1_date = read_xlsx(fname, sheet="DataSheet", range="D7", col_names = F) |> pull(`...1`),
      S2_date = read_xlsx(fname, sheet="DataSheet", range="D9", col_names = F) |> pull(`...1`),
      across( contains("No_"), ~if_else(.x=="1", T, F) ),
      across( ends_with("_daylight"), ~hm(.x)),
      S1_cond = case_when(
        group == "ESD" ~ "SD",
        group == "LSD" ~ "control",
        T ~ NA),
      S2_cond = case_when(
        group == "ESD" ~ "control",
        group == "LSD" ~ "SD",
        T ~ NA),
      )
}) |> list_rbind()

rm(fnames)
rm(read_xlsx)
