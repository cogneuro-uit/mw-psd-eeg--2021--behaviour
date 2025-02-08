#' this is the PANAS pre and post each session
#'
fnames=list.files("data/baseline+diary/", pattern="*.xlsx", full.names = T) 
fnames=fnames[!str_detect(fnames, "~")]

read_xlsx <- function(...){
  suppressMessages(readxl::read_xlsx(...))
}

pos.items <- c("Q1", "Q3", "Q5", "Q9", "Q10", "Q12", "Q14", "Q16", "Q17", "Q19")
read_sheet <- function(fname) {
  print(fname)
  subj = str_split(fname, "[/_]")[[1]][3]
  
  sd.pre  <- readxl::read_xlsx(fname, sheet="PANAS", range="A3:B23", col_names = T, na = "NA") |> mutate(sleepdep="SD", prepost="pre")
  sd.post <- readxl::read_xlsx(fname, sheet="PANAS", range="E3:F23", col_names = T, na = "NA") |> mutate(sleepdep="SD", prepost="post") 
  ns.pre  <- readxl::read_xlsx(fname, sheet="PANAS", range="I3:J23", col_names = T, na = "NA") |> mutate(sleepdep="control", prepost="pre")
  ns.post <- readxl::read_xlsx(fname, sheet="PANAS", range="M3:N23", col_names = T, na = "NA") |> mutate(sleepdep="control", prepost="post")
  
  tibble(subj=subj, bind_rows(sd.pre, sd.post, ns.pre, ns.post)) |>
    mutate(PANASposneg=ifelse(PANAS %in% pos.items, "pos", "neg")) |>
    group_by(subj, sleepdep, prepost, PANASposneg) |>
    reframe( 
      PANASsum = unique( sum(Value, na.rm=T) ),
      PANASsum = ifelse(PANASsum<5, NA, PANASsum) 
    )
}

panas_session <- map_df(fnames, read_sheet)
