# specific functions for a specific use case

clock_24 <- function(data){
  map(data, \(d){
    if(is.na(d)){
      NA
    } else {
      d2 <- str_split(d, "\\.")[[1]][1]
      if( str_length(d2[1]) == 1) d2[1] <- paste0("0", d2[1])
      d3 <- str_split( round((as.numeric(d) - as.numeric(d2)) * .6, digits = 2), "\\.")[[1]][2]
      if(is.na(d3)) d3 <- "00" # if it does not exist, assume it is 00
      if(str_length(d3)<2) d3 <- paste0(d3,"0") # if length is not 2, assume we are missing 0
      str_glue("{d2}:{d3}")
    }
  }) |> list_c()
}

clock_h_m <- function(data, rm_0h = TRUE){
  map(data, \(d){
    is_negative <- ""
    d2 <- str_split(d, "\\.")[[1]][1]
    if( str_starts(d2, "-") ) is_negative <- "-"
    d3 <- str_split( round((as.numeric(d) - as.numeric(d2)) * .6, digits=2), "\\.")[[1]][2]
    if(str_starts(d3, "0") & !is.na(d3)) d3 <- str_remove(d3, "0")
    if(is.na(d3)) d3 <- "0"
    
    if(rm_0h & (d2 == 0 | d2 == "-0")){
      sprintf("%s%sm", is_negative, d3)
    } else {
      str_glue("{d2}h {d3}m")
    }
  }) |> list_c()
}
