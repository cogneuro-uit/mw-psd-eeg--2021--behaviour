# Format numbers to APA standard
fmt_APA_numbers <- function(x, .p = FALSE, .psym = FALSE, .low_val = FALSE, .chr = FALSE){
  require(purrr)
  
  purrr::map(x, \(num){
    o_num <- num
    # Store original value
    num <- as.numeric(num)
    
    # Transform to numeric (if possible)
    if(is.na(num)){
      return(o_num)
      # if is not numeric after transformation, return original
    }
    
    # p with symbol
    # = symbol:
    if(.psym & !.p){
      if(num == 1){
        return("= 1.00")
      }
      if(num < .001){
        return("< .001")
      } 
      if( num > .999 ){
        return("> .999")
      }
      return( round(num, 3) |> as.character() |> str_replace("0.", "= .") )
    }
    
    # < than symbol
    if(.p){
      if( num == 1 ){
        return( "1.00" )
      }
      if( num > .999 ){
        return( ".999")
      }
      if(num < .001){ 
        return( ".001" )
      } else {
        num <- num |> round(digits = 3) |> as.character() |> str_replace("0.", ".")
        if(str_length(num) <= 3){
          for(x in 1:(4 - str_length( num)) ){
            num <- paste0(num, "0")
          }
        }
      }
      return( num ) 
    }
    
    # > 100
    if(num >= 100 | num <= -100){
      num <- round(num, 0)
      if(.chr){
        return( as.character( num) ) 
      }
      return( num )
    }
    
    # > 10
    if(num >= 10 | num <= -10){
      num <- round(num, 1)
      # Add an extra 0 using strings to return tidy APA numbers 
      if(.chr){
        if(str_length( as.character( abs(num) ) ) == 2){
          num <- paste0(num, ".0") # if exactly two, always add .0)
        }
        return( as.character(num) )
      }
    }
    
    # > 1  | > 1 & .low_val
    if(num >= 1 | num <= -1 | num < 1 & !.low_val | num > -1 & !.low_val){
      num <- round(num, 2)
      
      if(.chr){
        if(str_length( as.character( abs(num) ) ) <= 3){
          # Add an extra 0 using strings to return tidy APA numbers 
          for(x in 1:(4 - str_length( as.character( abs(num) ) )) ){
            if(str_detect(num,"\\.", negate = TRUE)){
              num <- paste0(num, ".")
            } else{
              num <- paste0(num, "0")
            }
          }
        }
        return( as.character(num) )
      }
    }
    
    
    # Low values 
    if(num < 1 & .low_val | num > -1 & .low_val){
      num <- round(num, 3)
      if(.chr){
        if(str_length( as.character( abs(num) ) ) <= 4){
        # Add an extra 0 using strings to return tidy APA numbers 
          for(x in 1:(5 - str_length( as.character( abs(num) ) )) ){
            if(str_detect(num,"\\.", negate = TRUE)){
              num <- paste0(num, ".")
            } else {
              num <- paste0(num, "0")
            }
          }
        }
        return( as.character(num) )
      }
    }
    
    return( num )
  }) |> unlist()
  
}


tab_fmt_APA <- function(data){
  require(gt)
  #' Supplementary function to quickly transform a GT table to an approxiate APA 7 format
  #' 
  
  tab_line_width <- 1
  tab_line_colour <- "black"
  
  data |>
    tab_style(cell_text(size = 12, align="left"), cells_title()) |>
    tab_options(
      # General TABLE params
      table.background.color = "white",
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table_body.border.bottom.color = tab_line_colour,
      table.width = pct(100),
      # Column label things
      column_labels.border.top.width = tab_line_width, # +1 ?
      column_labels.border.top.color = tab_line_colour,
      column_labels.border.bottom.width = tab_line_width,
      column_labels.border.bottom.color = tab_line_colour,
      # ROW groups (if relevant)
      row_group.border.bottom.color = tab_line_colour,
      row_group.border.bottom.width = tab_line_width,
      row_group.border.top.color = tab_line_colour,
      row_group.border.top.width = tab_line_width,
    ) |>
    # set table style
    tab_style(
      style = list(
        # remove horizontal lines
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        # remove row striping in Markdown documents
        cell_fill(color = "white", alpha = NULL)
      ),
      # do this for all columns and rows
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))
}
