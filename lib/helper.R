save_output_cnd <- function(data, name, suppressMessages = FALSE, relative_path = NULL){
  #' Save tabel and figure locally
  #' 
  #' A handy function to save ggplots and gttables with minimal effort. 
  #' Options are set at the project level, such that the function requires only 
  #' applying a saving name. 
  
  #' could add "..., force_diff = FALSE"
  
  require(tidyverse)
  require(gt)
  require(ggplot)
  
  # Check whether the data has the correct data structure to be saved...
  class <- class(data) 
  
  if( any( class %in% c("gg", " ggplot", "gt_tbl") ) ){
    if( !suppressMessages )  message("Valid data structure found, proceeding...")
    
    # Get settings
    save_figure <- getOption("project_save_figs_to_file")
    save_table  <- getOption("project_save_tbls_to_file")
    
    #' Check whether we should save anything and *get* the respective *parameters*
    if( any( save_figure, save_table) ){
      # FIGURE
      if( save_figure & any( class %in% c("gg", "ggplot") ) ){
        if( !suppressMessages )   message("Saving figure...") 
        type <- "fig"
        fmts <- getOption("project_save_figs_formats")
        
        if( !( any( fmts %in% c(".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png", ".bmp", ".svg") ) ) ){
          warning("Figure format not valid, using default .png...") 
          fmts <- ".png"
        }
        base <- getOption("project_save_figs_location")
        cond <- NULL
        if( getOption("project_save_figs_with_date_time") )   cond <- getOption("project_date_time")
      }
      
      # TABLE 
      if( save_table & any( class %in% c("gt_tbl") ) ){
        if( !suppressMessages )   message("Saving table...")
        
        type <- "tbl"
        fmts <- getOption("project_save_tbls_formats")
        if( !( any( fmts %in% c(".html", ".tex", ".ltx", ".rtf", ".docx") ) ) ){
          warning("Table format not valid, setting to .rtf...")
          fmts <- ".rtf"
        }
        base <- getOption("project_save_tbls_location")
        cond <- NULL
        if( getOption("project_save_tbls_with_date_time") )   cond <- getOption("project_date_time")
      }
      
      # Check relative saving parameter as "script_relative_path".
      if( !exists("script_relative_path") & is.null(relative_path) ) script_relative_path <- ""
      if( !is.null(relative_path) ) script_relative_path <- relative_path
      
      # Set filname
      filename <- paste0(script_relative_path, base, name, cond)
      message("Saving files to:", filename, "\n")
      
      # Save across....
      walk(fmts, \(fmt){
        if( !suppressMessages )   message("Saving object: ", type, " with format: ", fmt)
        if(type == "fig") ggsave(paste0(filename, fmt), data)
        if(type == "tbl") gtsave(data, paste0(filename, fmt))
      })
      
      if( !suppressMessages )  message("Done!")
    } else { 
      if( !suppressMessages )  message("Saving disabled, skipping...") 
    }
  }
  data
}



clock_24 <- function(data){
  map(data, \(d){
    d2 <- str_split(d, "\\.")[[1]][1]
    if( str_length(d2[1]) == 1) d2[1] <- paste0("0", d2[1])
    d3 <- str_split( round((as.numeric(d) - as.numeric(d2)) * .6, digits = 2), "\\.")[[1]][2]
    if(is.na(d3)) d3 <- "00"
    str_glue("{d2}:{d3}")
  }) |> list_c()
}

clock_h_m <- function(data){
  map(data, \(d){
    d2 <- str_split(d, "\\.")[[1]][1]
    d3 <- str_split( round((as.numeric(d) - as.numeric(d2)) * .6, digits=2), "\\.")[[1]][2]
    if(str_starts(d3, "0") & !is.na(d3)) d3 <- str_remove(d3, "0")
    if(is.na(d3)) d3 <- "0"
    str_glue("{d2}h {d3}m")
  }) |> list_c()
}

