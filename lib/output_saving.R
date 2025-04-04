#' Specific functions for conditional saving of figures and tables 

condition_save_figure <- function(plot, name, ..., .suppressMessages = FALSE, .relative_path = NULL){
  #' Save ggplot figures locally
  #' 
  #' A handy function to save ggplots with minimal effort. 
  #' Options are set at the project level, such that the function requires only 
  #' applying a saving name.
  
  require(tidyverse)
  require(ggplot2)
  
  # Check whether the data has the correct data structure to be saved...
  class <- class(plot) 
  
  if(any(class %in% c("gg", "ggplot"))){
    if(!.suppressMessages) message("Valid figure structure found, proceeding...")
    
    # Get settings
    save_figure <- getOption("project_save_figs_to_file")
    
    # Check whether we should save anything
    if(save_figure){
      if(!.suppressMessages) message("Saving figure...") 
      
      fmts <- getOption("project_save_figs_formats")
      
      if(!(any( fmts %in% c(".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png", ".bmp", ".svg")) )){
        warning("Figure format(s) not valid. Saving as .png") 
        fmts <- ".png"
      }
      
      base <- getOption("project_save_figs_location")
      cond <- NULL
      if(getOption("project_save_figs_with_date_time")) cond <- getOption("project_date_time")
      
      # Check relative saving parameter as "relative_path".
      if(!exists("relative_path") & is.null(.relative_path)) relative_path <- ""
      if(!is.null(.relative_path)) relative_path <- .relative_path
      
      # Set filename
      filename <- paste0(relative_path, base, name, cond)
      message("Saving files to:", filename, "\n")
      
      # Save across formats
      walk(fmts, \(fmt){
        if(!.suppressMessages) message("Saving figure with format: ", fmt)
        ggsave(paste0(filename, fmt), plot, ...)
      })
      
      if(!.suppressMessages) message("Done!")
    } else { 
      if(!.suppressMessages) message("Figure saving disabled, skipping...") 
    }
  } else {
    warning("Data is not a ggplot object. Cannot save as figure.")
  }
  plot
}

condition_save_table <- function(table, name, ..., .suppressMessages = FALSE, .relative_path = NULL, .suppress_print = FALSE){
  #' Save gt tables locally
  #' 
  #' A handy function to save gt tables with minimal effort. 
  #' Options are set at the project level, such that the function requires only 
  #' applying a saving name.
  
  require(tidyverse)
  require(gt)
  
  # Check whether the data has the correct data structure to be saved...
  class <- class(table) 
  
  if(any(class %in% c("gt_tbl"))){
    if(!.suppressMessages) message("Valid table structure found, proceeding...")
    
    # Get settings
    save_table <- getOption("project_save_tbls_to_file")
    
    # Check whether we should save anything
    if(save_table){
      if(!.suppressMessages) message("Saving table...") 
      
      fmts <- getOption("project_save_tbls_formats")
      
      if(!(any( fmts %in% c(".html", ".tex", ".ltx", ".rtf", ".docx")) )){
        warning("Table format not valid, setting to .rtf...")
        fmts <- ".rtf"
      }
      
      base <- getOption("project_save_tbls_location")
      cond <- NULL
      if(getOption("project_save_tbls_with_date_time")) cond <- getOption("project_date_time")
      
      # Check relative saving parameter as "relative_path".
      if(!exists("relative_path") & is.null(.relative_path)) relative_path <- ""
      if(!is.null(.relative_path)) relative_path <- .relative_path
      
      # Set filename
      filename <- paste0(relative_path, base, name, cond)
      message("Saving files to:", filename, "\n")
      
      # Save across formats
      walk(fmts, \(fmt){
        if(!.suppressMessages) message("Saving table with format: ", fmt)
        gtsave(table, paste0(filename, fmt), ...)
      })
      
      if(!.suppressMessages) message("Done!")
    } else { 
      if(!.suppressMessages) message("Table saving disabled, skipping...") 
    }
  } else {
    warning("Data is not a gt_tbl object. Cannot save as table.")
  }
  
  if(!.suppress_print){
    table
  }
}



#' save_output_cnd <- function(data, name, relative_path = "",  ..., .suppressMessages = FALSE, .suppress_print = FALSE){
#'   #' Save tabel and figure locally
#'   #' 
#'   #' A handy function to save ggplots and gttables with minimal effort. 
#'   #' Options are set at the project level, such that the function requires only 
#'   #' applying a saving name. 
#'   
#'   #' could add "..., force_diff = FALSE"
#'   
#'   require(tidyverse)
#'   require(gt)
#'   require(ggplot2)
#'   
#'   # Check whether the data has the correct data structure to be saved...
#'   class <- class(data) 
#'   
#'   if( any( class %in% c("gg", " ggplot", "gt_tbl") ) ){
#'     if( !.suppressMessages )  message("Valid data structure found, proceeding...")
#'     
#'     # Get settings
#'     save_figure <- getOption("project_save_figs_to_file")
#'     save_table  <- getOption("project_save_tbls_to_file")
#'     
#'     #' Check whether we should save anything and *get* the respective *parameters*
#'     if( any( save_figure, save_table) ){
#'       # FIGURE
#'       if( save_figure & any( class %in% c("gg", "ggplot") ) ){
#'         if( !.suppressMessages )   message("Saving figure...") 
#'         type <- "fig"
#'         fmts <- getOption("project_save_figs_formats")
#'         
#'         if( !( any( fmts %in% c(".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png", ".bmp", ".svg") ) ) ){
#'           warning("Figure format not valid, using default .png...") 
#'           fmts <- ".png"
#'         }
#'         base <- getOption("project_save_figs_location")
#'         cond <- NULL
#'         if( getOption("project_save_figs_with_date_time") )   cond <- getOption("project_date_time")
#'       }
#'       
#'       # TABLE 
#'       if( save_table & any( class %in% c("gt_tbl") ) ){
#'         if( !.suppressMessages )   message("Saving table...")
#'         
#'         type <- "tbl"
#'         fmts <- getOption("project_save_tbls_formats")
#'         if( !( any( fmts %in% c(".html", ".tex", ".ltx", ".rtf", ".docx") ) ) ){
#'           warning("Table format not valid, setting to .rtf...")
#'           fmts <- ".rtf"
#'         }
#'         base <- getOption("project_save_tbls_location")
#'         cond <- NULL
#'         if( getOption("project_save_tbls_with_date_time") )   cond <- getOption("project_date_time")
#'       }
#'       
#'       print(relative_path)
#'       # Check relative saving parameter as "relative_path".
#'       if( !exists("relative_path") & is.null(.relative_path) ) relative_path <- ""
#'       if( !is.null(.relative_path) ) relative_path <- .relative_path
#'       
#'       # Set filname
#'       filename <- paste0(relative_path, base, name, cond)
#'       message("Saving files to:", filename, "\n")
#'       
#'       # Save across....
#'       walk(fmts, \(fmt){
#'         if( !.suppressMessages ) message("Saving object: ", type, " with format: ", fmt)
#'         if(type == "fig")        ggsave(paste0(filename, fmt), data, ...)
#'         if(type == "tbl")        gtsave(data, paste0(filename, fmt))
#'       })
#'       
#'       if( !.suppressMessages )  message("Done!")
#'     } else { 
#'       if( !.suppressMessages )  message("Saving disabled, skipping...") 
#'     }
#'   }
#'   
#'   if( !.suppress_print ){
#'     data
#'   }
#' }

