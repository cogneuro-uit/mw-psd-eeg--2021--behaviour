save_output_cnd <- function(data, name){
  #' Save tabel and figure locally
  #' 
  #' A handy function to save ggplots and gttables with minimal effort. 
  #' Options are set at the project level, such that the function requires only 
  #' applying a saving name. 
  
  #' could add "..., force_diff = FALSE"
  class <- class(data) 
  if( any( class %in% c("gg", " ggplot", "gt_tbl") ) ){
    type <- ""
    
    # Set/get saving parameters 
    if( !exists("script_relative_path")) script_relative_path <- ""
    # ggplot
    if( any( class %in% c("gg", "ggplot") ) ){
      if( getOption("project_save_figs_to_file") ){
        type <- "fig"
        fmts <- getOption("project_save_figs_formats")
        base <- getOption("project_save_figs_location")
        cond <- NULL
        if( getOption("project_save_figs_with_date_time") ) cond <- getOption("project_date_time")
      }
    }
    # gt
    if( any( class %in% c("gt_tbl") ) ){
      if( getOption("project_save_tbls_to_file") ){
        type <- "tbl"
        fmts <- getOption("project_save_tbls_formats")
        base <- getOption("project_save_tbls_location")
        cond <- NULL
        if( getOption("project_save_tbls_with_date_time") ) cond <- getOption("project_date_time")
      } 
    }
    
    filename <- str_glue(script_relative_path, "{base}{name}{cond}")
    
    # Save across....
    walk(fmts, \(fmt){
      message("Saving object: ", type, " with format: ", fmt)
      if(type == "fig") ggsave(paste0(filename, fmt), data)
      if(type == "tbl") gtsave(data, paste0(filename, fmt))
    })
    message("Done!")
  } else { warning("No valid format found, skipping saving...") }
  data
}