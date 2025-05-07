#' A diverse set of function related to bayesian diagnostics and reporting.

rep_bayes <- function(bayes_vec, calc = "m"){
  switch(
    calc
    , m   = rep_bayes_m(bayes_vec)
    , hdi = rep_bayes_hdi(bayes_vec)
    , er  = rep_bayes_er(bayes_vec)
    , p   = rep_bayes_p(bayes_vec)
  )
}

rep_bayes_m <- function(bayes_vec, ...){
  mean(bayes_vec) |> fmt_APA_numbers(.chr=T, ...)
}

rep_bayes_hdi <- function(bayes_vec, ...){
  #' Get the highest density interval for a posterior vector.
  #'
  require(tidyverse)
  require(tidybayes)
  
  bayes_vec |>
    tidybayes::hdi() |>
    tibble::as_tibble() |>
    dplyr::mutate( dplyr::across(tidyselect::everything(), ~fmt_APA_numbers( .x, .chr=T, ... )) ) |>
    stringr::str_glue_data("[{V1}, {V2}]") |>
    as.character()
}

rep_bayes_er <- function(bayes_vec, ...){
  #' Get the evidence ratio that the posterior coefficient is in the beta specified direction.
  #'
  
  mean <- mean(bayes_vec)
  
  fmt_APA_numbers( ifelse(
    mean > 0
    , sum(bayes_vec > 0) / sum(bayes_vec <= 0)
    , sum(bayes_vec < 0) / sum(bayes_vec >= 0)
  ), .chr=T, ... )
}

rep_bayes_p <- function(bayes_vec, ...){
  #' Get the probability that the vector is in the (mean) specified direction.
  #'
  
  mean <- mean(bayes_vec)
  
  fmt_APA_numbers( ifelse(
    mean > 0
    , sum(bayes_vec > 0) / length(bayes_vec)
    , sum(bayes_vec < 0) / length(bayes_vec)
  ), .chr=T, ...)
}

rep_bayes_coef <- function(x, .rep = "simple", ..., .preserve_negative = TRUE){
  #' Produce a simple text output of a Bayesian posterior vector
  
  .rep2 <- str_to_lower(.rep)
  
  if( .rep2 %in% c("text", "txt", "tx", "long" ) ){
    .rep2 <- "long"
  } 
  if( .rep2 %in% c("df","data", "frm", "dataframe", "data.frame", "tibble", "frame") ){
    .rep2 <- "df"
  } 
  
  # calc
  mean                     <- mean(x)
  hdi                      <- rep_bayes_hdi(x, ...)
  evidence_ratio_direction <- rep_bayes_er(x, ...)
  probability_direction    <- rep_bayes_p(x, ..., .psym = T)
  
  # preserve negative if rounding turns very low
  if(.preserve_negative & mean < 0 & round(mean, 2)==0){
    mean_fix <- paste0("-", fmt_APA_numbers(mean, .chr=T, ...))
  } else {
    mean_fix <- mean |> fmt_APA_numbers(.chr = T, ...)
  }
  
  # Present infinity if infinity
  if( is.infinite(evidence_ratio_direction) ){
    evidence_ratio_direction = if_else(evidence_ratio_direction < 0, "-\\infty", "\\infty")
  }
  
  ####    RETURN    #### 
  #' If dataframe specified, return data frame.
  if( .rep == "df" ){return( tibble::tibble(
    m = mean_fix, hdi = hdi, er = evidence_ratio_direction, p = probability_direction
    ))
  }
  
  #' Otherwise, return text version
  sprintf(
    switch(
      .rep2
      , simple = "b = %s, %s, ER = %s, p %s"
      , long = "$b = %s$, %s, $\\text{ER}_\\text{dir} = %s$, $p_\\text{dir} %s$",
    )
    , mean_fix, hdi, evidence_ratio_direction, probability_direction
  )
}


bayes_coef_plot <- function( bayes_model, add_label = TRUE, offset = 0){
  #' Generate a Bayesian coefficient plot.
  
  require(brms)
  require(bayesplot)
  
  coefficients <- paste0("b_", bayes_model |> fixef() |> rownames())
  
  # Generate main plot
  p <- bayesplot::mcmc_intervals( bayes_model, coefficients, prob_outer = .95) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = .4) +
    ggplot2::labs(title = paste( "Dependent variable:",  bayes_model[["formula"]][["formula"]][[2]] ) )
  
  # Get offset for each variable
  calc_offset <-
    bayes_model |>
    tibble::as_tibble() |>
    dplyr::select( tidyselect::all_of(coefficients) ) |>
    tidyr::pivot_longer( tidyselect::everything() ) |>
    dplyr::summarise(
      .by = name,
      m = mean(value),
      sd = sd(value)
    ) |>
    dplyr::mutate(offset = dplyr::if_else(m>=0, m+sd, m-sd) / 2) |>
    dplyr::pull(offset)
  
  # Add text
  if(add_label){
    for(coefficient in 1:length(coefficients)){
      # Generate the label for the coefficient
      coefficient_label <-
        as.matrix(bayes_model)[ ,coefficients[ coefficient ] ] |>
        rep_bayes_coef()
      
      # Get the coefficient offset
      coefficient_offset <- if_else(
        calc_offset[coefficient] >= 0,
        calc_offset[coefficient] + offset,
        calc_offset[coefficient] - offset)
      
      # Add text to plot
      p <- p +
        ggplot2::geom_text(
          label = coefficient_label,
          x = coefficient_offset,
          y = coefficients[coefficient],
          check_overlap = T,
          vjust = 1.5,
        )
    }
  }
  p
}

bayes_chain_stab <- function( bayes_model ){
  
  require(tidyverse)
  require(tidybayes)
  require(bayesplot)
  
  bayesplot::mcmc_rank_ecdf(
    bayes_model, paste0("b_", bayes_model |> brms::fixef() |> rownames()),
    plot_diff = T, prob = .99)
}

bayes_diag <- function( bayes_model, convergence = TRUE, offset = 0){
  #'
  #' Criteria:
  #'   -  Rhat > 1.05
  #'   -  ESS <= 1000
  
  require(tidyverse)
  require(tidybayes)
  require(bayesplot)
  
  tidybayes::summarise_draws(bayes_model) |>
    dplyr::summarise(
      rhat  = dplyr::if_else( any(rhat >= 1.05, na.rm=T),
                              paste("CHECK (", round(max(rhat, na.rm=T), 0), ")"),
                              paste("OK    (", round(max(rhat, na.rm=T), 0), ")")),
      ESS_Bulk = dplyr::if_else( any(ess_bulk <= 1000, na.rm=T),
                                 paste("CHECK (", round(min(ess_bulk, na.rm=T), 0), ")"),
                                 paste("OK    (", round(min(ess_bulk, na.rm=T), 0), ")")),
      ESS_Tail = dplyr::if_else( any(ess_tail <= 1000, na.rm=T),
                                 paste("CHECK (", round(min(ess_tail, na.rm=T), 0), ")"),
                                 paste("OK    (", round(min(ess_tail, na.rm=T), 0), ")"))
    ) |> print()
  
  bayes_coef_plot( bayes_model, offset = offset )
}

#' Apply Transformation Functions to Data
#'
#' @description
#' Applies a specified transformation function to data. Preserves the original data structure:
#' vectors return vectors, data frames/tibbles return the same with transformed columns,
#' and matrices return matrices with transformed values.
#'
#' @param data A vector, matrix, data frame, or tibble containing the data to transform
#' @param transformation Either a string specifying the name of a transformation function
#'   (e.g., "mean", "plogis", "exp") or a function object to apply
#' @param coefficient Optional. If \code{data} is a data frame or tibble, the name of the column to transform.
#'   If \code{data} is a matrix, \code{coefficient} can be a numeric index or name (if the matrix has dimnames).
#'   If NULL and \code{data} is a vector, the entire vector is transformed.
#'
#' @return The transformed data, preserving the original data structure
#'
#' @details
#' Supported transformation functions include:
#' \itemize{
#'   \item \code{mean}: Calculate the mean of the data
#'   \item \code{plogis}: Apply the logistic cumulative distribution function
#'   \item \code{dlogis}: Apply the logistic probability density function
#'   \item \code{rlogis}: Generate random numbers from a logistic distribution
#'   \item \code{exp}: Apply the exponential function
#'   \item \code{log}: Apply the natural logarithm
#'   \item \code{log2}: Apply the base-2 logarithm
#'   \item \code{log10}: Apply the base-10 logarithm
#' }
#'
#' @examples
#' # Apply the mean function to a vector
#' values <- c(1, 2, 3, 4, 5)
#' bayes_coefficient_estimate_switch(values, "mean")
#'
#' # Apply the logistic function to a column in a data frame
#' df <- data.frame(a = c(1, 2, 3), b = c(0.1, 0.5, 0.9))
#' transformed_df <- bayes_coefficient_estimate_switch(df, "plogis", coefficient = "b")
#'
#' # Apply a transformation to a tibble
#' library(tibble)
#' tbl <- tibble(a = c(1, 2, 3), b = c(0.1, 0.5, 0.9))
#' transformed_tbl <- bayes_coefficient_estimate_switch(tbl, "plogis", coefficient = "b")
#'
#' # Apply a transformation to a matrix column
#' mat <- matrix(1:6, nrow = 2, ncol = 3,
#'               dimnames = list(c("row1", "row2"), c("col1", "col2", "col3")))
#' # Transform the second column by index
#' transformed_mat1 <- bayes_coefficient_estimate_switch(mat, "exp", coefficient = 2)
#' # Transform by column name
#' transformed_mat2 <- bayes_coefficient_estimate_switch(mat, "exp", coefficient = "col3")
#'
#' # Apply a custom function
#' custom_func <- function(x) x^2 + 1
#' bayes_coefficient_estimate_switch(values, custom_func)
#'
#' @export
bayes_estimate_transformation <- function(data, transformation, coefficient = NULL) {
  # If transformation is already a function, use it directly
  transform_func <- if( is.function(transformation) ) {
    transformation
  } else {
    # Otherwise, get the function based on the name
    transform_func <- switch(
      transformation,
      plogis = plogis,
      dlogis = dlogis,
      rlogis = rlogis,
      exp    = exp,
      log    = log,
      log2   = log2,
      log10  = log10,
      # Add a default case to handle invalid transformation names
      stop( paste("Unknown transformation:", transformation) )
    )
  }
  
  # Apply the function to the data
  if (is.null(coefficient)) {
    # If data is a vector, just apply the transformation
    if (is.vector(data) && !is.list(data)) {
      return(transform_func(data))
    } else {
      # Handle case when user passes a whole tibble/data.frame without coefficient
      stop("Must specify a coefficient when data is not a simple vector")
    }
  } else {
    # Handle different data types
    if (is.data.frame(data) || inherits(data, "data.frame") || inherits(data, "tbl_df")) {
      # Data frame or tibble case - use [[ ]] for extraction
      if (!coefficient %in% names(data)) {
        stop(paste("Column", coefficient, "not found in the data"))
      }
      
      # Create a copy of the data to avoid modifying the original
      result <- data
      
      # Extract column data, apply transformation, and store result
      column_data <- data[[coefficient]]
      result[[coefficient]] <- transform_func(column_data)
      
      return(result)
    } else if (is.matrix(data)) {
      # Matrix case - use [, ] for extraction
      result <- data  # Create a copy
      
      # Handle either numeric index or column name
      col_index <- coefficient
      if (is.character(coefficient)) {
        # Check if the matrix has column names
        if (is.null(colnames(data))) {
          stop("Matrix has no column names. Use a numeric index instead.")
        }
        if (!coefficient %in% colnames(data)) {
          stop(paste("Column name", coefficient, "not found in the matrix"))
        }
        col_index <- which(colnames(data) == coefficient)
      } else if (is.numeric(coefficient)) {
        if (coefficient > ncol(data) || coefficient < 1) {
          stop(paste("Invalid column index:", coefficient))
        }
      } else {
        stop("For matrices, coefficient must be either a column name or numeric index")
      }
      
      # Apply transformation
      result[, col_index] <- transform_func( data[, col_index] )
      
      return(result)
    } else {
      stop("When coefficient is specified, data must be a data frame, tibble, matrix, or similar object")
    }
  }
}



bayes_tbl_sum <- function(bayes_model
                          , add_sigma = FALSE
                          , add_loo = FALSE
                          , add_R2 = FALSE
                          , add_loo_R2 = FALSE
                          , add_convergence = FALSE
                          , apa_table  = FALSE
                          , coef_calc   = "mean"
                          , coef_except = NULL
                          , .low_val    = FALSE
                          , fmt_md      = FALSE){
  
  require(tidyverse)
  require(tidybayes)
  
  ###   Set variables   ###
  coef_except <- c(coef_except, "threshold", "intercept")
  
  # If apa_table, set the below:
  if(apa_table){
    add_sigma  = T
    add_loo    = T
    add_R2     = T
    add_loo_R2 = T
  }
  
  #' FUNCTION FUNCTIONS
  fn_get_data_column <- function(data, name){
    if( is.data.frame(mod_data) ){
      return( mod_data[[name]] )
    } else { # else matrix
      return( mod_data[, name] )
    }
  }
  
  fn_add_convergence <- function(data, coefficient, new_name = NULL){
    # data |>
    coverg <- mod_sum |>
      dplyr::filter( variable %in% coefficient ) |>
      dplyr::select( variable, rhat, ess_bulk, ess_tail )
    if( !is.null(new_name) ){
      coverg <- coverg |> dplyr::mutate( variable = new_name )
    }
    coverg |> dplyr::rename_with(~paste0("conv_",.x), c(-variable))
  }
  
  
  # Get model data:
  mod_data <- as.matrix(bayes_model)
  
  # Get all variable names.
  var_names <- colnames(mod_data)
  
  # Get BETA variable names
  var_b_names <- var_names[stringr::str_detect(var_names, "^b_")]
  
  ###   ADD COEFFICIENTS   ###
  # FOR EACH BETA. Get the posterior (m, hdi, er, p | and)
  tbl <- purrr::map(var_b_names, \(coefficient){
    # Get the data column as a vector
    data <- fn_get_data_column(mod_data, coefficient)
    
    #' If the coefficient name starts with "Threshold" or "Intercept" (or
    #' additional - also ignore case, then we do not want to estimate the
    #' coefficient differently (i.e., follow est_formula).
    except_coefficient_switch <- purrr::map(est_except, \(except){
      stringr::str_starts(coefficient, stringr::regex(paste0("^", execpt), ignore_case = T ))
    }) |> list_c()
    
    print(paste0(" coef switch:", except_coefficient_switch))
    
    # If coefficient is except, then mean transformation, otherwise, as specified
    if( any(except_coefficient_switch) ){
      c_m   <- rep_bayes_m(   data )
      c_hdi <- rep_bayes_hdi( data )
    } else {
      c_m   <- rep_bayes_m(   bayes_estimate_transformation( data, coef_calc ) )
      c_hdi <- rep_bayes_hdi( bayes_estimate_transformation( data, coef_calc ) )
    }
    c_er <- rep_bayes_er( data )
    c_p  <- rep_bayes_p(  data )
    
    # Frame
    coef_est <- tibble::tibble(
      var = stringr::str_replace(coefficient, "b_", "")
      , m = c_m, hdi = c_hdi, er = c_er, p = c_p)
    
    # If convergence is enabled, add the convergence statistic for each BETA
    if(add_convergence){
      #' Get convergence statistics from function:
      mod_sum <- tidybayes::summarise_draws(bayes_model)
      
      # Add it together
      coef_est <- coef_est |>
        dplyr::rename_with(~paste0("est_", .x), c(-var)) |>
        left_join(
          fn_add_convergence(mod_sum, coefficient)
          , by = c("var"="variable")
        )
    }
    
    coef_est
  }) |> purrr::list_rbind()
  
  # If table, add group split:
  if(apa_table) tbl <- tbl |> dplyr::mutate(.before=1, group="Coefficients")
  
  
  ###    MODEL FIT    ###
  # If we should add sigma (random effects)
  if(add_sigma){
    # Get the SIGMA values (starts with sd_) (i)
    var_sd_names <- c(var_names[stringr::str_detect(var_names, "^sd_")])
    
    # For each SIGMA, get the posterior statistic
    sigma_tbl <- purrr::map(var_sd_names, \(estimate){
      # GET coefficient name
      estimate_name <- stringr::str_extract(estimate, "(?<=_).*?(?=_)")
      
      # create the row name
      estimate_row_name <- sprintf( ifelse(
        fmt_md, "$\\sigma\\,\\text{(%s)}$", "Sigma (%s)"
      ), estimate_name )
      
      # Get the estimate data
      data <- fn_get_data_column(mod_data, estimate)
      
      # Estimate the posterior
      sigma_est <- rep_bayes_coef( data, .rep = "df" ) |>
        dplyr::mutate(.before = 1, var = estimate_row_name)
      
      # If convergence add the convergence to the sigma
      if(add_convergence){
        sigma_est <- sigma_est |>
          dplyr::rename_with(~paste0("est_", .x), c(-var)) |>
          dplyr::left_join(
            fn_add_convergence(mod_sum, estimate, estimate_row_name)
            , by = c("var"="variable")
          )
      }
      
      # If table, add group split:
      if(apa_table) sigma_est <- sigma_est |> dplyr::mutate(group="Model fit")
      
      sigma_est
    }) |> purrr::list_rbind()
    
    print(tbl)
    print(sigma_tbl)
    tbl <- dplyr::bind_rows(tbl, sigma_tbl)
  }
  
  # If we add model fit criterion (leave one out information criterion)
  if(add_loo){
    message("Adding LOOIC...")
    
    if( !is.null(bayes_model[["criteria"]][["loo"]]) ){
      looic_name <- ifelse(fmt_md, "$\\text{LOOIC}$", "LOOIC")
      looic <-  tibble::tibble(
        var = looic_name
        , m   = bayes_model[["criteria"]][["loo"]][["estimates"]]["looic", "Estimate"] |>
          fmt_APA_numbers(.chr=T)
        , hdi = sprintf(
          "SE = %s", bayes_model[["criteria"]][["loo"]][["estimates"]]["looic", "SE"] |>
            fmt_APA_numbers(.chr=T) )
        , er = "", p  = ""
      )
      
      # If table, add group split:
      if(apa_table) looic <- looic |> dplyr::mutate(group="Model fit")
      
      tbl <- tbl |> dplyr::bind_rows(looic)
      
    } else {
      warning("NO LOOIC criterion found! Skipping... \n   Did you forget to `add_loo()`?")
    }
  }
  
  # If model LOOIC R2 add it.
  if(add_loo_R2){
    message("Adding LOO R2...")
    
    if( !is.null(bayes_model[["criteria"]][["loo_R2"]]) ){
      # Get name
      loo_R2_name <- if_else(fmt_md, "$\\text{LOO}\\,\\text{R}^2$", "LOO R2")
      
      # Get estimates
      loo_r2 <- tidybayes::mean_hdi( bayes_model[["criteria"]][["loo_R2"]] ) |>
        fmt_APA_numbers(.chr=T, .low_val=T, .rm_leading_0 = T)
      
      # Create tibble
      loo_r2_df <- tibble::tibble(
        var   = loo_R2_name
        , m   = loo_r2[["y"]]
        , hdi = sprintf( "[%s, %s]", loo_r2[["ymin"]], loo_r2[["ymax"]] )
        , er  = "", p   = "")
      
      # If table, add group split:
      if(apa_table) loo_r2_df <- loo_r2_df |> mutate(group="Model fit")
      
      tbl <- tbl |> dplyr::bind_rows(loo_r2_df)
    } else {
      warning("NO LOO R2 criterion found! Skipping... \n   Did you forget to `add_criterion()`?")
    }
  }
  
  # If simple R2, add it.
  if(add_R2){
    message("Adding R2...")
    
    if( !is.null(bayes_model[["criteria"]][["loo"]]) ){
      R2_name <- ifelse(fmt_md, "$\\text{R}^2$", "R2")
      
      r2 <- tidybayes::mean_hdi( bayes_model[["criteria"]][["bayes_R2"]] ) |>
        fmt_APA_numbers(.chr=T, .low_val=T, .rm_leading_0 = T)
      
      r2_df <- tibble::tibble(
        var   = R2_name
        , m   = r2[["y"]]
        , hdi = sprintf("[%s, %s]", r2[["ymin"]], r2[["ymax"]])
        , er  = "" , p   = "")
      
      # If table, add group split:
      if(apa_table) r2_df <- r2_df |> mutate(group="Model fit")
      
      tbl <- tbl |> dplyr::bind_rows(r2_df)
      
    } else {
      warning("NO R2 criterion found! Skipping... \n   Did you forget to `add_criterion()`?")
    }
  }
  message("Complete!")
  tbl
}


bayes_tbl_add_sig <- function(data, sig = .95, na.rm = T){
  #' Function to add significance marker to tables produced from @bayes_tbl_sum.
  
  require(tidyverse)
  
  data <-
    data |> mutate(ppp222 = as.numeric( str_remove_all(p, "[<>=]") ))
  if(na.rm) data <- data |> mutate( ppp222 = if_else( is.na(ppp222), 0, ppp222) )
  data |>
    mutate(
      m  = if_else(ppp222 > sig, paste0(m,"*"), m),
      ppp222 = NULL
    )
}


tab_bayes_generics <- function(data, fmt_md_var = F, pre_footnote="", post_footnote=""){
  c_names <- names(data$`_data`)
  vars <- data$`_data`$var
  
  # ER
  if( any( str_ends(c_names, "er" ) ) ){
    er_dir <- "ER~dir~ indicates the evidence ratio that the effect is in the *b* specified direction."
    if(pre_footnote != ""){ er_dir = paste0(" ", er_dir) }
  } else { er_dir = ""}
  
  # P
  if( any( str_ends(c_names, "p") ) ){
    p_dir <- " *p*~dir~ indicates the probability that the effect is in the *b* specified direction."
  } else { p_dir <- "" }
  
  
  # Convergence
  if( any( str_ends(c_names, "rhat") ) ){
    rhat <- " $\\hat{R}^2$ indicates convergence between and within-chain."
  } else { rhat <- "" }
  if( any( str_ends(c_names, "ess_bulk") ) ){
    ess_bulk <- " $\\text{ESS}_{bulk}$ indicates the effective sample size of the bulk of the distribution."
  } else { ess_bulk <- "" }
  if( any( str_ends(c_names, "ess_tail") ) ){
    ess_tail <- " $\\text{ESS}_{bulk}$ indicates the effective sample size of the tails of the distribution."
  } else { ess_tail <- "" }
  
  # Add empty space if some are present
  if( !("" %in% c(er_dir, p_dir)) ){
    post_footnote = paste0(" ", post_footnote)
  }
  
  # if( any( str_ends(vars, "LOOIC") ) ){
  #   looic <- " LOOIC = Leave one out information criterion"
  # } else { looic <- "" }
  
  # if( any( str_ends(vars, "LOO R2") ) ){
  #   loor2 <- " LOO R2 = Leave-one-out R squared"
  # } else { loor2 <- "" }
  
  
  # general transformation.
  data <-
    data |>
    cols_label(
      "var"             = ""
      , ends_with("m")    ~ md("*b*")
      , ends_with("hdi")  ~ md("HDI")
      , ends_with("er")   ~ md("ER~dir~")
      , ends_with("p")    ~ md("*p*~dir~")
      , ends_with("rhat") ~ md("$\\hat{R}$")
      , ends_with("e")    ~ ""
    ) |>
    tab_footnote(md(str_glue(
      "*Note.* {pre_footnote}{er_dir}{p_dir}{rhat}{ess_bulk}{ess_tail}{post_footnote}") )
      #'                                                            {looic}{loor2}
      , placement = "left"
    ) |>
    tab_footnote( md("\\* *p*~dir~ > .95") ) |> # Probability notation
    cols_align("center", -var) |>
    cols_align("left", var) |> # Align
    tab_fmt_APA()  # General table transformations
  
  if(fmt_md_var){
    data <- data |>
      fmt_markdown(columns = var)
  }
  
  data
}