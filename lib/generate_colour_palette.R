#' Generate a vector of colours from a colour code string
#'
#' This function takes a string of colour codes and returns a vector of corresponding hex colour values.
#' The function looks for colours set in "Options()" as "project_custom_colours".
#' The function supports the following color representations:
#' - Very light colors prefixed with forward slash (e.g., /r)
#' - Light colors prefixed with a dash (e.g., -r)
#' - Medium colors represented by lowercase letters (e.g., r)
#' - Dark colors represented by uppercase letters (e.g., R)
#' - Darkest colors prefixed with a plus sign (e.g., +R)
#' - Custom hex colors prefixed with a hash symbol followed by 6 characters (e.g., #FF5500)
#'
#' @param colour_str A character string containing color codes.
#'
#' @return A character vector of hex colour codes
#'
#' @examples
#' # Get medium red, dark red, medium blue, dark blue
#' gen_col("rRbB")  # Returns: c("#F8766D", "#b20b00", "#78ADFF", "#00348C")
#'
#' # Get all variations of blue from lightest to darkest
#' gen_col("/b-bbB+B")  # Returns: c("#d6e9ff", "#8EBEFF", "#78ADFF", "#00348C", "#00246D")
#'
#' # Insert a custom hex color
#' gen_col("rR#FF5500y")  # Returns: c("#F8766D", "#b20b00", "#FF5500", "#FFE359")
#'
#' @export
gen_col <- function(colour_str) {
  
  colours <- getOption("project_custom_colours")
  
  colour_adjustors <- c(
    "/" # Very light/weak
    , "-" # Light/weak
    , "+" # Strong
    , "#" # Custom hex color
  )
  
  # Split the input string into individual characters
  colour_chars <- strsplit(colour_str, "")[[1]]
  
  # Process special cases for colours
  result <- c()
  i <- 1
  
  while (i <= length(colour_chars)) {
    current_char <- colour_chars[i]
    
    # Check for color adjustors
    if (current_char %in% colour_adjustors) {
      # Special case for hex color (#)
      if (current_char == "#") {
        # Check if we have at least 6 more characters
        if (i + 6 <= length(colour_chars)) {
          # Get the next 6 characters as hex color
          hex_value <- paste0("#", paste(colour_chars[(i+1):(i+6)], collapse=""))
          
          # Validate if it's a proper hex color (only hex digits)
          if (grepl("^#[A-Fa-f0-9]{6}$", hex_value)) {
            result <- c(result, hex_value)
            i <- i + 7  # Move past the # and the 6 hex digits
            next
          } else {
            warning(paste0("Invalid hex color: ", hex_value))
          }
        } else {
          warning("Incomplete hex color code at the end of the string")
        }
      } else {
        # Handle other adjustors (/, -, +)
        if (i < length(colour_chars)) {
          colour_key <- paste0(current_char, colour_chars[i+1])
          if (colour_key %in% names(colours)) {
            result <- c(result, colours[[colour_key]])
          } else {
            warning(paste0("Unknown colour code: ", colour_key))
          }
          i <- i + 2
          next
        }
      }
    }
    
    # Process normal characters (single letter color codes)
    if (current_char %in% names(colours)) {
      result <- c(result, colours[[current_char]])
    } else {
      warning(paste0("Unknown colour code: ", current_char))
    }
    
    i <- i + 1
  }
  
  return(result)
}
