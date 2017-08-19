#' @name is_valid_color
#' @title Test a Character String For Pixiedust Recognized Color Format
#' 
#' @description \code{pixiedust} recognizes colors as dvips names, 
#'   \code{rgb(R,G,B)}, \code{rgba(R,G,B,A)}, \code{#RRGGBB}, or 
#'   \code{#RRGGBBAA}.  This code returns a logical indicating if 
#'   the given character strings are valid.
#'   
#' @param color A character vector of color names.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Returns a logical vector correctly identifying valid color formats.
#'  \item Casts an error if \code{color} is not a character object.
#' }
#'   
#' @export

is_valid_color <- function(color){
  
  checkmate::assert_character(x = color)
  
  vapply(X = color,
         FUN = is_valid_color_single,
         FUN.VALUE = logical(1),
         USE.NAMES = FALSE)
  
}

#' @rdname is_valid_color
#' @export
  
is_valid_color_single <- function(color)
{
  checkmate::assert_character(x = color,
                              len = 1)
  
  color <- tolower(color)
  color <- gsub("\\s", "", color)
  
  regex_0_255 <- "\\b([0-9]|[0-9][0-9]|0[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\b"
  regex_0_1 <- ",((\\.\\d+)?|0(\\.\\d+)?|1(\\.0+)?)[)]$" 
  
  regex_rgb <- sprintf("^rgb[(]%s[)]$",
                       paste0(rep(regex_0_255, 3), collapse = ","))
  
  regex_rgba <- sprintf("^rgba[(]%s%s$",
                        paste0(rep(regex_0_255, 3), collapse = ","),
                        regex_0_1)
  
  regex_html <- "^#[a-f0-9]{6}$"
  regex_html_alpha <- "^#[a-f0-9]{8}$"
  
  grepl(regex_rgb, color) | grepl(regex_rgba, color) | 
    grepl(regex_html, color) | grepl(regex_html_alpha, color) | 
    color %in% c(grDevices::colors(), "transparent")
}
