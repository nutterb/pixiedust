#' @name medley
#' 
#' @title Sprinkle Medleys
#' @description \code{pixiedust} can get to be pretty verbose if you are doing
#'   a great deal of customization.  Sprinkle medleys can take out some of that
#'   code by bundling much of the formatting sprinkling into a single function.
#'   
#'   \code{pixiedust} comes with a couple very basic medleys that are mostly 
#'   for illustration of how to write medleys.  Once you get the hang of 
#'   sprinkling, you need only bundle your most common sprinkles into a 
#'   medley function of your own and cut down on some of the time 
#'   coding your most basic formatting.
#'   
#' @param x a \code{dust} object.
#' @param round A numerical value passed to the \code{round} sprinkle.
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' \dontrun{
#' fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
#' 
#' dust(fit) %>%
#'   medley_bw() %>%
#'   sprinkle_print_method("html")
#'   
#' dust(fit, glance_foot = TRUE) %>%
#'   medley_model() %>%
#'   sprinkle_print_method("html")
#'   
#' # Medleys are not generics and do not have methods.
#' # Using a medley on a dust_list object requires pixieply
#' 
#' library(dplyr)
#' mtcars %>% 
#'   group_by(gear) %>% 
#'   dust(ungroup = FALSE) %>% 
#'   pixieply(medley_bw) %>% 
#'   sprinkle_print_method("html")
#' }
#' 

#' @export

medley_bw <- function(x){
  x %>%
    sprinkle(rows = 1, border = "top", part = "head") %>%
    sprinkle(rows = 1, border = "top", part = "body") %>%
    sprinkle(rows = max(x$body$row), border = "bottom", part = "body")
}

#' @rdname medley
#' @export
medley_model <- function(x, round = 2){
  not_pval <- unique(x$body$col_name)
  not_pval <- not_pval[!not_pval %in% "p.value"]
  
  x <- x %>%
    #* Borders
    sprinkle(rows = 1, border = "top", part = "head") %>%
    sprinkle(rows = 1, border = "top", part = "body") %>%
    sprinkle(rows = max(x$body$row), border = "bottom", part = "body") %>%
    #* Rounding
    sprinkle(cols = not_pval, round = round, part = "body") %>%
    sprinkle(cols = "p.value", fn = quote(pvalString(value)))
  
  if (!is.null(x$foot)){
    x <- x %>%
      sprinkle(rows = max(x$foot$row), border = "bottom", part = "foot") %>%
      sprinkle(round = round, na_string = "", part = "foot")
  }
  x
}


