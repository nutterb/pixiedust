#' @name pixiedust_option
#' @title \code{pixiedust} Options
#' 
#' @description The \code{pixiedust} package references many different options
#'   that may be used to define the characteristics of tables throughout a 
#'   session.  Use of these options can make it easier to format tables
#'   consistently with a reduced code base.  A list of the options is 
#'   given below along with their defaults.
#'   
#' @details \code{pixie_bookdown} determines if references and labels are 
#' managed using the \code{bookdown} package methods.  This should be set 
#' to \code{TRUE} if you are rendering documents via the \code{bookdown} 
#' package.
#' 
#' \code{border_collapse} determines the settings for border styles in HTML
#' tables.  The most common values are \code{"collapse"} - which presses all
#' of the borders between cells on top of each other - and \code{"separate"} - 
#' which allows each cell to have its own, distinct border.  
#' 
#' \code{pixie_count} is used to manage table numbering in non-LaTeX tables.
#' See \code{\link{set_pixie_count}} for methods to manipulate the numbering.
#' 
#' \code{pixie_float} determines if tables in LaTeX output are placed in 
#' floating environments.
#' 
#' \code{pixie_hhline} determins if tables in LaTeX output use the 
#' \code{hhline} package for constructing table cells.
#' 
#' \code{pixie_html_linebreak} controls the number of line breaks placed 
#' after a table in HTML output.
#' 
#' \code{pixie_justify} controls the positioning of the complete table in the
#' document.  Note that \code{"none"} renders the table to the left side of 
#' the page, and subsequent elements will appear below the table.  When using
#' \code{"left"}, subsequent elements will appear to the right of the table.
#' When using \code{"right"}, subsequent elements will appear to the left of 
#' the table.
#' 
#' \code{pixie_longtable} determines if the \code{longtable} environment is 
#' used in LaTeX output.
#' 
#' \code{pixie_na_string} sets the default character set for replacing 
#' \code{NA} values in tables.
#' 
#' \code{pixie_tabcolsep} determines the spacing placed between cells in 
#' LaTeX output.  
#' 
#' \code{pixiedust_print_method} Sets the default printing method for tables. 
#' When \code{pixiedust} is being used with \code{knitr} and \code{rmarkdown},
#' the default is the value of \code{knitr::opts_knit$get("rmarkdown.pandoc.to")},
#' otherwise it is \code{"console"} 
#'   
#' @section Options:
#' \tabular{lll}{
#'  Option Name          \tab Default         \tab Permissible Values        \cr
#'  \code{pixie_bookdown} \tab \code{FALSE}   \tab \code{logical}            \cr
#'  \code{pixie_border_collapse} \tab \code{"collapse"} \tab \code{collapse, separate, initial, inherit} \cr
#'  \code{pixie_count}   \tab 0               \tab \code{integer} like value  \cr
#'  \code{pixie_float}   \tab \code{TRUE}     \tab \code{logical}               \cr
#'  \code{pixie_hhline}  \tab \code{FALSE}    \tab \code{logical}             \cr
#'  \code{pixie_html_linebreak} \tab 2        \tab \code{integer} like value  \cr
#'  \code{pixie_justify} \tab \code{"center"} \tab \code{center, none, left, right} \cr
#'  \code{pixie_longtable} \tab \code{FALSE}  \tab \code{logical}             \cr
#'  \code{pixie_na_string} \tab NA            \tab \code{character}           \cr
#'  \code{pixie_tabcolsep} \tab 6             \tab \code{integer} like value  \cr
#'  \code{pixiedust_print_method} \tab  \tab \code{console, html, latex, markdown, beamer}
#' }
#' 

NULL