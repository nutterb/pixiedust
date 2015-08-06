#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom dplyr ungroup
#' @importFrom tidyr spread

print_dust_console <- function(x, ...)
{
  #************************************************
  #* 1. apply a function, if any is indicated
  #* 2. Perform any rounding
  #* 3. Bold
  #* 4. Italic
  #* 5. Spread to wide format for printing
  #* 6. Column Names
  #************************************************
  
  head <- part_prep_console(x$head)
  body <- part_prep_console(x$body)
  
  names(body) <- names(head) <- head[1, ]
  
  if (nrow(head) > 1) body <- dplyr::bind_rows(head[-1, ], body)
  
  print(as.data.frame(body))
}

#**** Helper functions

part_prep_console <- function(part)
{
  #* values in the dust object are all stored as character strings.
  #* These classes need to be converted to numerics for rounding
  #* to have the appropriate effect.
  numeric_classes <- c("double", "numeric")
  
  #* If functions are assigned, perform the function.
  part <- perform_function(part)
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
    with(part, as.character(round(as.numeric(value[logic]), as.numeric(round[logic]))))
  
  
    #* Bold text.  In the console, bold text is denoted by "**".  In order
    #* to keep the all of the formatting lined up in columns, the data 
    #* frame is grouped by column, and if any cell in the column has bold 
    #* text, the unbolded text gets two spaces on either side to make the 
    #* columns the same width.
    dplyr::group_by_(part, "col_name") %>%
    dplyr::mutate_(value = ~if (any(bold)) ifelse(bold, 
                                                  paste0("**", value, "**"),
                                                  paste0("  ", value, "  "))
                   else value) %>%
    dplyr::ungroup() %>%
    #* Italic. Follows the same process as bold text.
    dplyr::group_by_("col_name") %>%
    dplyr::mutate_(value = ~if (any(italic)) ifelse(italic, 
                                                    paste0("_", value, "_"),
                                                    paste0(" ", value, " "))
                   else value) %>%
    dplyr::ungroup() %>%
    #* Spread to wide format for printing
    dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
}