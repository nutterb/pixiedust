#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom dplyr ungroup
#' @importFrom tidyr spread

print_dust_console <- function(x, ...)
{
  
  #* Determine the number of divisions
  #* It looks more complicated than it is, but the gist of it is
  #* total number of divisions: ceiling(total_rows / longtable_rows)
  #* The insane looking data frame is just to make a reference of what rows 
  #*   go in what division.
  if (!is.numeric(x$longtable) & x$longtable) longtable_rows <- 25
  else if (!is.numeric(x$longtable) & !x$longtable) longtable_rows <- max(x$body$row)
  else longtable_rows <- x$longtable
  
  Divisions <- data.frame(div_num = rep(1:ceiling(max(x$body$row) / longtable_rows),
                                        each = longtable_rows)[1:max(x$body$row)],
                          row_num = 1:max(x$body$row))
  total_div <- max(Divisions$div_num)
  
  #* Format table parts
  head <- part_prep_console(x$head)
  body <- part_prep_console(x$body)
  foot <- if (!is.null(x$foot)) part_prep_console(x$foot) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_console(x$interfoot) else NULL
  
  names(body) <- names(head) <- head[1, ]
  
  if (!is.null(foot)) names(foot) <- names(head)
  if (!is.null(interfoot)) names(interfoot) <- names(head)
  
  
  #* Run a loop to print all the divisions
  for (i in 1:total_div){
    tbl <- dplyr::bind_rows(if (nrow(head) > 1) head[-1, ] else NULL, 
                            body[Divisions$row_num[Divisions$div_num == i], ], 
                            if (i == total_div) foot else interfoot)
    print(as.data.frame(tbl))
    cat("\n\n")
  }
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
    with(part, as.character(roundSafe(as.numeric(value[logic]), as.numeric(round[logic]))))
  
  
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