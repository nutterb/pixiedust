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
  numeric_classes <- c("double", "numeric")
  
  perform_function(part) %>%
    #* 2. Perform any rounding
    dplyr::mutate_(
      value = ~suppressWarnings(
        ifelse(!is.na(round) & col_class %in% numeric_classes,
               as.character(round(as.numeric(value), round)),
               value))) %>%
    #* 3. Bold
    dplyr::group_by_("col_name") %>%
    dplyr::mutate_(value = ~if (any(bold)) ifelse(bold, 
                                                  paste0("**", value, "**"),
                                                  paste0("  ", value, "  "))
                   else value) %>%
    dplyr::ungroup() %>%
    #* 4. Italic
    dplyr::group_by_("col_name") %>%
    dplyr::mutate_(value = ~if (any(italic)) ifelse(italic, 
                                                    paste0("_", value, "_"),
                                                    paste0(" ", value, " "))
                   else value) %>%
    dplyr::ungroup() %>%
    #* 5. Spread to wide format for printing
    dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
}