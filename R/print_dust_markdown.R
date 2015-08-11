#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom knitr asis_output
#' @importFrom knitr kable
#' @importFrom tidyr spread

print_dust_markdown <- function(x, ...)
{
  #************************************************
  #* 1. apply a function, if any is indicated
  #* 2. Perform any rounding
  #* 3. Bold
  #* 4. Italic
  #* 5. Spread to wide format for printing
  #* 6. Column Names
  #************************************************
  
  head <- part_prep_markdown(x$head)
  body <- part_prep_markdown(x$body)
  
  names(body) <- names(head) <- head[1, ]
  
  subhead <- head[-1, ]
  subhead <- lapply(subhead, function(v) paste0("**", v, "**")) %>%
    as.data.frame(stringsAsFactors=FALSE)

  if (nrow(head) > 1) body <- dplyr::bind_rows(subhead, body)
  
  numeric_classes <- c("numeric", "double", "int")
  
  #* Determine the alignments.  Alignments in 'knitr::kable' are assigned
  #* by the first letter of the HTML alignment.  If no alignment is 
  #* assigned, a default is chosen based on the variable type.  Numerics
  #* are aligned right, characters are aligned left.
  alignments <- dplyr::filter_(x$head, "row == 1") %>%
    dplyr::select_("row", "col", "halign", "col_class") %>%
    dplyr::mutate_(halign = ~ifelse(halign == "",
                                    ifelse(col_class %in% numeric_classes, 
                                           "r",
                                           "l"),
                                    substr(halign, 1, 1)))
  knitr::asis_output(
    paste(c("", "", knitr::kable(body,
                                 format = "markdown",
                                 align = alignments$halign)), 
          collapse = "\n"))
  
}

#**** Helper functions

part_prep_markdown <- function(part)
{
  numeric_classes <- c("double", "numeric")
  
  part <- perform_function(part)
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
    with(part, as.character(round(as.numeric(value[logic]), as.numeric(round[logic]))))
  
  #* Bold text
  logic <- part$bold
  part$value[logic] <- 
    with(part, paste0("**", value[logic], "**"))
  
  #* Italic text
  logic <- part$italic
  part$value[logic] <- 
    with(part, paste0("_", value[logic], "_"))


  #* Spread to wide format for printing
  dplyr::select_(part, "row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
}