#' @importFrom dplyr bind_rows
#' @importFrom dplyr data_frame
#' @importFrom dplyr select_
#' @importFrom htmltools htmlPreserve
#' @importFrom knitr asis_output
#' @importFrom stringr str_extract_all
#' @importFrom tidyr spread_


print_dust_latex <- function(x, ...)
{
  
  #* Determine the number of divisions
  #* It looks more complicated than it is, but the gist of it is
  #* total number of divisions: ceiling(total_rows / longtable_rows)
  #* The insane looking data frame is just to make a reference of what rows 
  #*   go in what division.
  if (!is.numeric(x$longtable) & x$longtable) longtable_rows <- 25
  else if (!is.numeric(x$longtable) & !x$longtable) longtable_rows <- max(x$body$row)
  else longtable_rows <- x$longtable
  
#   Divisions <- data.frame(div_num = rep(1:ceiling(max(x$body$row) / longtable_rows),
#                                         each = longtable_rows)[1:max(x$body$row)],
#                           row_num = 1:max(x$body$row))
#   total_div <- max(Divisions$div_num)
  
  
  #* Format the table parts
  head <- part_prep_latex(x$head, head = TRUE)
  body <- part_prep_latex(x$body)
  foot <- if (!is.null(x$foot)) part_prep_latex(x$foot) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_latex(x$interfoot) else NULL
  
  

  begin <- paste0("\\begin{tabular}{", 
                  paste0(rep("c", max(x$body$col)), collapse = ""), "}\n")
  end <- "\\end{tabular}"
  
  tbl <- paste0(vapply(list(head, body, foot, interfoot),
                 paste_latex_part,
                 character(1)),
          collapse = "\n")
  
  knitr::asis_output(paste(begin, tbl, end, collapse = "\n"))
}

#**** Helper functions

part_prep_latex <- function(part, head=FALSE)
{
  numeric_classes <- c("double", "numeric")
  
  #* apply a function, if any is indicated
  part <- perform_function(part) 
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
      as.character(roundSafe(part$value[logic], as.numeric(part$round[logic])))
  
  #* Bold and italic
  part$value[part$bold] <- paste0("\\textbf{", part$value[part$bold], "}")
  
  part$value[part$italic] <- paste0("\\emph{", part$value[part$italic], "}")

  #* Alignments. Unlike with markdown, we do not assign alignments where 
  #* none are given.  I chose not to do so because I didn't want to override
  #* any CSS settings that may exist elsewhere in the document.
  logic <- part$halign != ""
  part$value[logic] <- 
    paste0("\\multicolumn{", part$colspan[logic], "}{", 
           substr(part$halign[logic], 1, 1), "}{",
           part$value[logic], "}")
#   
#   logic <- part$valign != ""
#   part$valign[logic] <- 
#     with(part, paste0("vertical-align:", valign[logic], ";"))
#   
#   #** Background
#   logic <- part$bg != ""
#   part$bg[logic] <- 
#     with(part, paste0("background-color:", bg[logic], ";"))
#   
  #* Font Color
  logic <- part$font_color != ""
  part$font_color <- vapply(part$font_color, 
                            convertColor,
                            character(1))
  part$value[logic] <- 
    paste0("\\textcolor", part$font_color[logic], "{", part$value[logic], "}")
  
  #* Font size
  logic <- part$font_size != ""
  part$font_size_units[logic] <- ifelse(part$font_size_units[logic] == "%",
                                        "pt",
                                        part$font_size_units[logic])
                                        
  part$value[logic] <- 
    paste0("{\\fontsize{", part$font_size[logic],
           part$font_size_units[logic], "}{1em}\\selectfont ",
           part$value[logic], "}")
#   
#   #* cell height and width
#   logic <- part$height != ""
#   part$height[logic] <- 
#     with(part, paste0("height:", height[logic], height_units[logic], ";"))
#   
#   logic <- part$width != ""
#   part$width[logic] <- 
#     with(part, paste0("width:", width[logic], width_units[logic], ";"))
#   
#   #* Borders
#   logic <- part$top_border != ""
#   part$top_border[logic] <- 
#     with(part, paste0("border-top:", top_border[logic], "; "))
#   
#   logic <- part$bottom_border != ""
#   part$bottom_border[logic] <- 
#     with(part, paste0("border-bottom:", bottom_border[logic], "; "))
#   
#   logic <- part$left_border != ""
#   part$left_border[logic] <- 
#     with(part, paste0("border-left:", left_border[logic], "; "))
#   
#   logic <- part$right_border != ""
#   part$right_border[logic] <- 
#     with(part, paste0("border-right:", right_border[logic], "; "))
#   
  #* Set NA (missing) values to na_string
  logic <- is.na(part$value) & !is.na(part$na_string)
  part$value[logic] <- 
    part$na_string[logic]
#   
#   #* Padding
#   logic <- part$pad != ""
#   part$pad[logic] <- 
#     with(part, paste0("padding:", pad[logic], "px;"))
#   
#   #* Text Rotation
#   logic <- part$rotate_degree != ""
#   part$rotate_degree[logic] <- 
#     with(part, rotate_tag(rotate_degree[logic]))
# 
#   #* Replace some of the potentially problematic symbols with HTML codes
#   #* At some point, this should be handled by a 'sanitize' like function
#   #* (see xtable)
#   part$value <- gsub("[<]", "&lt; ", part$value)
#   part$value <- gsub("[>]", "&gt; ", part$value)
#   
#   #* Generate css style definitions for each cell.
#   part$value <- 
#     with(part, paste0("<", dh, 
#                       " colspan = '", colspan, "'; ", 
#                       "rowspan = '", rowspan, "'; ",
#                       "style='", 
#                       bold, italic, halign, valign, bg, font_color, 
#                       font_size, height, width,
#                       top_border, bottom_border, left_border, right_border,
#                       rotate_degree, pad,
#                       "'>", value, "</", dh, ">"))
#   
  ncol <- max(part$col)
  part <- dplyr::filter_(part, "!(rowspan == 0 | colspan == 0)")
# 
# 
# #   part$value[part$rowspan == 0] <- ""
# #   part$value[part$colspan == 0] <- ""

  #* Spread to wide format for printing
  part <- dplyr::select_(part, "row", "col", "value") %>%
    tidyr::spread_("col", "value", fill = "") %>%
    dplyr::select_("-row")
  
  if (ncol(part) != ncol){
    part <- dplyr::bind_cols(part, 
                     do.call("cbind",
                             lapply(1:(ncol - ncol(part)), 
                            function(i) dplyr::data_frame(value = ""))))
    names(part) <- 1:ncol
  }
  part
}

paste_latex_part <- function(part){
  if (is.null(part)) return("")
  apply(part, 1, paste, collapse = " & ") %>%
    paste0(" \\\\") %>%
    paste0(collapse = "\n")
}

convertColor <- function(color){
  if (grepl("#", color)){
    return(paste0("[HTML]{", sub("#", "", color), "}"))
  }
  else if (grepl("rgb", color, ignore.case = TRUE)){
    rgb <- stringr::str_extract_all(color, "\\d{1,3}", simplify = TRUE)[1, 1:3]
    return(paste0("[RGB]{", paste0(rgb, collapse=","), "}"))
  }
  else return(color)
}
  
