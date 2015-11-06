#' @importFrom dplyr bind_rows
#' @importFrom dplyr data_frame
#' @importFrom dplyr select_
#' @importFrom htmltools htmlPreserve
#' @importFrom knitr asis_output
#' @importFrom stringr str_extract_all
#' @importFrom tidyr spread_


print_dust_latex <- function(x, ..., asis=TRUE)
{
  
  #* Determine the number of divisions
  #* It looks more complicated than it is, but the gist of it is
  #* total number of divisions: ceiling(total_rows / longtable_rows)
  #* The insane looking data frame is just to make a reference of what rows 
  #*   go in what division.
  if (!is.numeric(x$longtable) & x$longtable) longtable_rows <- 25
  else if (!is.numeric(x$longtable) & !x$longtable) longtable_rows <- max(x$body$row)
  else longtable_rows <- x$longtable
  
  tab_env <- if (is.numeric(x$longtable) || x$longtable) "longtable" else "tabular"
  
  Joint <- joint_reference_table(x)
  
  col_halign <- get_column_halign(Joint)
  
  #* Format the table parts
  head <- part_prep_latex(x$head, head = TRUE, col_halign = col_halign)
  body <- part_prep_latex(x$body, col_halign = col_halign)
  foot <- if (!is.null(x$foot)) part_prep_latex(x$foot, col_halign = col_halign) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_latex(x$interfoot, col_halign = col_halign) else NULL
  
  #* Write the LaTeX Code
  prebegin <- numeric_longtable_newline(longtable_rows, is.numeric(x$longtable))
  
  begin <- paste0("\\begin{", tab_env, "}{", 
                  paste0(col_halign, collapse = ""), "}\n")
  end <- paste0("\\end{", tab_env, "}")
  
  #* Convert each part into a character string
  #* Returns a character vector of length 4.
  tbl <- vapply(list(head, body, foot, interfoot),
                paste_latex_part,
                character(1),
                newline = if (is.numeric(x$longtable)) " \\ltabnewline" else " \\\\")
  
  #* Append longtable tags
  if (is.numeric(x$longtable) || x$longtable){
    tbl <- paste0(tbl[c(1, 4, 3, 2)], 
                  c("\n\\endhead\n", "\n\\endfoot\n", "\n\\endlastfoot\n", ""))
  }
  
  tbl <- paste(tbl, collapse = "\n")
  
  if (asis) knitr::asis_output(paste(prebegin, begin, tbl, end, collapse = "\n"))
  else paste(prebegin, begin, tbl, end, collapse = "\n")
 
}

#* Prepare Cell Values for Printing
part_prep_latex <- function(part, head=FALSE, col_halign)
{
  #* If a cell's column alignment differs from the column default, 
  #* the \multicolumn command will need to be used.  This 
  #* joins the table part with the default column setting, then
  #* sets a logical flag for those variables that need to use
  #* the multicolumn.  It also flags cells that have vertical 
  #* borders specified.
  part <- dplyr::left_join(part,
                           data.frame(col = 1:length(col_halign),
                                      col_halign = col_halign,
                                      stringsAsFactors = FALSE)) %>%
    dplyr::mutate(require_multicol = (halign != "" & halign != col_halign) |
                              (left_border != "" | right_border != "" | 
                                 bottom_border != "" | top_border != ""),
           halign = ifelse(require_multicol & halign == "",
                           col_halign,
                           substr(halign, 1, 1)))
  
  numeric_classes <- c("double", "numeric")
  
  #* apply a function, if any is indicated
  part <- perform_function(part) 
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
    as.character(roundSafe(part$value[logic], as.numeric(part$round[logic])))
  
  #* Set NA (missing) values to na_string
  logic <- is.na(part$value) & !is.na(part$na_string)
  part$value[logic] <- 
    part$na_string[logic]
  
  #* Bold and italic
  part$value[part$bold] <- paste0("\\textbf{", part$value[part$bold], "}")
  
  part$value[part$italic] <- paste0("\\emph{", part$value[part$italic], "}")
  
  #* Font Color
  logic <- part$font_color != ""
  part$font_color <- vapply(part$font_color, 
                            convertColor,
                            character(1))
  part$value[logic] <- 
    paste0("\\textcolor", part$font_color[logic], "{", part$value[logic], "}")
  
  #* Font size
  logic <- part$font_size != ""
  part$font_size_units[logic] <- ifelse(part$font_size_units[logic] %in% c("%", "px"),
                                        "pt",
                                        part$font_size_units[logic])
  
  part$value[logic] <- 
    paste0("{\\fontsize{", part$font_size[logic],
           part$font_size_units[logic], "}{1em}\\selectfont ",
           part$value[logic], "}")
  
  #** Background
  logic <- part$bg != ""
  part$bg[logic] <- 
    paste0("\\cellcolor", vapply(part$bg[logic],
                                 convertColor,
                                 character(1)))
  
  part$value[logic] <- paste(part$bg[logic], part$value[logic])
  
  
  #** Borders
  logic <- part$left_border != ""
  part$left_border[logic] <- 
    vapply(part$left_border[logic], latex_vertical_border_code, character(1))
  
  logic <- part$right_border != ""
  part$right_border[logic] <- 
    vapply(part$right_border[logic], latex_vertical_border_code, character(1))
  
  logic <- part$bottom_border != ""
  part$bottom_border[logic] <- 
    mapply(latex_horizontal_border_code, 
           part$bottom_border[logic],
           part$col[logic])
  bottom_borders <- dplyr::select(part, row, col, bottom_border) %>%
    tidyr::spread(col, bottom_border) %>%
    dplyr::select(-row) %>%
    apply(1, paste0, collapse = "")
  
  logic <- part$top_border != ""
  part$top_border[logic] <- 
    mapply(latex_horizontal_border_code, 
           part$top_border[logic],
           part$col[logic])
  top_borders <- dplyr::select(part, row, col, top_border) %>%
    tidyr::spread(col, top_border) %>%
    dplyr::select(-row) %>%
    apply(1, paste0, collapse = "")
  
  
  #* Column alignment for cells that differ from the default
  logic <- part$require_multicol
  part$value[logic] <- 
    paste0("\\multicolumn{", part$colspan[logic], "}", 
           "{", part$left_border[logic], " ", part$halign[logic], " ", part$right_border[logic], "}",
           "{", part$value[logic], "}")
  
  ncol <- max(part$col)
  part <- dplyr::filter_(part, "!(rowspan == 0 | colspan == 0)")
  
  #* Spread to wide format for printing
  part <- dplyr::select_(part, "row", "col", "value") %>%
    tidyr::spread_("col", "value", fill = NA) %>%
    dplyr::select_("-row")
  
  if (ncol(part) != ncol){
    part <- dplyr::bind_cols(part, 
                             do.call("cbind",
                                     lapply(1:(ncol - ncol(part)), 
                                            function(i) dplyr::data_frame(value = NA))))
    names(part) <- 1:ncol
  }
  
  cbind(top_borders, bottom_borders, part)
}

#* Converts the data frame object to one line of LaTeX
#* code per row.

paste_latex_part <- function(part, newline = " \\\\"){
  paste_row <- function(r) paste(r[!is.na(r)], collapse = " & ")
  
  if (is.null(part)) return("")
  #* This commented line existed when I had horizontal 
  #* borders worked out.  It may be needed again.
  apply(part[, -(1:2), drop = FALSE], 1, paste_row) %>%
  # apply(part[, , drop = FALSE], 1, paste_row) %>%
    paste(newline) %>%
    paste(part[, 2]) %>%     #* also from borders
    paste(part[, 1], .) %>%  #* also from borders
    paste0(collapse = "\n")
}
  

#**************************************************
#**************************************************
#* Combine the four table parts for convenience of looking for common traits
joint_reference_table <- function(x){
  addPartCol <- function(p, part_name) {
    if (is.null(p)) return(NULL)
    p$part <- part_name 
    return(p)
  }
  mapply(addPartCol,
         x[c("head", "body", "foot", "interfoot")],
         part_name = c("head", "body", "foot", "interfoot"),
         SIMPLIFY = FALSE) %>%
    dplyr::bind_rows()
}

#**************************************************
#**************************************************
convertColor <- function(color){
  if (length(color) == 0) return(character(0))
  
  if (grepl("#", color)){
    return(paste0("[HTML]{", sub("#", "", color), "}"))
  }
  else if (grepl("rgb", color, ignore.case = TRUE)){
    rgb <- stringr::str_extract_all(color, "\\d{1,3}", simplify = TRUE)[1, 1:3]
    return(paste0("[RGB]{", paste0(rgb, collapse=","), "}"))
  }
  else return(paste0("{", color, "}"))
}

#**************************************************
#**************************************************
#* Get the default column alignments.
#* Right aligned for numeric, otherwise, left aligned

get_column_halign <- function(Joint){

  Joint$halign[Joint$halign == ""] <- 
    vapply(Joint$col_class[Joint$halign == ""],
           default_halign,
           character(1))
  Joint <- Joint %>%
    dplyr::mutate(halign = substr(halign, 1, 1)) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(col_halign = names(sort(table(halign), decreasing = TRUE))[1])
  Joint$col_halign
}

default_halign <- function(col_class){
  if (col_class %in% c("numeric", "int", "double")) "r" else "l"
}

#**************************************************
#**************************************************
#* Get the column widths.
#* Because of differing units, the following 
#* hierarchy is adopted.
#* in > cm > % > pt


#**************************************************
#**************************************************
#* Writes the code that is necessary to force
#* longtable breaks at the user-specified number 
#* of lines
numeric_longtable_newline <- function(n, redefine = FALSE){
  if (redefine)
    return(paste0("\\newcount\\mylineno \n",
                  "\\mylineno=0 \n",
                  "\\def\\ltabnewline{% \n", 
                  "\\global\\advance\\mylineno by 1 \n", 
                  "\\ifnum\\mylineno=", n, " \n",
                  "\\global\\mylineno=0 \n",
                  "\\\\ \n",
                  "\\newpage \n",
                  "\\else \n",
                  "\\\\ \n",
                  "\\fi \n",
                  "}"))
  else return("")
}

#**************************************************
#**************************************************
#* Prepares code for vertical borders
latex_vertical_border_code <- function(x){
  border <- stringr::str_split_fixed(x, " ", 3)
  border[, 1] <- gsub("px", "pt", border[, 1])
  border[, 2] <- ifelse(border[, 2] %in% c("dashed", "dotted"), 
                        "dashed",
                        ifelse(border[, 2] %in% c("groove", "ridge", "inset", "outset", "hidden"),
                               "solid", border[, 2]))
  if (border[, 2] %in% c("hidden", "none")) return("")
  if (border[, 2] == "dashed"){
    border_code <- paste("!{\\color", convertColor(border[, 3]), "\\vdashline}")
    return(border_code)
  }
  if (border[, 2] %in% c("solid", "double")){
    border_code <- paste0("!{\\color", convertColor(border[, 3]), "\\vrule width ", border[, 1], "}")
    return(border_code)
  }
}

#**************************************************
#**************************************************
#* Prepares code for horizontal borders
latex_horizontal_border_code <- function(x, col){
  border <- stringr::str_split_fixed(x, " ", 3)
  border[, 1] <- gsub("px", "pt", border[, 1])
  border[, 2] <- ifelse(border[, 2] %in% c("dashed", "dotted"), 
                        "dashed",
                        ifelse(border[, 2] %in% c("groove", "ridge", "inset", "outset", "hidden"),
                               "solid", border[, 2]))
  if (border[, 2] %in% c("hidden", "none")) return("")
  if (border[, 2] == "dashed"){
    border_code <- paste0("\\arrayrulecolor", convertColor(border[, 3]), 
                          "\\cdashline{", col, "-", col, "}")
    return(border_code)
  }
  if (border[, 2] %in% c("solid", "double")){
    border_code <- paste0("\\arrayrulecolor", convertColor(border[, 3]), 
                          "\\cline{", col, "-", col, "}")
    return(border_code)
  }
}


utils::globalVariables(c("halign", "left_border", "right_border", 
                         "bottom_border", "top_border",
                         "require_multicol"))