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
  
  col_width <- determine_column_width(Joint)
  col_halign_default <- get_column_halign(Joint)
  
  row_height <- lapply(list(x$head, x$body, x$foot, x$interfoot), 
                       determine_row_height)
                       

  #* Format the table parts
  head <- part_prep_latex(x$head, col_width, col_halign_default, head = TRUE)
  body <- part_prep_latex(x$body, col_width, col_halign_default)
  foot <- if (!is.null(x$foot)) part_prep_latex(x$foot, col_width, col_halign_default) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_latex(x$interfoot, col_width, col_halign_default) else NULL
  
  #* Write the LaTeX Code
  prebegin <- numeric_longtable_newline(longtable_rows, is.numeric(x$longtable))
  
  begin <- paste0("\\begin{", tab_env, "}{", 
                  paste0(col_halign_default$default_halign, collapse = ""), "}\n")
  end <- paste0("\\end{", tab_env, "}")
  
  #* Convert each part into a character string
  #* Returns a character vector of length 4.
  tbl <- mapply(paste_latex_part,
                list(head, body, foot, interfoot),
                row_height,
                MoreArgs = list(newline = if (is.numeric(x$longtable)) " \\ltabnewline" else " \\\\"))

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
part_prep_latex <- function(part, col_width, col_halign_default, head=FALSE)
{
  part %<>% 
    dplyr::select(-width) %>%
    dplyr::left_join(col_width, by = c("col" = "col")) %>%
    dplyr::left_join(col_halign_default, by = c("col" = "col")) %>%
    dplyr::mutate(width_units = "pt",
                  halign = ifelse(halign == "", default_halign, halign)) 
   
    #* Calculate the row cell width for multicolumn cells
    
    Widths <- part %>%
      dplyr::select(html_row, html_col, width, merge) %>%
      dplyr::distinct() %>%
      dplyr::group_by(html_row, html_col) %>%
      dplyr::mutate(width = ifelse(merge == TRUE, 
                            sum(width[merge]),
                            width)) %>%
      dplyr::ungroup()
    
    part %>% 
      dplyr::select(-width) %>%
      dplyr::left_join(Widths,
                        by = c("html_row" = "html_row", 
                               "html_col" = "html_col",
                               "merge" = "merge")) %>%
      dplyr::mutate(width = ifelse(is.na(width), "", width))

  
  numeric_classes <- c("double", "numeric")
  
  #* apply a function, if any is indicated
  part <- perform_function(part) 
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
    as.character(roundSafe(part$value[logic], as.numeric(part$round[logic])))

  #* Bold and italic
  boldify <- part$bold
  part$value[boldify] <- paste0("\\textbf{", part$value[boldify], "}")

  italicize <- part$italic
  part$value[italicize] <- paste0("\\emph{", part$value[italicize], "}")
  
  #* Set NA (missing) values to na_string
  logic <- is.na(part$value) & !is.na(part$na_string)
  part$value[logic] <- 
    part$na_string[logic]
  
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
  
  parbox = needs_parbox(part)
  
  
  part$halign_parbox <- part$halign
  part$halign_parbox[parbox] <- 
    c("r" = "\\raggedleft", 
      "c" = "\\centering", 
      "l" = "\\raggedright", 
      "p" = "\\raggedright")[substr(part$halign[parbox], 1, 1)]
  
  part$value[parbox] <- 
    paste0("\\parbox[", substr(part$valign[parbox], 1, 1), "]{", part$width[parbox], "pt}{", 
               part$halign_parbox[parbox], " ",
               part$value[parbox], "}")
  
  #* Add the multirow tag where appropriate
  logic <- part$rowspan > 1
  part$value[logic] <- 
    paste0("\\multirow{", part$rowspan[logic], "}{*}{", part$value[logic], "}")
  
  #* Add blank multicolumn tags to fill multirow spaces
  #* set the colspan and rowspan to prevent deletion.
  #*   They are set to -1 to indicate that they are fillers
  logic <- part$html_row != part$row & part$html_col == part$col
  part$value[logic] <- paste0("\\multicolumn{", part$colspan[logic], "}",
                              "{", part$left_border[logic], "c", part$right_border[logic], "}{}")
  part$rowspan[logic] <- -1
  part$colspan[logic] <- -1
  
  #* Place multicolumn tags where needed
  logic <- part$colspan > 1
  part$value[logic] <- 
    paste0("\\multicolumn{", part$colspan[logic], "}{", 
           substr(part$halign[logic], 1, 1), "}{", part$value[logic], "}")
  
  #* Remove value where a merged cell is not the display cell
  ncol <- max(part$col)
  part %<>% dplyr::filter(!(rowspan == 0 | colspan == 0))
  
    
  
  dplyr::select(part, row, col, value) %>%
    tidyr::spread(col, value, fill = NA) %>%
    dplyr::select(-row)
}

#* Converts the data frame object to one line of LaTeX
#* code per row.
paste_latex_part <- function(part, row_height, newline = " \\\\"){
  paste_row <- function(r) paste(r[!is.na(r)], collapse = " & ")
  
  if (is.null(part)) return("")
  #* This commented line existed when I had horizontal 
  #* borders worked out.  It may be needed again.
  # apply(part[, -(1:2), drop = FALSE], 1, paste_row) %>%
    apply(part[, , drop = FALSE], 1, paste_row) %>%
    paste(row_height) %>%
    paste(newline) %>%
#     paste(part[, 2]) %>%     #* also from borders
#     paste(part[, 1], .) %>%  #* also from borders
    paste0(collapse = "\n")
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
#* Determine if the cell needs a parbox

needs_parbox <- function(x)
{
  !is.na(x$width) | 
    (x$halign != x$default_halign) | 
    x$valign != "" | 
    x$merge
}

#**************************************************
#**************************************************
#* Combine the four table parts for convenience of looking for common traits
joint_reference_table <- function(x){
  numeric_classes <- c("double", "numeric")
  
  addPartCol <- function(p, part_name) {
    if (is.null(p)) return(NULL)
    p$part <- part_name 
    return(p)
  }

  Joint <- 
    mapply(addPartCol,
         x[c("head", "body", "foot", "interfoot")],
         part_name = c("head", "body", "foot", "interfoot"),
         SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(width = as.numeric(width),
                  table_width = x$table_width * 72.27,
                  width = ifelse(width_units == "in",
                                 width * 72.27,
                                 ifelse(width_units == "cm",
                                        width * 28.45, 
                                        ifelse(width_units == "%",
                                               width/100 * table_width, 
                                               width)))) %>%
  #* apply a function, if any is indicated
  perform_function() 
  
  #* Perform any rounding
  logic <- Joint$round != "" & Joint$col_class %in% numeric_classes
  if (any(logic))
    Joint$value[logic] <- 
    as.character(roundSafe(Joint$value[logic], as.numeric(Joint$round[logic])))
  
  Joint$halign[Joint$halign == ""] <- 
    vapply(Joint$col_class[Joint$halign == ""],
           default_halign,
           character(1))
  
  Joint %>%
    dplyr::mutate(halign = substr(halign, 1, 1)) %>%
    dplyr::group_by(col) %>%
    dplyr::mutate(default_halign = names(sort(table(halign), decreasing = TRUE))[1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(parbox = needs_parbox(.),
                  width_by_char = nchar(value) * 4.5) %>%
    dplyr::group_by(col) %>%
    dplyr::mutate(replace = all(is.na(width)) && any(parbox),
                  width_by_char = max(width_by_char, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(width = ifelse(replace,
                                 width_by_char,
                                 width)) %>%
    dplyr::select(col, row, width, default_halign)
}

#**************************************************
#**************************************************
#* Get the default column alignments.
#* Right aligned for numeric, otherwise, left aligned
determine_column_width <- function(Joint, x)
{
  Joint %>%
    dplyr::select(row, col, width) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(width = max(width, na.rm=TRUE)) %>%
    dplyr::ungroup() 
}

determine_row_height <- function(part)
{
  if (is.null(part)) return("")
  part %>%
    dplyr::select(row, col, height, height_units) %>%
    dplyr::mutate(height = as.numeric(height), 
                  height = ifelse(height_units == "in", 
                                  height * 72.27, 
                                  ifelse(height_units == "cm",
                                         height * 28.45,
                                         height))) %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(height = max(height, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(height = ifelse(is.na(height),
                         "", paste0("\\\\[", height, "pt]"))) %>%
    '$'("height") 
}

#**************************************************
#**************************************************
#* Get the default column alignments.
#* Right aligned for numeric, otherwise, left aligned

get_column_halign <- function(Joint){
  Joint %>%
    dplyr::mutate(default_halign = ifelse(is.na(width),
                                          default_halign,
                                          paste0("p{", width, "pt}"))) %>%
    dplyr::select(row, col, default_halign) %>%
    dplyr::group_by(col) %>%
    dplyr::summarise(default_halign = default_halign[1]) %>%
    dplyr::ungroup() 
}

default_halign <- function(col_class){
  if (col_class %in% c("numeric", "int", "double")) "r" else "l"
}


utils::globalVariables(c("halign", "left_border", "right_border", 
                         "bottom_border", "top_border",
                         "require_multicol", "height", "width",
                         "height_units", "width_units", "table_width",
                         "parbox", "width_by_char", "html_row", 
                         "html_col", "rowspan", "colspan"))