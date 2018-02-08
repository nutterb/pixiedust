#* This provides an alternative method for generating horizontal lines in 
#* LaTeX tables, using the hhline package.  The advantage to hhline is that
#* when both backgrounds and borders are applied, the cell background won't 
#* overshadow the borders as is the case when using \cline (the approach
#* used by print_dust_latex.
#* Use of the hhline method is determined by the package option
#* getOption("pixiedust_latex_hhline"), with the default being TRUE.

print_dust_latex_hhline <- function(x, ..., asis=TRUE)
{
  
  if (!is.null(x$caption) & x$caption_number) increment_pixie_count()
  
  label <- 
    if (is.null(x[["label"]]))
    {
      chunk_label <- knitr::opts_current$get("label")
      if (is.null(chunk_label))
        paste0("tab:pixie-", getOption("pixie_count"))
      else
        paste0("tab:", chunk_label)
    }
  else
  {
    paste0("tab:", x[["label"]])
  }
  
  label <- 
    if (x[["bookdown"]])
    {
      paste0("(\\#", label, ")")
    }
  else
  {
    paste0("\\label{", label, "}")
  }
  
  #* Determine the number of divisions
  #* It looks more complicated than it is, but the gist of it is
  #* total number of divisions: ceiling(total_rows / longtable_rows)
  #* The insane looking data frame is just to make a reference of what rows 
  #*   go in what division.
  if (!is.numeric(x$longtable) & x$longtable) longtable_rows <- 25L
  else if (!is.numeric(x$longtable) & !x$longtable) longtable_rows <- as.integer(max(x$body$row))
  else longtable_rows <- as.integer(x$longtable)
  
  tab_env <- if (is.numeric(x$longtable) || x$longtable) "longtable" else "tabular"
  
  Joint <- joint_reference_table(x)
  
  col_width <- determine_column_width(Joint)
  col_halign_default <- get_column_halign(Joint)
  
  row_height <- lapply(list(x$head, x$body, x$foot, x$interfoot), 
                       determine_row_height)
                       

  #* Format the table parts
  head <- part_prep_latex_hhline(x$head, col_width, col_halign_default, head = TRUE)
  body <- part_prep_latex_hhline(x$body, col_width, col_halign_default)
  foot <- if (!is.null(x$foot)) part_prep_latex_hhline(x$foot, col_width, col_halign_default) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_latex_hhline(x$interfoot, col_width, col_halign_default) else NULL
  
  #* Write the LaTeX Code
  prebegin <- numeric_longtable_newline(longtable_rows, is.numeric(x$longtable))
  prebegin <- paste0(prebegin, 
                     "\\setlength{\\tabcolsep}{", x$tabcolsep, "pt}", sep = "\n")
  
  if (tab_env == "longtable")
  {
    begin <- paste0("\\begin{longtable}[",
                    sub("n", "l", substr(x[["justify"]], 1, 1)), "]{",
                    paste0(col_halign_default$default_halign, collapse = ""), "}\n",
                    if (!is.null(x$caption))
                      paste("\\caption", 
                            if (x$caption_number) "" else "*",
                            "{", x$caption, "}")
                    else "", 
                    "\n", label, "\\\\ \n")
    end <- "\\end{longtable}"
  }
  else if (x$float)
  {
    begin <- paste0("\\begin{table}\n",
                    if (x[["justify"]] == "center") "\\centering\n" else "",
                    if (!is.null(x$caption))
                      paste0("\\caption", 
                             if (x$caption_number) "" else "*", 
                             "{", x$caption, "}")
                    else "", 
                    "\n", label,
                    "\\begin{tabular}{",
                    paste0(col_halign_default$default_halign, collapse = ""), "}\n")
    
    end <- paste0("\\end{tabular}\n\\end{table}\n")
  }
  else
  {
    begin <- paste0(if (x[["justify"]] == "center")
      "\\begin{center}\n"
      else
        "",
      if (!is.null(x$caption))
        paste0("\\captionof{table}{", x$caption, "}")
      else "", 
      "\n", label,
      "\\begin{tabular}{",
      paste0(col_halign_default$default_halign, collapse = ""), "}\n")
    end <- paste0("\\end{tabular}\n",
                  if (x[["justify"]] == "center")
                    "\\end{center}\n"
                  else
                    "")
  }
  
  
  
  
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
part_prep_latex_hhline <- function(part, col_width, col_halign_default, head=FALSE)
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
      dplyr::distinct(html_row, html_col, width, merge) %>%
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
  logic <- part$round == "" & part$col_class %in% numeric_classes
  part$round[logic] <- getOption("digits")
  
  logic <- part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <-
    as.character(roundSafe(part$value[logic], as.numeric(part$round[logic])))
  
  #* Replacement
  logic <- !is.na(part[["replace"]])
  part[["value"]][logic] <- part[["replace"]][logic]
  
  #* Set NA (missing) values to na_string
  logic <- is.na(part$value) & !is.na(part$na_string)
  part$value[logic] <- 
    part$na_string[logic]
  
  #* Sanitize value strings
  #* `sanitize` is defined in `print_dust_latex.R`
  logic <- part[["sanitize"]]
  part[["value"]][logic] <- sanitize(part[["value"]][logic],
                                     part[["sanitize_args"]][logic])

  #* Bold and italic
  boldify <- part$bold
  part$value[boldify] <- paste0("\\textbf{", part$value[boldify], "}")

  italicize <- part$italic
  part$value[italicize] <- paste0("\\emph{", part$value[italicize], "}")
  
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
  
  logic <- part$rotate_degree != ""
  part$value[logic] <- 
    paste0("\\rotatebox{", part$rotate_degree[logic], "}{", part$value[logic], "}")
  
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
    mapply(latex_horizontal_border_code_hhline, 
           part$bottom_border[logic],
           part$col[logic])
  part$bottom_border[!logic] <- "~"
  bottom_borders <- dplyr::select(part, row, col, bottom_border) %>%
    tidyr::spread(col, bottom_border) %>%
    dplyr::select(-row) %>%
    apply(1, paste0, collapse = "") %>%
    paste0("\\hhline{", ., "}")
  
  logic <- part$top_border != ""
  part$top_border[logic] <- 
    mapply(latex_horizontal_border_code_hhline, 
           part$top_border[logic],
           part$col[logic])
  part$top_border[!logic] <- "~"
  top_borders <- dplyr::select(part, row, col, top_border) %>%
    tidyr::spread(col, top_border) %>%
    dplyr::select(-row) %>%
    apply(1, paste0, collapse = "") %>%
    paste0("\\hhline{", ., "}")
  
  
  
  
  parbox <- needs_parbox(part)
  
  
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
  part$colspan[logic] <- part$colspan[logic] * -1
  
  #* Place multicolumn tags where needed
  logic <- part$colspan > 1 | (part$left_border != "" | part$right_border != "") | 
            !(part$html_row != part$row & part$html_col == part$col)
  part$value[logic] <- 
    paste0("\\multicolumn{", part$colspan[logic], "}{", 
           part$left_border[logic], 
           #* 'p' isn't a valid alignment in 'multicol', so we replace it with 'r'
           sub("p", "r", substr(part$halign[logic], 1, 1)), 
           part$right_border[logic], "}{", part$value[logic], "}")
  
  #* Remove value where a merged cell is not the display cell
  ncol <- max(part$col)
  part %<>% dplyr::filter(!(rowspan == 0 | colspan == 0))
  
  #* In order to get the multirow to render correctly, the cell with 
  #* the multirow needs to be at the top of the block.  This 
  #* rearranges the merged cells so that the multirow is at the top.
  
  proper_multirow <- 
    part[part$colspan != 1, ] %>%
    dplyr::mutate(group = paste0(html_row, html_col)) %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(dplyr::desc(colspan)) %>%
    dplyr::mutate(row = sort(row)) %>%
    dplyr::ungroup()
  
  part %<>%
    dplyr::filter(colspan == 1) %>%
    dplyr::bind_rows(proper_multirow)
  
  cbind(top_borders, 
        bottom_borders,
        dplyr::select(part, row, col, value) %>%
          tidyr::spread(col, value, fill = NA) %>%
          dplyr::select(-row)
  )
}

#**************************************************
#**************************************************
#* Prepares code for horizontal borders
latex_horizontal_border_code_hhline <- function(x, col){
  border <- str_split_fixed_base(x, " ", 3)
  border[, 1] <- gsub("px", "pt", border[, 1])
  border[, 2] <- ifelse(test= border[, 2] %in% c("dashed", "dotted", "groove", 
                                                "ridge", "inset", "outset"),
                        yes = "solid", 
                        no = border[, 2])
  if (border[, 2] %in% c("hidden", "none")) return("~")
  if (border[, 2] == "solid"){
    border_code <- paste0("<{\\arrayrulecolor", 
                          convertColor(border[, 3]), 
                          "}-")
    return(border_code)
  }
  if (border[, 2] %in% c("double")){
    border_code <- paste0(">{\\arrayrulecolor", 
                          convertColor(border[, 3]), 
                          "}=")
    return(border_code)
  }
}


utils::globalVariables(c("halign", "left_border", "right_border", 
                         "bottom_border", "top_border",
                         "require_multicol", "height", "width",
                         "height_units", "width_units", "table_width",
                         "parbox", "width_by_char", "html_row", 
                         "html_col", "rowspan", "colspan", "group"))
