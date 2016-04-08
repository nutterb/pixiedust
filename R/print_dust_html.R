#' @importFrom dplyr bind_rows
#' @importFrom dplyr data_frame
#' @importFrom dplyr select_
#' @importFrom htmltools htmlPreserve
#' @importFrom knitr asis_output
#' @importFrom tidyr spread_


print_dust_html <- function(x, ..., asis=TRUE)
{
  
  if (!is.null(x$caption)) increment_pixie_count()
  
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
  

  #* Format the table parts
  head <- part_prep_html(x$head, head = TRUE)
  body <- part_prep_html(x$body)
  foot <- if (!is.null(x$foot)) part_prep_html(x$foot) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_html(x$interfoot) else NULL

  tmpfile <- tempfile(fileext=".html")
  non_interactive <- ""

  #* Run a for loop to build all the table divisions
  for (i in 1:total_div){
    tbl <- dplyr::bind_rows(head, 
                            body[Divisions$row_num[Divisions$div_num == i], , drop=FALSE], 
                            if (i == total_div) foot else interfoot)
    rows <- apply(tbl, 1, paste0, collapse = "\n")
    rows <- paste0("<tr>", rows, "</tr>", sep = "\n")
  
    html_code <- paste0("<table style = 'border-collapse:", 
                        if (x$border_collapse) "collapse" else "separate" , ";'>",
                     paste0(rows, collapse = "\n"),
                     "</table><br/><br/>", 
                     sep = "\n")
    
    if (!is.null(x$caption)) 
      html_code <- sub(">", 
                       paste0(">\n<caption>Table ", get_pixie_count(), ": ", x$caption, "</caption>"),
                       html_code)
  
    #* When interactive, write to a temporary file so that it 
    #* can be displayed in the viewer
    if (interactive() & asis){
      write(html_code, tmpfile, append = i > 1)
    }
    else non_interactive <- paste0(non_interactive, html_code)
  }
  # print(html_code)
  if (interactive() & asis){
    getOption("viewer")(tmpfile)
  }
  else if (asis) knitr::asis_output(htmltools::htmlPreserve(non_interactive))
  else htmltools::htmlPreserve(non_interactive)
  
}

#**** Helper functions

part_prep_html <- function(part, head=FALSE)
{
  numeric_classes <- c("double", "numeric")
  
  dh <- if (head) "th" else "td"
  
  #* apply a function, if any is indicated
  part <- perform_function(part) 
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
       as.character(roundSafe(part$value[logic], as.numeric(part$round[logic])))
  
  #* Bold and italic
  boldify <- part$bold
  part$bold[boldify] <- "font-weight:bold;"
  part$bold[!boldify] <- ""
  
  italicize <- part$italic
  part$italic[italicize] <- "font-style:italic;"
  part$italic[!italicize] <- ""

  #* Alignments. Unlike with markdown, we do not assign alignments where 
  #* none are given.  I chose not to do so because I didn't want to override
  #* any CSS settings that may exist elsewhere in the document.
  logic <- part$halign != ""
  part$halign[logic] <- 
    with(part, paste0("text-align:", halign[logic], ";"))
  
  logic <- part$valign != ""
  part$valign[logic] <- 
    with(part, paste0("vertical-align:", valign[logic], ";"))
  
  #** Background
  logic <- part$bg != ""
  part$bg[logic] <- 
    with(part, paste0("background-color:", bg[logic], ";"))
  
  #* Font Family
  logic <- part$font_family != ""
  part$font_family[logic] <-
    with(part, paste0("font-family:", font_family[logic], ";"))
  
  #* Font Color
  logic <- part$font_color != ""
  part$font_color[logic] <- 
    with(part, paste0("color:", font_color[logic], ";"))
  
  #* Font size
  logic <- part$font_size != ""
  part$font_size[logic] <- 
    with(part, paste0("font-size:", font_size[logic],
                      font_size_units[logic], ";"))
  
  #* cell height and width
  logic <- part$height != ""
  part$height[logic] <- 
    with(part, paste0("height:", height[logic], height_units[logic], ";"))
  
  logic <- part$width != ""
  part$width[logic] <- 
    with(part, paste0("width:", width[logic], width_units[logic], ";"))
  
  #* Borders
  logic <- part$top_border != ""
  part$top_border[logic] <- 
    with(part, paste0("border-top:", top_border[logic], "; "))
  
  logic <- part$bottom_border != ""
  part$bottom_border[logic] <- 
    with(part, paste0("border-bottom:", bottom_border[logic], "; "))
  
  logic <- part$left_border != ""
  part$left_border[logic] <- 
    with(part, paste0("border-left:", left_border[logic], "; "))
  
  logic <- part$right_border != ""
  part$right_border[logic] <- 
    with(part, paste0("border-right:", right_border[logic], "; "))
  
  #* Set NA (missing) values to na_string
  logic <- is.na(part$value) & !is.na(part$na_string)
  part$value[logic] <- 
    part$na_string[logic]
  
  #* Padding
  logic <- part$pad != ""
  part$pad[logic] <- 
    with(part, paste0("padding:", pad[logic], "px;"))
  
  #* Text Rotation
  logic <- part$rotate_degree != ""
  part$rotate_degree[logic] <- 
    with(part, rotate_tag(rotate_degree[logic]))

  #* Replace some of the potentially problematic symbols with HTML codes
  #* At some point, this should be handled by a 'sanitize' like function
  #* (see xtable)
#   part$value <- gsub("[<]", "&lt; ", part$value)
#   part$value <- gsub("[>]", "&gt; ", part$value)
  
  #* Generate css style definitions for each cell.
  part$value <- 
    with(part, paste0("<", dh, 
                      " colspan = '", colspan, "'; ", 
                      "rowspan = '", rowspan, "'; ",
                      "style='", 
                      bold, italic, halign, valign, bg, font_family, font_color, 
                      font_size, height, width,
                      top_border, bottom_border, left_border, right_border,
                      rotate_degree, pad,
                      "'>", value, "</", dh, ">"))
  
  ncol <- max(part$col)
  part <- dplyr::filter_(part, "!(rowspan == 0 | colspan == 0)")


#   part$value[part$rowspan == 0] <- ""
#   part$value[part$colspan == 0] <- ""

  #* Spread to wide format for printing
  part <- dplyr::select_(part, "html_row", "html_col", "value") %>%
    tidyr::spread_("html_col", "value", fill = "") %>%
    dplyr::select_("-html_row")
  
  if (ncol(part) != ncol){
    part <- dplyr::bind_cols(part, 
                     do.call("cbind",
                             lapply(1:(ncol - ncol(part)), 
                            function(i) dplyr::data_frame(value = ""))))
    names(part) <- 1:ncol
  }
  part
}

#***********************************

#* Rotation tags vary by browser.  To make the rotation as robust as 
#* possible, specifying a rotation applies tags for webkit (Chrome?), 
#* Mozilla, Internet Explorer, Opera, and a generic transformation.

rotate_tag <- function(degree)
{
  paste0("-webkit-transform:rotate(", degree, "deg);",
         "-moz-transform:rotate(", degree, "deg);",
         "-ms-transform:rotate(", degree, "deg);",
         "-o-transform:rotate(", degree, "deg);",
         "transform:rotate(", degree, "deg);")
}
