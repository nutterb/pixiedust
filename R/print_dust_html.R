#' @importFrom dplyr bind_rows
#' @importFrom dplyr select_
#' @importFrom knitr asis_output
#' @importFrom tidyr spread_

print_dust_html <- function(x, ...)
{
  #************************************************
  #* 1. apply a function, if any is indicated
  #* 2. Perform any rounding
  #* 3. Bold
  #* 4. Italic
  #* 5. Spread to wide format for printing
  #* 6. Column Names
  #************************************************
  
  # table <- part_prep_html(x$table, head = FALSE)
  
  head <- part_prep_html(x$head, head = TRUE)
  body <- part_prep_html(x$body)
  foot <- if (!is.null(x$foot)) part_prep_html(x$foot) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_html(x$interfoot) else NULL
  
  body <- dplyr::bind_rows(head, body, foot)
  
  rows <- apply(body, 1, paste0, collapse = "\n")
  rows <- paste0("<tr>", rows, "</tr>", sep = "\n")
  
  html_code <- paste0("<table style = 'border-collapse:", 
                      if (x$border_collapse) "collapse" else "separate" , ";'>",
                   paste0(rows, collapse = "\n"),
                   "</table>", 
                   sep = "\n")
  
  if (interactive()){
    tmpfile <- tempfile(fileext=".html")
    write(html_code, tmpfile)
    getOption("viewer")(tmpfile)
  }
  else knitr::asis_output(html_code)
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
      with(part, as.character(round(as.numeric(value[logic]), as.numeric(round[logic]))))
  
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
  part$widht[logic] <- 
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
  part$value <- gsub("[<]", "&lt; ", part$value)
  part$value <- gsub("[>]", "&gt; ", part$value)
  
  #* Generate css style definitions for each cell.
  part$value <- 
    with(part, paste0("<", dh, " style='", 
                      bold, italic, halign, valign, bg, font_color, 
                      font_size, height, width,
                      top_border, bottom_border, left_border, right_border,
                      rotate_degree, pad,
                      "'>", value, "</", dh, ">"))

  #* Spread to wide format for printing
  dplyr::select_(part, "row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
  
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
