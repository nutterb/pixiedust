print_dust_html <- function(x, ..., asis=TRUE, 
                            linebreak_at_end = getOption("pixie_html_linebreak", 2),
                            interactive = getOption("pixie_interactive"))
{
  if (is.null(interactive)) interactive <- interactive()
  if (!is.null(x$caption) & x$caption_number) increment_pixie_count()
  caption_number_prefix <- 
    if (x$caption_number) sprintf("Table %s: ", get_pixie_count())
    else ""
  
  label <-
    if (is.null(x[["label"]]))
    {
      chunk_label <- knitr::opts_current$get("label")
      if (is.null(chunk_label))
        sprintf("tab:pixie-%s", getOption("pixie_count"))
      else
        sprintf("tab:%s", chunk_label)
    }
    else
    {
     sprintf("tab:%s", x[["label"]])
    }
  
  label <-
    if (x[["bookdown"]])
    {
      sprintf("(\\#%s)", label)
    }
    else
    {
      caption_number_prefix
    }
  
  
  #* Determine the number of divisions
  #* It looks more complicated than it is, but the gist of it is
  #* total number of divisions: ceiling(total_rows / longtable_rows)
  #* The insane looking data frame is just to make a reference of what rows
  #*   go in what division.
  if (!is.numeric(x$longtable) & x$longtable) longtable_rows <- 25L
  else if (!is.numeric(x$longtable) & !x$longtable) longtable_rows <- as.integer(max(x$body$row))
  else longtable_rows <- as.integer(x$longtable)
  
  Divisions <- data.frame(div_num = rep(1:ceiling(max(x$body$row) / longtable_rows),
                                        each = longtable_rows)[1:max(x$body$row)],
                          row_num = 1:max(x$body$row))
  total_div <- max(Divisions$div_num)
  
  
  #* Format the table parts
  head <- part_prep_html(x$head, head = TRUE, 
                         fixed_header = x[["fixed_header"]],
                         fixed_header_class_name = x[["fixed_header_param"]][["fixed_header_class_name"]])
  body <- part_prep_html(x$body)
  foot <- if (!is.null(x$foot)) part_prep_html(x$foot) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_html(x$interfoot) else NULL
  
  tmpfile <- tempfile(fileext=".html")
  non_interactive <- ""
  
  #* Run a for loop to build all the table divisions
  for (i in 1:total_div){
    tbl <- .rbind_internal(head,
                           body[Divisions$row_num[Divisions$div_num == i], , drop=FALSE],
                           if (i == total_div) foot else interfoot)
    rows <- apply(tbl, 1, paste0, collapse = "\n")
    rows <- sprintf("<tr>\n%s\n</tr>", rows)
    
    justify <- 
      if (x[["justify"]] == "center") "margin:auto"
      else sprintf("float:%s", x[["justify"]])
    
    # Tables aligned to the left or right of the page need a barrier with the
    # clear propert set to prevent text from being placed next to the table.
    float_guard <- 
      if (x[["justify"]] == "center") ""
      else "<div style = 'clear:both'></div>"
    
    fixed_head_css <- 
      if (x[["fixed_header"]] & x[["include_fixed_header_css"]])
        do.call(fixed_header_css,
                c(x[["fixed_header_param"]],
                  list(pretty = FALSE)))
      else ""
    
    if (x[["fixed_header"]]){
      fixed_head_open_tag <- 
        sprintf("<div style = 'text-align:%s'><section class='%s-section'><div class='%s-container'><div>",
                x[["justify"]],
                x[["fixed_header_param"]][["fixed_header_class_name"]],
                x[["fixed_header_param"]][["fixed_header_class_name"]])
      fixed_head_close_tag <- "</div></section></div>"
    }
    else{
      fixed_head_open_tag <- fixed_head_close_tag <- ""
    }
    
    
    html_code <- sprintf("%s%s%s<table style = '%s;border-collapse:%s;'>\n%s\n</table>%s%s%s",
                         float_guard,
                         fixed_head_css,
                         fixed_head_open_tag,
                         justify,
                         x$border_collapse, 
                         paste0(rows, collapse = "\n"),
                         fixed_head_close_tag,
                         float_guard,
                         paste0(rep("</br>", linebreak_at_end), collapse = ""))
    
    if (!is.null(x$caption))
      html_code <- sub(">",
                       sprintf(">\n<caption>%s %s</caption>", 
                              label, x$caption),
                       html_code)
    
    #* When interactive, write to a temporary file so that it
    #* can be displayed in the viewer
    if (interactive & asis){
      write(html_code, tmpfile, append = i > 1)
    }
    else non_interactive <- paste0(non_interactive, html_code)
  }
  # print(html_code)
  if (interactive & asis){
    getOption("viewer")(tmpfile)
  }
  else if (asis){
    if (x$html_preserve) knitr::asis_output(htmltools::htmlPreserve(non_interactive))
    else knitr::asis_output(non_interactive)
  }
  else { 
    if (x$html_preserve) htmltools::htmlPreserve(non_interactive)
    else non_interactive
  }
  
}

#**** Helper functions

part_prep_html <- function(part, head=FALSE, 
                           fixed_header = FALSE, fixed_header_class_name = "")
{
  numeric_classes <- c("double", "numeric")
  
  dh <- 
    if (head)
    {
      if (fixed_header){
        sprintf("th class = 'th-%s'", fixed_header_class_name)
      }
      else
      {
        "th"
      }
    }
    else 
    {
      "td"
    }
  
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
  
  #* Bold and italic
  boldify <- part$bold
  part$bold[boldify] <- "font-weight:bold;"
  part$bold[!boldify] <- ""
  
  italicize <- part$italic
  part$italic[italicize] <- "font-style:italic;"
  part$italic[!italicize] <- ""
  
  #* Alignments. With horizontal alignment, first we determine
  #* default alignment for any cell without a given designation.
  #* The defaults are right aligned for numeric, left aligned for
  #* all otheres.  The `default_halign` function is defined in
  #* `print_dust_latex.R`
  
  logic <- part$halign == ""
  part$halign[logic] <- vapply(X = part$col_class[logic],
                               FUN = default_halign,
                               FUN.VALUE = character(1),
                               print_method = "html")
  part$halign <-
    with(part, sprintf("text-align:%s;", halign))
  
  logic <- part$valign != ""
  part$valign[logic] <-
    with(part, sprintf("vertical-align:%s;", valign[logic]))
  
  #** Background
  logic <- part$bg != ""
  part$bg[logic] <-
    with(part, sprintf("background-color:%s;", bg[logic]))
  
  #* Font Family
  logic <- part$font_family != ""
  part$font_family[logic] <-
    with(part, sprintf("font-family:%s;", font_family[logic]))
  
  #* Font Color
  logic <- part$font_color != ""
  part$font_color[logic] <-
    with(part, sprintf("color:%s;", font_color[logic]))
  
  #* Font size
  logic <- part$font_size != ""
  part$font_size[logic] <-
    with(part, sprintf("font-size:%s%s;", 
                       font_size[logic],
                       font_size_units[logic]))
  
  #* cell height and width
  logic <- part$height != ""
  part$height[logic] <-
    with(part, sprintf("height:%s%s;", height[logic], height_units[logic]))
  
  logic <- part$width != ""
  part$width[logic] <-
    with(part, sprintf("width:%s%s;", width[logic], width_units[logic]))
  
  #* Borders
  logic <- part$top_border != ""
  part$top_border[logic] <-
    with(part, sprintf("border-top:%s;", top_border[logic]))
  
  logic <- part$bottom_border != ""
  part$bottom_border[logic] <-
    with(part, sprintf("border-bottom:%s;", bottom_border[logic]))
  
  logic <- part$left_border != ""
  part$left_border[logic] <-
    with(part, sprintf("border-left:%s;", left_border[logic]))
  
  logic <- part$right_border != ""
  part$right_border[logic] <-
    with(part, sprintf("border-right:%s;", right_border[logic]))
  
  #* Set NA (missing) values to na_string
  logic <- is.na(part$value) & !is.na(part$na_string)
  part$value[logic] <-
    part$na_string[logic]
  
  #* Padding
  logic <- part$pad != ""
  part$pad[logic] <-
    with(part, sprintf("padding:%spx;", pad[logic]))
  
  #* Text Rotation
  logic <- part$rotate_degree != ""
  part$rotate_degree[logic] <-
    with(part, rotate_tag(rotate_degree[logic]))
  
  #* Generate css style definitions for each cell.
  part$value <-
    with(part, sprintf("<%s colspan = '%s'; rowspan = '%s'; style='%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s'>%s %s</%s>",
                       dh, colspan, rowspan, 
                       bold, italic, halign, valign, bg, font_family, #6
                       font_color, font_size, height, width,          #4
                       top_border, bottom_border, left_border, right_border, #4
                       rotate_degree, pad,  #2
                       value, 
                       if (fixed_header) paste0("<div>", value, "</div>") else "",
                       substr(dh, 1, 2)))

  ncol <- max(part$col)
  
  part <- part[!(part$rowspan == 0 | part$colspan == 0), ]
  
  logic <-
    part[["row"]] == part[["html_row"]] & 
    part[["col"]] == part[["html_col"]] & 
    part[["colspan"]] > 1
  
  if ("html_row_pos" %in% names(part))
    part[["html_row"]][logic] <- part[["html_row_pos"]][logic]
  
  if ("html_col_pos" %in% names(part))
    part[["html_col"]][logic] <- part[["html_col_pos"]][logic]
  
  #* Spread to wide format for printing
  part <- part[c("html_row", "html_col", "value")]  
  part <- reshape2::dcast(part,
                          html_row ~ html_col,
                          value.var = "value",
                          fill = "")
  part <- part[!names(part) %in% "html_row"]
  
  if (ncol(part) != ncol){
    part <- cbind(part,
                  do.call("cbind",
                          lapply(1:(ncol - ncol(part)),
                                 function(i) data.frame(value = "", 
                                                        stringsAsFactors = FALSE))))
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
  sprintf(
    paste0("-webkit-transform:rotate(%sdeg);",
           "-moz-transform:rotate(%sdeg);",
           "-ms-transform:rotate(%sdeg);",
           "-o-transform:rotate(%sdeg);",
           "transform:rotate(%sdeg);"),
    degree, degree, degree, degree, degree)
}
