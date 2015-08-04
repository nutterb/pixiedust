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
  
  body <- dplyr::bind_rows(head, body)
  
  rows <- apply(body, 1, paste0, collapse = "\n")
  rows <- paste0("<tr>", rows, "</tr>", sep = "\n")
  
  knitr::asis_output(
    paste0("<table>",
           paste0(rows, collapse = "\n"),
           "</table>", 
           sep = "\n"))

  
}

#**** Helper functions

part_prep_html <- function(part, head=FALSE)
{
  numeric_classes <- c("double", "numeric")
  
  dh <- if (head) "th" else "td"
  
  #* 1. apply a function, if any is indicated
  part <- perform_function(part) 
  
  #* 2. Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
      with(part, as.character(round(as.numeric(value[logic]), as.numeric(round[logic]))))
  
  #* 3. Bold and italic
  boldify <- part$bold
  part$bold[boldify] <- "font-weight:bold;"
  part$bold[!boldify] <- ""
  
  italicize <- part$italic
  part$italic[italicize] <- "font-style:italic;"
  part$italic[!italicize] <- ""

  #** Alignments
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
  
  #* x. Font Color
  logic <- part$font_color != ""
  part$font_color[logic] <- 
    with(part, paste0("color:", font_color[logic], ";"))
  
  #* x. Font size
  logic <- part$font_size != ""
  part$font_size[logic] <- 
    with(part, paste0("font-size:", font_size[logic],
                      font_size_units[logic], ";"))
  
  #* x. cell height and width
  logic <- part$height != ""
  part$height[logic] <- 
    with(part, paste0("height:", height[logic], height_units[logic], ";"))
  
  logic <- part$width != ""
  part$widht[logic] <- 
    with(part, paste0("width:", width[logic], width_units[logic], ";"))
  
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
  
  logic <- part$pad != ""
  part$pad[logic] <- 
    with(part, paste0("padding:", pad[logic], "px;"))
  
  logic <- part$rotate_degree != ""
  part$rotate_degree[logic] <- 
    with(part, rotate_tag(rotate_degree[logic]))

  part$value <- gsub("[<]", "&lt; ", part$value)
  part$value <- gsub("[>]", "&gt; ", part$value)
  
  part$value <- 
    with(part, paste0("<", dh, " style='", 
                      bold, italic, halign, valign, bg, font_color, 
                      font_size, height, width,
                      top_border, bottom_border, left_border, right_border,
                      rotate_degree, pad,
                      "'>", value, "</", dh, ">"))

    #* 5. Spread to wide format for printing
    dplyr::select_(part, "row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
  
}

rotate_tag <- function(degree)
{
  paste0("-webkit-transform:rotate(", degree, "deg);",
         "-moz-transform:rotate(", degree, "deg);",
         "-ms-transform:rotate(", degree, "deg);",
         "-o-transform:rotate(", degree, "deg);",
         "transform:rotate(", degree, "deg);")
}