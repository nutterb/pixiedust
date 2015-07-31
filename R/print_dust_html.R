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
  perform_function(part) %>%
    #* 2. Perform any rounding
    dplyr::mutate_(
      value = ~suppressWarnings(
        ifelse(!is.na(round) & col_class %in% numeric_classes,
               as.character(round(as.numeric(value), round)),
               value))) %>%
    #* 3. Bold and italic
    dplyr::mutate_(bold = ~ifelse(bold, 
                                  "font-weight:bold;",
                                  ""),
                   italic = ~ifelse(italic, 
                                    "font-style:italic;", 
                                    "")) %>%
    #** Alignments
    dplyr::mutate_(halign = ~ifelse(is.na(halign), 
                                    "",
                                    paste0("text-align:", halign, ";"))) %>%
    #** Vertical alignments
    dplyr::mutate_(valign = ~ifelse(is.na(valign), 
                                    "",
                                    paste0("vertical-align:", valign, ";"))) %>%
    #** Background
    dplyr::mutate_(bg = ~ifelse(is.na(bg), "",
                                paste0("background-color: ", bg, ";"))) %>%
    #* x. Font Color
    dplyr::mutate_(font_color = ~ifelse(is.na(font_color), "",
                                        paste0("color:", font_color, "; "))) %>%
    #* x. Font size
    dplyr::mutate_(font_size = ~ifelse(is.na(font_size), "",
                                       paste0("font-size:", font_size, font_size_units, "; "))) %>%
    #* x. cell height and width
    dplyr::mutate_(height = ~ifelse(is.na(height), "",
                                    paste0("height:", height, height_units, "; ")),
                   width = ~ifelse(is.na(width), "",
                                   paste0("width:", width, width_units, "; ")),
                   top_border = ~ifelse(is.na(top_border), "",
                                        paste0("border-top:", top_border, "; ")),
                   bottom_border = ~ifelse(is.na(bottom_border), "",
                                           paste0("border-bottom:", bottom_border, "; ")),
                   left_border = ~ifelse(is.na(left_border), "",
                                         paste0("border-left:", left_border, "; ")),
                   right_border = ~ifelse(is.na(right_border), "",
                                          paste0("border-right:", right_border, "; ")),
                   rotate_degree = ~ifelse(is.na(rotate_degree), "",
                                    rotate_tag(rotate_degree)),
                   padding = ~ifelse(is.na(pad), "",
                                     paste0("padding:", pad, "px;"))) %>%
    dplyr::mutate_(value = ~gsub("[<]", " &lt; ", value),
                   value = ~gsub("[>]", " &gt; ", value),
                   value = ~paste0("<", dh, " style='", 
                                   bold, italic, halign, valign, bg, font_color, 
                                   font_size, height, width,
                                   top_border, bottom_border, left_border, right_border,
                                   rotate_degree, padding,
                                   "'>", value, "</", dh, ">")) %>%
    #* 5. Spread to wide format for printing
    dplyr::select_("row", "col", "value") %>%
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