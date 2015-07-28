#' @name print.dust
#' @export 
#' @importFrom dplyr group_by_
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_
#' @importFrom dplyr rename_
#' @importFrom dplyr select_
#' @importFrom dplyr ungroup
#' @importFrom knitr asis_output
#' @importFrom knitr kable
#' @importFrom tidyr spread_
#' @method print dust
#' 
#' @title Print A \code{dust} Table
#' @details Apply the formatting to a \code{dust} object and print the table.
#' 
#' @param x An object of class \code{dust}
#' @param ... Additional arguments to pass to the print method.  Currently ignored.
#' 
#' @details The printing format is drawn from \code{options()$dustpan_output} and may take any of
#'   the values \code{"console"}, \code{"markdown"}, \code{"html"}, or \code{"latex"}
#'   
#' @author Benjamin Nutter
#' 
#' @examples 
#' dust(lm(mpg ~ qsec + factor(am), data = mtcars))

print.dust <- function(x, ...)
{
  switch(x$print_method,
        "console" = print_dust_console(x, ...),
        "markdown" = print_dust_markdown(x, ...),
        "html" = print_dust_html(x, ...),
#         "latex" = print_dust_latex(x, ...),
        stop(paste0("'", x$print_method, "' is not an valid option")))
}

#*************************************************
#*************************************************
#* Print Console Output
#*************************************************
#*************************************************

print_dust_console <- function(x, ...)
{
  #************************************************
  #* 1. apply a function, if any is indicated
  #* 2. Perform any rounding
  #* 3. Bold
  #* 4. Italic
  #* 5. Spread to wide format for printing
  #* 6. Column Names
  #************************************************
  
  numeric_classes <- c("double", "numeric")

  #* 1. apply a function, if any is indicated
  body <- perform_function(x$body)

  #* 2. Perform any rounding
  body <- body %>%
    dplyr::mutate_(
      value = ~suppressWarnings(
               ifelse(!is.na(round) & col_class %in% numeric_classes,
                      as.character(round(as.numeric(value), round)),
                      value)))

  #* 3. Bold
  body <- dplyr::group_by_(body, "col_name") %>%
    dplyr::mutate_(value = ~if (any(bold)) ifelse(bold, 
                                                  paste0("**", value, "**"),
                                                  paste0("  ", value, "  "))
                            else value) %>%
    dplyr::ungroup()

  #* 4. Italic
  body <- dplyr::group_by_(body, "col_name") %>%
    dplyr::mutate_(value = ~if (any(italic)) ifelse(italic, 
                                                  paste0("_", value, "_"),
                                                  paste0(" ", value, " "))
                   else value) %>%
    dplyr::ungroup()

  #* 5. Spread to wide format for printing
  body <- body %>%
    dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")

  #* 6. Column Names     
  colnames(body) <- x$head$col_title
  
  print(as.data.frame(body))
}

#*************************************************
#*************************************************
#* Print Markdown Output
#*************************************************
#*************************************************

print_dust_markdown <- function(x, ...)
{
  numeric_classes <- c("double", "numeric")
  
  #************************************************
  #* 1. apply a function, if any is indicated
  #* 2. Perform any rounding
  #* 3. Bold
  #* 4. Italic
  #* 5. Spread to wide format for printing
  #* 6. Column Names
  #************************************************
  
  #* 1. apply a function, if any is indicated
  body <- perform_function(x$body)

  #* 2. Perform any rounding
  body <- body %>%
    dplyr::mutate_(
      value = ~suppressWarnings(
        ifelse(!is.na(round) & col_class %in% numeric_classes,
               as.character(round(as.numeric(value), round)),
               value)))

  #* 3. Bold
  if (any(body$bold))
    body <- dplyr::mutate_(body, 
                   value = ~ifelse(bold, 
                                   paste0("**", value, "**"), 
                                   value))
  
  #* 4. Italic
  if (any(body$italic))
    body <- dplyr::mutate_(body, 
                   value = ~ifelse(italic, 
                                   paste0("_", value, "_"), 
                                   value))

  #* 5. Spread to wide format for printing
  body <- body %>%
    dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
  
  #* 6. Column Names 
  x$head <- dplyr::mutate_(x$head,
                           halign = ~set_halign_tag(halign, col_class, "html"),
                           halign = ~paste0("text-align:", halign, ";"))
                           
  colnames(body) <- x$head$col_title

  knitr::asis_output(
    paste(c("", "", knitr::kable(body,
                                 format = "markdown",
                                 align = x$head$halign)), 
          collapse = "\n"))
}


#*************************************************
#*************************************************
#* Print HTML Output
#*************************************************
#*************************************************

print_dust_html <- function(x, ...)
{
  numeric_classes <- c("double", "numeric")
  
  #* Table Head Attributes
  
  x$table_border[-1] <- 
  lapply(names(x$table_border[-1]),
         function(nm){ 
           if (x$table_border[[nm]] != "") 
               x$table_border[[nm]] <- paste0("border-", nm, ":", x$table_border[[nm]], "; ")
           return(x$table_border[[nm]])
         })
  table_attr <- 
    paste0("<table style = '",
           "border-collapse:", x$table_border$border_collapse, ";",
           x$table_border$top, 
           x$table_border$bottom, 
           x$table_border$left, 
           x$table_border$right, 
           "'>")
                       
  
  #* Heading Column Alignment
  #* This must be set first and then applied to cell data where alignment is not declared.
  x$head <- dplyr::mutate_(x$head,
                           halign = ~set_halign_tag(halign, col_class, "html"),
                           halign = ~paste0("text-align:", halign, ";"))
 
  #************************************************
  #* 1. apply a function, if any is indicated
  #* 2. Perform any rounding
  #* 3. Bold
  #* 4. Italic
  #* 5. Spread to wide format for printing
  #* 6. Column Names
  #************************************************
  
  #* 1. apply a function, if any is indicated
  body <- perform_function(x$body)

  #* 2. Perform any rounding
  body <- body %>%
    dplyr::mutate_(
      value = ~suppressWarnings(
        ifelse(!is.na(round) & col_class %in% numeric_classes,
               as.character(round(as.numeric(value), round)),
               value)))

  #* 3. Bold
  body <- dplyr::mutate_(body, 
                        bold = ~ifelse(bold, 
                                       "font-weight:bold;",
                                       ""))

  #* 4. Italic
  body <- dplyr::mutate_(body, 
                 italic = ~ifelse(italic, 
                                 "font-style:italic;", 
                                 ""))
  
  #** Alignments
  body <- dplyr::mutate_(body,
           halign = ~expand_halign_tag(halign),
           halign = ~ifelse(is.na(halign), 
                            "",
                            paste0("text-align:", halign, ";"))) %>%
    dplyr::left_join(x$head[, c("col_name", "halign")],
                     by = c("col_name" = "col_name")) %>%
    dplyr::rename_("halign" = "halign.x") %>%
    dplyr::mutate_(halign = ~ifelse(is.na(halign),
                                    halign.y,
                                    halign)) %>%
    dplyr::select_("-halign.y")
  
  body <- dplyr::mutate_(body,
                         valign = ~expand_valign_tag(valign),
                         valign = ~ifelse(is.na(valign), 
                                          "",
                                          paste0("vertical-align:", valign, ";"))) %>%
    dplyr::left_join(x$head[, c("col_name", "valign")],
                     by = c("col_name" = "col_name")) %>%
    dplyr::rename_("valign" = "valign.x") %>%
    dplyr::mutate_(halign = ~ifelse(is.na(valign),
                                    valign.y,
                                    valign)) %>%
    dplyr::select_("-valign.y")
  
  #** Background
  body <- dplyr::mutate_(body,
            bg = ~ifelse(is.na(bg), "",
                         paste0("background-color: ", bg, ";")))

  #* x. Font Color
  body <- dplyr::mutate_(body,
            font_color = ~ifelse(is.na(font_color), "",
                                 paste0("color:", font_color, "; ")))
  
  #* x. Font size
  body <- dplyr::mutate_(body,
            font_size = ~ifelse(is.na(font_size), "",
                                paste0("font-size:", font_size, "; ")))
  
  #* x. cell height and width
  body <- dplyr::mutate_(body,
            cell_height = ~ifelse(is.na(cell_height), "",
                                  paste0("height:", cell_height, "; ")),
            cell_width = ~ifelse(is.na(cell_width), "",
                                 paste0("width:", cell_width, "; ")),
            top_border = ~ifelse(is.na(top_border), "",
                                 paste0("border-top:", top_border, "; ")),
            bottom_border = ~ifelse(is.na(bottom_border), "",
                                    paste0("border-bottom:", bottom_border, "; ")),
            left_border = ~ifelse(is.na(left_border), "",
                                  paste0("border-left:", left_border, "; ")),
            right_border = ~ifelse(is.na(right_border), "",
                                   paste0("border-right:", right_border, "; ")))
      
  body <- dplyr::mutate_(body, 
      value = ~gsub("[<]", " &lt; ", value),
      value = ~gsub("[>]", " &gt; ", value),
      value = ~paste0("<td style='", 
                        bold, italic, halign, valign, bg, font_color, 
                        font_size, cell_height, cell_width,
                        top_border, bottom_border, left_border, right_border,
                      "'>", value, "</td>"))

  #* 5. Spread to wide format for printing
  body <- body %>%
    dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")

  #* 6. Column Names   
  colnames(body) <- x$head$col_title
  

  
  knitr::asis_output(
    paste0(table_attr, "\n<thead><tr>",
           paste0(paste0("<th style='",
                         x$head$halign, "'> ", 
                         colnames(body), 
                         "</th>"), 
                  collapse="\n"),
           "</thead>\n<tbody>\n",
           paste0(paste0("<tr> ", 
                         apply(body, 1, paste0, collapse = "\n"), 
                         " </tr>"),
                  collapse="\n"),
           "</tbody>\n</table>"))
}

#******************
set_halign_tag <- function(halign, col_class, format)
{
  halign <- ifelse(is.na(halign),
                   ifelse(col_class %in% c("integer", "numeric", "double"),
                          "r", "l"),
                   halign)
  if (format == "html") halign <- expand_halign_tag(halign)
  halign
}

expand_halign_tag <- function(tag)
{
  tag <- ifelse(is.na(tag), "na", tag)
  for (i in 1:length(tag)){
    tag[i] <- switch(tag[i],
                        "l" = "left",
                        "c" = "center",
                        "r" = "right",
                        "na" = NA,
                        stop("Invalid halign tag"))
  }
  tag
}

set_valign_tag <- function(valign, col_class, format)
{
  valign <- ifelse(is.na(valign),
                   "m",
                   valign)
  if (format == "html") valign <- expand_valign_tag(valign)
  valign
}

expand_valign_tag <- function(tag)
{
  tag <- ifelse(is.na(tag), "na", tag)
  for (i in 1:length(tag)){
    tag[i] <- switch(tag[i],
                     "m" = "middle",
                     "b" = "bottom",
                     "t" = "top",
                     "na" = NA,
                     stop("Invalid valign tag"))
  }
  tag
}
