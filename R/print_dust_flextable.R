print_dust_flextable <- function(x, ..., return_ft = FALSE)
{
  x[c("head", "body", "interfoot", "foot")] <- 
    lapply(x[c("head", "body", "interfoot", "foot")],
           applyReportersDefaults)
  
  
  ft <- flextable_body(x$body)

  if (!is.null(x$head))
  {
    head <- x$head[x$head$colspan > 0, ]
    for (i in unique(x$head$row))
    {
      row_match <- head$row == i
      ReporteRs::addHeaderRow(
        x = ft,
        value = head$value[row_match],
        colspan = head$colspan[row_match],
        text.properties = 
          ReporteRs::textProperties(
            color = head$font_color[row_match],
            font.size = as.numeric(head$font_size[row_match]),
            font.weight = head$font_weight[row_match],
            font.style = head$font_style[row_match],
            font.family = head$font_family[row_match]
          ),
        cell.properties = 
          ReporteRs::cellProperties(
            padding = as.numeric(head$pad[row_match]),
            
            border.bottom.color = head$border_bottom_color[row_match],
            border.bottom.style = head$border_bottom_style[row_match],
            border.bottom.width = head$border_bottom_width[row_match],
            border.left.color   = head$border_left_color[row_match],
            border.left.style   = head$border_left_style[row_match],
            border.left.width   = head$border_left_width[row_match],
            border.top.color    = head$border_top_color[row_match],
            border.top.style    = head$border_top_style[row_match],
            border.top.width    = head$border_top_width[row_match],
            border.right.color  = head$border_right_color[row_match],
            border.right.style  = head$border_right_style[row_match],
            border.right.width  = head$border_right_width[row_match],
            
            vertical.align   = head$valign[row_match],
            background.color = head$bg[row_match]
          )
      )
    }
  }
  
  if (return_ft) 
  {
    return(ft)
  }
  else
  {
    print(ft)
  }
}

#*********************************************
#*********************************************
#* Non exported functions


#* ReporteRs has specific defaults that often times conflict with 
#* the pixiedust philosophy of "do nothing until requested"
#* For example, ReporteRs, by default, puts black borders on all cells.
#* This function applies the appropriate values to each option to 
#* make it similar to the usual pixiedust output.

applyReportersDefaults <- function(x)
{
  if (is.null(x)) return(NULL)
  
  x$font_color[x$font_color == ""] <- "black"
  x$font_size[x$font_size == ""] <- getOption("ReporteRs-fontsize", 11)
  x$font_size <- as.numeric(x$font_size)
  x$font_weight <- ifelse(test = x$bold, "bold", "normal")
  x$font_style <- ifelse(test = x$italic, "italic", "normal")
  x$font_family[x$font_family == ""] <- getOption("ReporteRs-default-font", 
                                                  "Times New Roman")
  x$pad[x$pad == ""] <- 0
  x$pad <- as.numeric(x$pad)
  
  prop_mat_top    <- stringr::str_split_fixed(x$top_border,    " ", n = 3)
  prop_mat_bottom <- stringr::str_split_fixed(x$bottom_border, " ", n = 3)
  prop_mat_left   <- stringr::str_split_fixed(x$left_border,   " ", n = 3)
  prop_mat_right  <- stringr::str_split_fixed(x$right_border,  " ", n = 3)
  
  x$border_top_color    <- make_reporter_color(prop_mat_top[, 3])
  x$border_bottom_color <- make_reporter_color(prop_mat_bottom[, 3])
  x$border_left_color   <- make_reporter_color(prop_mat_left[, 3])
  x$border_right_color  <- make_reporter_color(prop_mat_right[, 3])
  
  x$border_top_style    <- prop_mat_top[, 2]
  x$border_bottom_style <- prop_mat_bottom[, 2]
  x$border_left_style   <- prop_mat_left[, 2]
  x$border_right_style  <- prop_mat_right[, 2]
  
  x$border_top_style[x$border_top_style == ""]       <- "none"
  x$border_bottom_style[x$border_bottom_style == ""] <- "none"
  x$border_left_style[x$border_left_style == ""]     <- "none"
  x$border_right_style[x$border_right_style == ""]   <- "none"
  
  x$border_top_width    <- stringr::str_extract(prop_mat_top[, 1],    "\\d{1,4}")
  x$border_bottom_width <- stringr::str_extract(prop_mat_bottom[, 1], "\\d{1,4}")
  x$border_left_width   <- stringr::str_extract(prop_mat_left[, 1],   "\\d{1,4}")
  x$border_right_width  <- stringr::str_extract(prop_mat_right[, 1],  "\\d{1,4}")
  
  x$border_top_width    <- as.numeric(x$border_top_width)
  x$border_bottom_width <- as.numeric(x$border_bottom_width)
  x$border_left_width   <- as.numeric(x$border_left_width)
  x$border_right_width  <- as.numeric(x$border_right_width)
  
  x$border_top_width[is.na(x$border_top_width)]       <- 1
  x$border_bottom_width[is.na(x$border_bottom_width)] <- 1
  x$border_left_width[is.na(x$border_left_width)]     <- 1
  x$border_right_width[is.na(x$border_right_width)]   <- 1
  
  x$bg <- make_reporter_color(x$bg)
  x$bg[x$bg == ""] <- "transparent"
  
  x$valign[x$valign == ""] <- "middle"
  
  x
}

#*******************************************************
#* Generates the body of the FlexTable object

flextable_body <- function(body)
{
  body_prep <- part_prep_flextable(body)
  
  ft <- ReporteRs::FlexTable(body_prep,
                             header.columns = FALSE,
                             body.cell.props = 
                               ReporteRs::cellProperties(
                                 border.style = "none"
                               )
  )
  
  #* Bold, Italic, font color, font family, font size, and vertical alignment
  formatting <- 
    body[body$bold | 
           body$italic | 
           body$font_color != "black" | 
           body$font_family != "Times New Roman" | 
           body$font_size != getOption("ReporteRs-default-font", 
                                       "Times New Roman"), ]
  
  for (i in 1:nrow(formatting))
  {
    ft[formatting$row[i], formatting$col[i], to = "body"] <- 
      ReporteRs::textProperties(
        color = formatting$font_color[i],
        font.size = as.numeric(formatting$font_size[i]),
        font.weight = formatting$font_weight[i],
        font.style = formatting$font_style[i],
        font.family = formatting$font_family[i]
      )
  }
  
  #* Cell Borders, vertical alignment, and background
  borders <- 
    body[body$bg != "transparent" | 
           body$top_border != "transparent" | 
           body$bottom_border != "transparent" | 
           body$left_border != "transparent" | 
           body$right_border != "transparent" | 
           body$pad != 0 | 
           body$valign != "center", ]
  
  for(i in 1:nrow(borders))
  {
    ft[borders$row[i], borders$col[i], to = "body"] <- 
      ReporteRs::cellProperties(
        padding = as.numeric(borders$pad[i]),
        
        border.bottom.color = borders$border_bottom_color[i],
        border.bottom.style = borders$border_bottom_style[i],
        border.bottom.width = borders$border_bottom_width[i],
        border.left.color   = borders$border_left_color[i],
        border.left.style   = borders$border_left_style[i],
        border.left.width   = borders$border_left_width[i],
        border.top.color    = borders$border_top_color[i],
        border.top.style    = borders$border_top_style[i],
        border.top.width    = borders$border_top_width[i],
        border.right.color  = borders$border_right_color[i],
        border.right.style  = borders$border_right_style[i],
        border.right.width  = borders$border_right_width[i],
        
        vertical.align   = borders$valign[i],
        background.color = borders$bg[i]
      )
    
  }
  
  ft
}

#***************************************************
#* Converts all colors to a #XXYYZZAA format

make_reporter_color <- function(col)
{
  each_color <- function(col)
  {
    if (col == "")
    {
      col <- "transparent"
    }
    else if (grepl("^rgb", col))
    {
      col <- gsub("([[:alpha:]]+|[(]|[)])", "", col) %>%
        stringr::str_split_fixed(",", n=4) %>%
        trimws()
      col[, 1:3] <- 
        as.numeric(col[, 1:3]) %>%
        as.hexmode() %>%
        toupper() %>%
        stringr::str_pad(2, pad = "0")
      
      col[, 4] <- ifelse(col[, 4] != "", 
                         as.numeric(col[, 4]) * 100,
                         col[, 4])
      apply(col, 1, paste, collapse = "") %>%
        paste0("#", .)
    }
    else{
      col
    }
  }
  
  vapply(col, each_color, character(1))
}

#****************************************************
#* A reduced version of the part_prep function used
#* in the other print methods.  Applies rounding, 
#* functions, and na_strings, then returns the 
#* data frame.

part_prep_flextable <- function(part)
{
  #* values in the dust object are all stored as character strings.
  #* These classes need to be converted to numerics for rounding
  #* to have the appropriate effect.
  numeric_classes <- c("double", "numeric")
  
  #* If functions are assigned, perform the function.
  part <- perform_function(part)
  
  #* Perform any rounding
  logic <- part$round != "" & part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <- 
    with(part, as.character(roundSafe(value[logic], as.numeric(round[logic]))))

  #* Replace missing values with na_string sprinkle  
  part$value <- with(part, 
                     ifelse(test = is.na(value) & !is.na(na_string),
                            yes = na_string,
                            no = value))
  

  #* Spread to wide format for printing
  part %>%
  dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
}






