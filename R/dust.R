#' @name dust
#' @export dust
#' 
#' @title Dust Table Construction
#' @description Dust tables consist of four primary components that are 
#'   built together to create a full table.  Namely, the \code{head}, the 
#'   \code{body}, the \code{interfoot}, and the \code{foot}.  Dust tables 
#'   also contain a table-wide attributes \code{border_collapse} and 
#'   \code{longtable} as well as a \code{print_method} element.
#'   
#' @param object An object that has a \code{tidy} method in \code{broom}
#' @param tidy_df When \code{object} is an object that inherits the 
#'   \code{data.frame} class, the default behavior is to assume that the 
#'   object itself is the basis of the table.  If the summarized table is 
#'   desired, set to \code{TRUE}.
#' @param keep_rownames When \code{tidy_df} is \code{FALSE}, setting 
#'   \code{keep_rownames} binds the row names to the data frame as the first
#'   column, allowing them to be preserved in the tabulated output.  This 
#'   is only to data frame like objects, as the \code{broom::tidy.matrix} method 
#'   performs this already.
#' @param glance_foot Arrange the glance statistics for the \code{foot} of the
#'   table. (Not scheduled for implementation until version 0.4.0)
#' @param glance_stats A character vector giving the names of the glance statistics
#'   to put in the output.  When \code{NULL}, the default, all of the available 
#'   statistics are retrieved.  In addition to controlling which statistics are 
#'   printed, this also controls the order in which they are printed.
#' @param col_pairs An integer indicating the number of column-pairings for the 
#'   glance output.  This must be less than half the total number of columns,
#'   as each column-pairing includes a statistic name and value. See the full
#'   documentation for the unexported function \code{\link{glance_foot}}.
#' @param byrow A logical, defaulting to \code{FALSE}, that indicates if the 
#'   requested statistics are placed with priority to rows or columns.  
#'   See the full documentation for the unexported function \code{\link{glance_foot}}.
#' @param descriptors A character vector indicating the descriptors to
#'   be used in the table.  Acceptable inputs are \code{"term"}, 
#'   \code{"term_plain"}, \code{"label"}, \code{"level"}, and 
#'   \code{"level_detail"}.  These may be used in any combination and
#'   any order, with the descriptors appearing in the table from left
#'   to right in the order given.  The default, \code{"term"}, returns
#'   only the term descriptor and is identical to the output provided
#'   by \code{broom::tidy} methods.  See Details for a full explanation
#'   of each option and the Examples for sample output.
#'   See the full documentation for the unexported function \code{\link{tidy_levels_labels}}.
#' @param numeric_level A character string that determines which descriptor
#'   is used for numeric variables in the \code{"level_detail"} descriptor
#'   when a numeric has an interaction with a factor.  Acceptable inputs
#'   are \code{"term"}, \code{"term_plain"}, and \code{"label"}.
#'   See the full documentation for the unexported function \code{\link{tidy_levels_labels}}.
#' @param caption A character string giving the caption for the table.
#' @param caption_number \code{logical(1)}. Should the table caption be prefixed 
#'   with the table number?
#' @param float A logical used only in LaTeX output.  When \code{TRUE}, the table is 
#'   set within a \code{table} environment.  The default is \code{TRUE}, as with 
#'   \code{xtable}.
#' @param longtable Allows the user to print a table in multiple sections.  
#'     This is useful when 
#'     a table has more rows than will fit on a printed page.  Acceptable inputs are \code{FALSE},
#'     indicating that only one table is printed (default); \code{TRUE} that the table should be 
#'     split into multiple tables with the default number of rows per table (see "Longtable"); or a 
#'     positive integer indicating how many rows per table to include. All other values are 
#'     interpreted as \code{FALSE}.  In LaTeX output, remember that after each section, a page 
#'     break is forced. This setting may also be set from \code{sprinkle}. 
#' @param hhline Logical.  When \code{FALSE}, the default, horizontal LaTeX cell borders 
#'   are drawn using the \code{\\cline} command.  These don't necessarily 
#'   play well with cell backgrounds, however.  Using \code{hhline = TRUE} 
#'   prints horizontal borders using the \code{\\hhline} command.  While the 
#'   \code{hhline} output isn't disrupted by cell backgrounds, it may require 
#'   more careful coding of the desired borders.  In \code{hhline}, cells with 
#'   adjoining borders tend to double up and look thicker than when using 
#'   \code{cline}.
#' @param label \code{character(1)}. An optional string for assigning labels with 
#'   which tables can be referenced elsewhere in the document.  If \code{NULL}, 
#'   \code{pixiedust} attempts to name the label \code{tab:[chunk-name]}, where 
#'   \code{[chunk-name]} is the name of the \code{knitr} chunk.  If this also
#'   resolves to \code{NULL} (for instance, when you aren't using \code{knitr}, 
#'   the label \code{tab:pixie-[n]} is assigned, where \code{[n]} is the current value 
#'   of \code{options()$pixie_count}.  Note that rendering multiple tables in a 
#'   chunk without specifying a label will result in label conflicts.
#' @param justify \code{character(1)}. Specifies the justification of the table on 
#'   the page.  May be \code{"center"} (default), \code{"left"}, or \code{"right"}.
#' @param bookdown Logical. When \code{TRUE}, \code{bookdown} style labels are
#'   generated.  Defaults to \code{FALSE}.
#' @param border_collapse \code{character(1)}. One of \code{"collapse"}, 
#'   \code{"separate"}, \code{"initial"}, or \code{"inherit"}.
#' @param tabcolsep \code{integerish(1)}. For LaTeX output, the distance in 
#'   \code{pt} between columns of the table.
#' @param fixed_header \code{logical(1)}. For HTML tables, should the 
#'   header rows be fixed in place over a scrollable body.
#' @param html_preserve \code{logical(1)}. When \code{TRUE}, HTML output is returned
#'   wrapped in \code{htmltools::htmlPreserve}. If using LaTeX style equations in 
#'   an HTML table, it may be necessary to set this to \code{FALSE}. Do this at
#'   your own risk; this has not been thoroughly field tested.
#' @param ... Additional arguments to pass to \code{tidy}
#' @param ungroup Used when a \code{grouped_df} object is passed to \code{dust}.
#'   When \code{TRUE} (the default), the object is ungrouped and dusted 
#'   as a single table. When \code{FALSE}, the object is split and each element
#'   is dusted separately.
#' 
#' @details The \code{head} object describes what each column of the table
#'   represents.  By default, the head is a single row, but multi row headers
#'   may be provided.  Note that multirow headers may not render in markdown
#'   or console output as intended, though rendering in HTML and LaTeX is 
#'   fairly reliable. In longtables (tables broken over multiple pages), 
#'   the \code{head} appears at the top of each table portion.
#'   
#'   The \code{body} object gives the main body of information.  In long tables,
#'   this section is broken into portions, ideally with one portion per page.
#'   
#'   The \code{interfoot} object is an optional table to be placed at the 
#'   bottom of longtable portions with the exception of the last portion.  A 
#'   well designed \code{interfoot} can convey to the user that the table 
#'   continues on the next page.
#'   
#'   The \code{foot} object is the table that appears at the end of the 
#'   completed table.  For model objects, it is recommended that the 
#'   \code{\link[broom]{glance}} statistics be used to display model fit 
#'   statistics.
#'   
#'   The \code{border_collapse} object applies to an entire HTML table.  It
#'   indicates if the borders should form a single line or distinct lines.
#'   
#'   The \code{longtable} object determines how many rows per page are printed.
#'   By default, all content is printed as a single table.  Using the 
#'   \code{longtable} argument in the \code{\link{sprinkle}} function can change this
#'   setting.
#'   
#'   The \code{table_width} element is specific to LaTeX tables.  This is a reference
#'   value for when column widths are specified in terms of the \code{\%} units.  For
#'   example, a column width of \code{20\%} will be defined as \code{table_width * .20}.
#'   The value in \code{table_width} is assumed to be in inches and defaults to 6.
#'   
#'   The \code{tabcolsep} object determines the spacing between columns in a 
#'   LaTeX table in pt.  By default, it is set at 6.
#'   
#'   The \code{print_method} object determines how the table is rendered when 
#'   the \code{print} method is invoked.  The default is to print to the 
#'   console.
#'   
#'   Many of these options may be set globally.  See 
#'   \code{\link{pixiedust}} for a complete list of package options.
#'   
#' @return Returns an object of class \code{dust}
#' 
#' @section Symbols and Greek Letters:
#' When using markdown, math symbols and greek letters may be employed as 
#' they would within a markdown document.  For example, \code{"$\alpha$"}
#' will render as the lower case Greek alpha.  Math symbols may be rendered
#' in the same manner.
#' 
#' @seealso \code{\link[broom]{tidy}} \code{\link{glance_foot}} 
#'   \code{\link{tidy_levels_labels}} \code{\link{pixiedust}}
#' 
#' \code{\link{get_dust_part}} for extracting parts of the \code{dust} object
#' in order to build custom headers and/or footers.
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' x

dust <- function(object, ...)
{
  UseMethod("dust")
}

#' @rdname dust
#' @export
dust.default <- function(object, ..., 
                 tidy_df = FALSE, keep_rownames = FALSE,
                 glance_foot = FALSE, glance_stats = NULL, 
                 col_pairs = 2, byrow = FALSE,
                 descriptors = "term", 
                 numeric_level = c("term", "term_plain", "label"),
                 label = NULL,
                 caption = NULL,
                 caption_number = getOption("pixied_caption_number", TRUE),
                 justify = getOption("pixie_justify", "center"),
                 float = getOption("pixie_float", TRUE),
                 longtable = getOption("pixie_longtable", FALSE),
                 hhline = getOption("pixie_hhline", FALSE),
                 bookdown = getOption("pixie_bookdown", FALSE),
                 border_collapse = getOption("pixie_border_collapse", "collapse"),
                 tabcolsep = getOption("pixie_tabcolsep", 6),
                 fixed_header = getOption("pixie_fixed_header", FALSE),
                 html_preserve = getOption("pixie_html_preserve", TRUE))
{
  coll <- checkmate::makeAssertCollection()
  
  descriptors <- checkmate::matchArg(x = descriptors,
                                  choices = c("term", "term_plain", "label",
                                              "level", "level_detail"),
                                  several.ok = TRUE,
                                  add = coll)
  
  #* By default, we assume data.frame-like objects are to be printed
  #* as given.  All other objects are tidied.
  if (!inherits(object, "data.frame") | tidy_df)
  {
    tidy_object <- as.data.frame(broom::tidy(object, ...))
  }
  else if (inherits(object, "data.frame"))
  {
    if (inherits(object, "data.table"))
    {
      object <- as.data.frame(object)
    }
    if (keep_rownames)
    {
      tidy_object <- cbind(rownames(object), 
                           object)
      rownames(tidy_object) <- NULL
      tidy_object[, 1] <- as.character(tidy_object[, 1])
      
      names(tidy_object)[1] <- ".rownames"
    }
    else
    {
      tidy_object <- object
    }
  }

  if (!inherits(object, "data.frame") & any(!descriptors %in% "term"))
  {
    nms <- names(tidy_object)
    
    tll <- tidy_levels_labels(object,
                              descriptors = descriptors,
                              numeric_level = numeric_level,
                              argcheck = coll) 
    tidy_object <- 
      merge(tidy_object, 
            tll, 
            by = "term", 
            all.x = TRUE)
      
     if ("label" %in% names(tidy_object))
     {
       is_intercept <- grepl(pattern = "([(]|)Intercept([)]|)", 
                             x = tidy_object[["term"]])
       
       tidy_object[["label"]][is_intercept] <- 
         tidy_object[["term"]][is_intercept]
     }

    if ("term_plain" %in% names(tidy_object))
    {
      is_intercept <- grepl(pattern = "([(]|)Intercept([)]|)", 
                            x = tidy_object[["term"]])
      
      tidy_object[["label"]][is_intercept] <- 
        tidy_object[["term_plain"]][is_intercept]
    }

    if (!"term" %in% descriptors)
    {
      nms <- nms[!nms %in% "term"]
    }
    
    tidy_object <- tidy_object[unique(c(descriptors, nms))]
  }

  checkmate::reportAssertions(coll)

  #* Create the table head
  head <- as.data.frame(t(names(tidy_object)),
                        stringsAsFactors=FALSE)
 
  names(head) <- names(tidy_object)

  if (glance_foot)
  {
    foot <- glance_foot(object,
                        col_pairs = col_pairs,
                        total_cols = ncol(tidy_object),
                        glance_stats = glance_stats,
                        byrow = byrow) %>%
      component_table()
  }
  else 
  {
    foot <- NULL
  }

  #* Eventually, by default, glance statistics will be inserted into
  #* the 'foot' object.  Objects passed as data frames should not have
  #* glance statistics by default.  Perhaps an option for glance_df should
  #* be provided here.
  
  out <- structure(list(head = component_table(head, tidy_object),
                 body = component_table(tidy_object),
                 interfoot = NULL,
                 foot = foot,
                 border_collapse = border_collapse,
                 caption = caption,
                 caption_number = caption_number,
                 label = label,
                 justify = justify,
                 float = float,
                 longtable = longtable,
                 table_width = 6,
                 tabcolsep = tabcolsep,
                 hhline = hhline,
                 bookdown = bookdown,
                 fixed_header = fixed_header,
                 include_fixed_header_css = FALSE, #Flag for if fixed header CSS 
                                            #should be generated with the table
                 fixed_header_param = 
                   list(
                     fixed_header_class_name = "pixie-fixed",
                     scroll_body_height = 300,
                     scroll_body_height_units = "px",
                     scroll_body_background_color = "white",
                     fixed_header_height = 20,
                     fixed_header_height_units = "px",
                     fixed_header_text_height = 10,
                     fixed_header_text_height_units = "px",
                     fixed_header_background_color = "white"
                   ),
                 html_preserve = html_preserve,
                 print_method = pixiedust_print_method()),
            class = "dust")

  out
}

#' @rdname dust
#' @export

dust.grouped_df <- function(object, ungroup = TRUE, ...)
{
  if (ungroup)
  {
    dust.default(as.data.frame(object), ...)
  }
  else
  {
    split_var <- attr(object, "var")
    # dplyr 0.8.0 replaces the var attribute with groups attribute
    if (is.null(split_var)){
      split_var <- utils::head(names(attr(object, "groups")), -1)
    }
    object <- as.data.frame(object)
    object <- split(object, object[, as.character(split_var)])
    dust.list(object, ...)
  }
}

#' @rdname dust
#' @export

dust.list <- function(object, ...)
{
  structure(
    lapply(X = object, 
           FUN = dust, 
           ...),
    class = "dust_list"
  )
}

#***********************************************************
#* Utilities

component_table <- function(tbl, object)
{
  #* Get the classes of each column in the data frame.
  #* These will be needed later for the 'round' sprinkle.
  if (missing(object)) object <- tbl
  
  Classes <- data.frame(col_name = colnames(object),
                        col_class = vapply(X = object, 
                                           FUN = primaryClass, 
                                           FUN.VALUE = character(1)), 
                        stringsAsFactors = FALSE)
  #* Initialize the table with row index, column index, and value
  tab <- gather_tbl(tbl)

  #* Initialize default values of table attributes
  tab <-
    merge(x = tab,
          y = cell_attributes_frame(nrow(tbl), ncol(tbl)),
          by = c("row", "col"),
          all.x = TRUE, 
          sort = FALSE)

  #* Join with column classes
  tab <- merge(x = tab,
               y = Classes,
               by = "col_name",
               all.x = TRUE, 
               sort = FALSE)

  return(tab)
}

#*********************************************

gather_tbl <- function(tbl)
{
  tbl_name <- names(tbl)
  #* Assign the row indices
  tbl[["row"]] <- seq_len(nrow(tbl))

  
  
  tbl <- stats::reshape(data = as.data.frame(tbl), 
                        direction = "long", 
                        varying = list(names(tbl)[!names(tbl) %in% "row"]))
  
  names(tbl)[names(tbl) %in% "time"] <- "col"
  names(tbl)[3] <- "value"
  tbl <- tbl[names(tbl)[!names(tbl) %in% "id"]]

    tbl$col_name <- factor(tbl$col, labels = tbl_name)
  tbl$col <- as.numeric(tbl$col_name)
  tbl$col_name <- as.character(tbl$col_name)
  tbl$value <- as.character(tbl$value)

  tbl
}

#*********************************************

cell_attributes_frame <- function(nrow, ncol)
{
  frame <- 
    expand.grid(row = 1:nrow,
                col = 1:ncol,
                fn = NA,
                round = "",
                bold = FALSE,
                italic = FALSE,
                halign = "",
                valign = "",
                bg = "",
                font_family = "",
                font_color = "",
                font_size = "",
                font_size_units = "",
                left_border = "",
                right_border = "",
                top_border = "",
                bottom_border = "",
                height = "",
                height_units = "",
                width = "",
                width_units = "",
                replace = NA,
                rotate_degree = "",
                sanitize = FALSE,
                sanitize_args = "",
                pad = "",
                rowspan = 1L,
                colspan = 1L,
                na_string = NA,
                stringsAsFactors=FALSE) 
  frame[["html_row"]] <- frame[["row"]]
  frame[["html_col"]] <- frame[["col"]]
  frame[["merge_rowval"]] <- frame[["row"]]
  frame[["merge_colval"]] <- frame[["col"]]
  frame[["merge"]] <- FALSE
  
  frame
}


primaryClass <- function(x)
{
  acceptedClasses <- c("integer", "double", "numeric", 
                       "character", "factor", "logical")
  class_vector <- class(x)
  class_vector[class_vector %in% acceptedClasses][1]
}

