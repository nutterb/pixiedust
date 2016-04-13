#' @name dust
#' @export dust
#' @importFrom ArgumentCheck newArgCheck
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom dplyr bind_cols
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_
#' @importFrom tidyr gather_
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
#' @param keep_rownames Whe \code{tidy_df} is \code{FALSE}, setting 
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
#' @return Returns an object of class \code{dust}
#'
#' @section Upcoming Developments:
#' \itemize{
#'   \item{dust_part }{A wrapper for extracting objects from a \code{dust} 
#'      object.  This is intended to assist in building custom heads and feet.}
#' }
#' 
#' @seealso \code{\link[broom]{tidy}} \code{\link{glance_foot}} \code{\link{tidy_levels_labels}}
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
                 justify = "center",
                 float = getOption("pixie_float", TRUE),
                 longtable = getOption("pixie_longtable", FALSE),
                 hhline = getOption("pixie_hhline", FALSE),
                 bookdown = getOption("pixie_bookdown", FALSE))
{
  Check <- ArgumentCheck::newArgCheck()
  
  #* By default, we assume data.frame-like objects are to be printed
  #* as given.  All other objects are tidied.
  if (!inherits(object, "data.frame") | tidy_df) 
    tidy_object <- broom::tidy(object, ...)

  else if (inherits(object, "data.frame")){
    if (inherits(object, "data.table"))
      object <- as.data.frame(object)
    if (keep_rownames){
      tidy_object <- cbind(rownames(object), object)
      rownames(tidy_object) <- NULL
      tidy_object[, 1] <- as.character(tidy_object[, 1])
      names(tidy_object)[1] <- ".rownames"
    }
    else{
      tidy_object <- object
    }
  }

  if (!inherits(object, "data.frame") & any(!descriptors %in% "term")){
    nms <- names(tidy_object)
    
    tidy_object <- tidy_levels_labels(object,
                                      descriptors = descriptors,
                                      numeric_level = numeric_level,
                                      argcheck = Check) %>%
      dplyr::left_join(tidy_object, .,
                       by = c("term" = "term"))
    
    if (!"term" %in% descriptors)
      nms <- nms[!nms %in% "term"]
    
    tidy_object <- dplyr::select_(tidy_object, .dots = c(descriptors, nms))
  }
  
  ArgumentCheck::finishArgCheck(Check)

  #* Create the table head
  head <- as.data.frame(t(names(tidy_object)),
                        stringsAsFactors=FALSE)
  names(head) <- names(tidy_object)

  if (glance_foot){
    foot <- glance_foot(object,
                        col_pairs = col_pairs,
                        total_cols = ncol(tidy_object),
                        glance_stats = glance_stats,
                        byrow = byrow) %>%
      component_table()
  }
  else {
    foot <- NULL
  }

  #* Eventually, by default, glance statistics will be inserted into
  #* the 'foot' object.  Objects passed as data frames should not have
  #* glance statistics by default.  Perhaps an option for glance_df should
  #* be provided here.

  print_method <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  
  if (is.null(print_method)) print_method <- getOption("pixiedust_print_method")
  
  structure(list(head = component_table(head, tidy_object),
                 body = component_table(tidy_object),
                 interfoot = NULL,
                 foot = foot,
                 border_collapse = TRUE,
                 caption = caption,
                 label = label,
                 justify = justify,
                 float = float,
                 longtable = longtable,
                 table_width = 6,
                 tabcolsep = 6,
                 hhline = hhline,
                 bookdown = bookdown,
                 print_method = print_method),
            class = "dust")

}

#' @rdname dust
#' @export

dust.grouped_df <- function(object, ungroup = TRUE, ...)
{
  if (ungroup)
  {
    dust.default(dplyr::ungroup(object), ...)
  }
  else
  {
    split_var <- attr(object, "var")
    object <- dplyr::ungroup(object)
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
                        col_class = vapply(object, primaryClass, character(1)), 
                        stringsAsFactors=FALSE)
  
  #* Initialize the table with row index, column index, and value
  tab <- gather_tbl(tbl)

  #* Initialize default values of table attributes
  tab <- dplyr::left_join(tab, cell_attributes_frame(nrow(tbl), ncol(tbl)),
              by = c("row" = "row", "col" = "col"))
  
  #* Join with column classes
  tab <- dplyr::left_join(tab, Classes,
              by = c("col_name" = "col_name"))
  return(tab)
}

#*********************************************

gather_tbl <- function(tbl)
{
  #* Assign the row indices
  dplyr::mutate_(tbl, row = ~1:n()) %>%
    #* Gather into a table with row (numeric), col (character), 
    #* and value (character)
    tidyr::gather_("col", "value", 
                   gather_cols=names(tbl)[!names(tbl) %in% "row"]) %>%
    #* Assign col_name as a factor.  Levels are in the same order as the column
    #*   appear in the broom output
    #* Extract numeric values of the col_name factor to get the column indices
    #* Recast col_name as a character
    dplyr::mutate_(col_name = ~factor(col, colnames(tbl)),
                   col = ~as.numeric(col_name),
                   col_name = ~as.character(col_name),
                   value = ~as.character(value))
}

#*********************************************

cell_attributes_frame <- function(nrow, ncol)
{
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
              rotate_degree = "",
              pad = "",
              rowspan = 1,
              colspan = 1,
              na_string = NA,
              stringsAsFactors=FALSE) %>%
    dplyr::mutate_(html_row = ~row,
            html_col = ~col,
            merge = ~FALSE)
}


primaryClass <- function(x){
  acceptedClasses <- c("integer", "double", "numeric", "character", "factor", "logical")
  class_vector <- class(x)
  class_vector[class_vector %in% acceptedClasses][1]
}


utils::globalVariables(c(".", "term"))
