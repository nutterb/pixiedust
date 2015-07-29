#' @name sprinkles
#' @importFrom ArgumentCheck newArgCheck
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' 
#' @title Define Customization to a Table
#' @description Customizations to a \code{dust} table are added by "sprinkling"
#'   with a little extra fairy dust.  Sprinkles are a collection of attributes
#'   to be applied over a subset of table cells.  They may be added to any 
#'   part of the table, or to the table as a whole.
#'   
#' @param rows A numeric vector specifying the rows of the table to sprinkle.
#'   See details for more about sprinkling.
#' @param cols A numeric (or character) vector specifying the columns (or 
#'   column names) to sprinkle.  See details for more about sprinkling.
#' @param part A character string denoting which part of the table to modify.
#' @param ... named arguments, each of length 1, defining the customizations
#'   for the given cells.  See "Sprinkles" for a listing of these arguments.
#'   
#' @details Sprinkling is done over the intersection of rows and columns.  If
#'   rows but no columns are specified, sprinkling is performed over all columns
#'   of the given given rows. The reverse is true for when columns but no rows
#'   are specified.  If neither columns nor rows are specified, the attribute 
#'   is applied over all of the cells in the table part denoted in \code{part}.
#'
#'   Whenever \code{part = "table"}, \code{rows} and \code{columns} are ignored
#'   and the attributes are applied to the entire table.
#'   
#' @section Sprinkles:
#' The following list describes the valid sprinkles that may be defined in the 
#' \code{...} dots argument.
#' 
#' \itemize{
#'   \item{\code{bg} }{A character string denoting the color 
#'      for the background.  See "Colors".}
#'   \item{\code{bg_pattern} }{This is one of the few exceptions to the length 1 rule.
#'      This accepts a vector of any length.  Background colors are recycled in a 
#'      pattern. See "Colors"}
#'   \item{\code{bg_pattern_by} }{A character string denoting if the background 
#'      pattern is recycled over rows or columns.  Accepts either \code{"rows"},
#'      or \code{"columns"} with partial matching and defaults to \code{"rows"}.
#'      if \code{bg_pattern} is provided, \code{bg_pattern_by} is assumed, meaning
#'      it is not necessary to explicitly define \code{bg_pattern_by} unless 
#'      changing an existing or default setting.}
#'   \item{\code{bold} }{}
#'   \item{\code{border} }{This is one of the few exceptions to the length 1 rule.  
#'      Accepts values \code{"left"}, \code{"right"}, \code{"top"}, and
#'      \code{"bottom"} with partial matching.  The border will be added
#'      to the sides indicated.}
#'   \item{\code{border_thickness} }{A numeric value denoting the thickness
#'      of the border.  Defaults to \code{1}.}
#'   \item{\code{border_units} }{A character string taking any one of the
#'      values \code{"px"} or \code{"pt"} with partial matching.  Defaults
#'      to \code{"px"}.}
#'   \item{\code{border_style} }{A character string taking any one of the
#'      values \code{"solid"}, \code{"dashed"}, \code{"dotted"}, 
#'      \code{"double"}, \code{"groove"}, \code{"ridge"}, \code{"inset"},
#'      \code{"outset"}, \code{"hidden"}, or \code{"none"}.  Defaults
#'      to \code{"solid"}.}
#'   \item{\code{border_color} }{A character string denoting the color 
#'      for the border.  See "Colors".}
#'   \item{\code{border_collapse} }{This element is only applicable to 
#'      \code{part = "table"} and will be applied to the table regardless
#'      the value of \code{part} in the call.}
#'   \item{\code{fn} }{}
#'   \item{\code{font_color} }{}
#'   \item{\code{font_size} }{}
#'   \item{\code{font_size_units} }{A character string giving the units 
#'     of the font size.  Accepts values \code{"px"}, \code{"pt"}, \code{"\%"},
#'     and \code{"em"}.  Defaults to \code{"px"}.}
#'   \item{\code{halign} }{}
#'   \item{\code{height} }{}
#'   \item{\code{height_units} }{}
#'   \item{\code{italic} }{}
#'   \item{\code{pad} }{}
#'   \item{\code{rotate_text} }{}
#'   \item{\code{round} }{}
#'   \item{\code{valign} }{}
#'   \item{\code{width} }{}
#'   \item{\code{width_units} }{}
#' }
#'
#' @section Colors:
#' Color specifications accept X11 color names (\code{"orchid"}), 
#' hexidecimal names (\code{"#DA70D6"}), rgb names (\code{"rgb(218 112 214)"}),
#' and rgba (rgb+alpha transparancy; \code{"rgba(218, 112, 214, .75)"}).
#' Refer to \url{https://en.wikipedia.org/wiki/Web_colors#X11_color_names}.
#'
#' @seealso \code{\link{sprinkle_colnames}} for changing column names in a table.
#' 
#' @author Benjamin Nutter
#'    
sprinkle <- function(rows, cols, ..., 
                     part = c("body", "head", "foot", "interfoot", "table"))
{
  Check <- ArgumentCheck::newArgCheck()
  part <- ArgumentCheck::match_arg(part, 
                                   c("body", "head", "foot", "interfoot", "table"),
                                   argcheck = Check)
  
  sprinkles <- list(...)
  
  if (length(sprinkles) == 0)
    ArgumentCheck::addError(
      msg = "No sprinkles declared. You must define at least one sprinkle in '...'",
      argcheck = Check)
  
  if (any(names(sprinkles) %in% ""))
    ArgumentCheck::addError(
      msg = paste0("All arguments in '...' must be named"),
      argcheck = Check)
  
  too_long <- names(sprinkles)[vapply(sprinkles, 
                                      function(x) length(x) != 1,
                                      TRUE)]
  if (length(too_long) > 0)
    ArgumentCheck::addError(
      msg = paste0("Arguments in '...' must have length 1.",
                   " Please check", paste0(too_long, collapse=", "), "."),
      argcheck = Check)
  
  bad_sprinkles <- names(sprinkles)[!names(sprinkles) %in% sprinkle_names()]
  if (length(bad_sprinkles) > 0)
    ArgumentCheck::addError(
      msg = paste0("The following arguments in '...' are not valid ",
                   "sprinkles: ", paste0(bad_sprinkles, collapse = ", ")),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(list(rows = rows,
                 cols = cols,
                 sprinkles = sprinkles,
                 part = part),
            class = "sprinkle")
}

#' @rdname sprinkles
#' @param print_method A character string giving the print method for the table.  
#' @export

sprinkle_print_method <- function(print_method = c("console", "markdown", "html", "latex"))
{
  
}
sprinkle_names <- function()
{
  c("bg", "bg_pattern", "bold", "border_collapse",
    "fn", "font_color", "font_size", "halign", 
    "height", "italic", "pad", "rotate_text", 
    "round", "valign", "width")
}
