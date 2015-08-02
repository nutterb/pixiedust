#' @name sprinkles
#' @export sprinkle
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
#'   If at least one of \code{border}, \code{border_thickness}, \code{border_units},
#'   \code{border_style} or \code{border_color} is specified, the remaining
#'   unspecified attributes assume their default values.
#'   
#'   The sprinkles `bg` and `bg_pattern` may not be used together.
#'   
#'   A more detailed demonstration of the use of sprinkles is available in 
#'   \code{vignette("pixiedust", package = "pixiedust")}
#'   
#' @section Sprinkles:
#' The following list describes the valid sprinkles that may be defined in the 
#' \code{...} dots argument.  All sprinkles may be defined for any output type, but 
#' only sprinkles recognized by that output type will be applied.  For a complete
#' list of which sprinkles are recognized by each output type, see 
#' \code{vignette("sprinkles", package = "pixiedust")}.
#' 
#' \itemize{
#'   \item{\code{bg} }{A character string denoting the color 
#'      for the background.  See "Colors".}
#'   \item{\code{bg_pattern} }{This is one of the few exceptions to the length 1 rule.
#'      This accepts a vector of any length.  Background colors are recycled in a 
#'      pattern. See "Colors". If left unspecified but \code{bg_pattern_by} is 
#'      specified, this will default to \code{c("white", "gainsboro")}.}
#'   \item{\code{bg_pattern_by} }{A character string denoting if the background 
#'      pattern is recycled over rows or columns.  Accepts either \code{"rows"},
#'      or \code{"columns"} with partial matching and defaults to \code{"rows"}.
#'      If \code{bg_pattern} is provided, \code{bg_pattern_by} is assumed, meaning
#'      it is not necessary to explicitly define \code{bg_pattern_by} unless 
#'      changing an existing or default setting.}
#'   \item{\code{bold} }{Logical value.  If \code{TRUE}, text is rendered in bold.}
#'   \item{\code{border_collapse} }{Logical.  Defaults to \code{TRUE}. 
#'      This element is only applicable to 
#'      \code{part = "table"} and will be applied to the table regardless
#'      the value of \code{part} in the call.}
#'   \item{\code{border} }{This is one of the few exceptions to the length 1 rule.  
#'      Accepts values \code{"left"}, \code{"right"}, \code{"top"}, 
#'      \code{"bottom"}, and \code{"all"} with partial matching.  The border will be added
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
#'   \item{\code{fn} }{A function to apply to values in cells.  The function 
#'      should be an expression that acts on the variable \code{value}. For
#'      example, \code{quote(round(value, 3))}.}
#'   \item{\code{font_color} }{A character string denoting the color of the
#'      font.  See "Colors".}
#'   \item{\code{font_size} }{A numeric value denoting the size of the font.}
#'   \item{\code{font_size_units} }{A character string giving the units 
#'     of the font size.  Accepts values \code{"px"}, \code{"pt"}, \code{"\%"},
#'     and \code{"em"}.  Defaults to \code{"px"}.}
#'   \item{\code{halign} }{A character string denoting the horizontal alignment.
#'     Accepts any one of the values \code{"left"}, \code{"center"}, or 
#'     \code{"right"}, with partial matching.}
#'   \item{\code{height} }{A numerical value giving the height of the cells.}
#'   \item{\code{height_units} }{A character string giving the units for the 
#'     \code{height} argument.  Accepts \code{"px"} and \code{"\%"}. Defaults
#'     to \code{"px"}.}
#'   \item{\code{italic} }{Logical value.  If \code{TRUE}, text is rendered in italics.}
#'   \item{\code{pad} }{A numerical value giving the cell padding in pixels.}
#'   \item{\code{rotate_degree} }{A numerical value that determines the angle of rotation
#'      in the clockwise direction.  Use negative values to rotate counter clockwise.}
#'   \item{\code{round} }{A numerical value for the number of decimal places to 
#'      which numerical values are rounded.  This can also be accomplished through
#'      the \code{fn} argument, but this argument makes it a bit easier to do.}
#'   \item{\code{valign} }{A character string giving the vertical alignment for the
#'      cells.  Accepts the values \code{"top"}, \code{"middle"}, or \code{"bottom"}
#'      with partial matching.}
#'   \item{\code{width} }{A numerical value giving the width of the cells.}
#'   \item{\code{width_units} }{A character string giving the units for the 
#'     \code{width} argument.  Accepts \code{"px"} and \code{"\%"}. Defaults
#'     to \code{"px"}.}
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
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' x + sprinkle(cols = 2:4, round = 3) + 
#'   sprinkle(cols = 5, fn = quote(pvalString(value))) + 
#'   sprinkle(rows = 2, bold = TRUE)
#'    
sprinkle <- function(rows = NULL, cols = NULL, ..., 
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
  
  if (any(names(sprinkles) %in% "")){
    unnamed_pos <- which(names(sprinkles) %in% "")
    ArgumentCheck::addError(
      msg = paste0("All arguments in '...' must be named. ",
                   "Check arguments in position(s) ", 
                   paste0(unnamed_pos, collapse=", ")),
      argcheck = Check)
  }
  
  if ("fn" %in% names(sprinkles))
    sprinkles$fn <- deparse(sprinkles$fn)
  
  too_long <- names(sprinkles)[vapply(sprinkles, 
                                      function(x) length(x) != 1,
                                      TRUE)]
  too_long <- too_long[!too_long %in% c("bg_pattern", "border")]
  
  if (length(too_long) > 0)
    ArgumentCheck::addError(
      msg = paste0("Arguments in '...' must have length 1.",
                   " Please check ", paste0(too_long, collapse=", "), "."),
      argcheck = Check)
  
  bad_sprinkles <- names(sprinkles)[!names(sprinkles) %in% sprinkle_names()]
  if (length(bad_sprinkles) > 0)
    ArgumentCheck::addError(
      msg = paste0("The following arguments in '...' are not valid ",
                   "sprinkles: ", paste0(bad_sprinkles, collapse = ", ")),
      argcheck = Check)
      
  if ("bg" %in% names(sprinkles) & !is.character(sprinkles[["bg"]]))
      ArgumentCheck::addError(
        msg = "The 'bg' argument must be a character string.",
        argcheck = Check)
  
  if (all(c("bg", "bg_pattern") %in% names(sprinkles)))
    ArgumentCheck::addError(
      msg = "The 'bg' and 'bg_pattern' arguments cannot be specified together",
      argcheck = Check)
  
  if ("bg_pattern_by" %in% names(sprinkles))
    sprinkles$bg_pattern_by <- ArgumentCheck::match_arg(sprinkles[["bg_pattern_by"]], c("rows", "columns"), 
                                                     argcheck = Check)
  
  if ("bold" %in% names(sprinkles) & !is.logical(sprinkles$bold))
    ArgumentCheck::addError(
      msg = "The 'bold' argument must be logical",
      argcheck = Check)
    
  if ("border" %in% names(sprinkles))
    sprinkles$border <- ArgumentCheck::match_arg(sprinkles[["border"]], 
                                                 c("all", "left", "right", "top", "bottom"),
                                                 several.ok = TRUE,
                                                 argcheck = Check)
  
  if ("border_thickness" %in% names(sprinkles) & !is.numeric(sprinkles$border_thickness))
    ArgumentCheck::addError(
      msg = "The 'border_thickness' arguments must be numeric",
      argcheck = Check)
  
  if ("border_units" %in% names(sprinkles))
    sprinkles$border_units <- ArgumentCheck::match_arg(sprinkles[["border_units"]],
                                                       c("px", "pt"),
                                                       argcheck = Check)
  
  if ("border_style" %in% names(sprinkles))
    sprinkles$border_style <- ArgumentCheck::match_arg(sprinkles[["border_style"]],
                                                       c("solid", "dashed", "dotted", "double", 
                                                         "grooved", "ridge", "inset", "outset",
                                                         "hidden"),
                                                       argcheck = Check)
  
  if ("border_color" %in% names(sprinkles) & !is.character(sprinkles$border_color))
    ArgumentCheck::addError(
      msg = "The 'border_color' argument must be a character string.",
      argcheck = Check)
  
  if ("border_collapse" %in% names(sprinkles) & !is.logical(sprinkles$border_collapse))
    ArgumentCheck::addError(
      msg = "The 'border_collapse' argument must be logical.",
      argcheck = Check)
      
  if ("font_color" %in% names(sprinkles) & !is.character(sprinkles$font_color))
    ArgumentCheck::addError(
      msg = "The 'font_color' argument must be a character string.",
      argcheck = Check)
  
  if ("font_size" %in% names(sprinkles) & !is.numeric(sprinkles$font_size))
    ArgumentCheck::addError(
      msg = "The 'font_size' argument must be numeric",
      argcheck = Check)
  
  if ("font_size_units" %in% names(sprinkles))
    sprinkles$font_size_units <- ArgumentCheck::match_arg(sprinkles[["font_size_units"]],
                                                          c("px", "pt", "%", "em"),
                                                          argcheck = Check)
  
  if ("halign" %in% names(sprinkles))
    sprinkles$halign <- ArgumentCheck::match_arg(sprinkles[["halign"]],
                                                 c("left", "center", "right"),
                                                 argcheck = Check)
  
  if ("height" %in% names(sprinkles) & !is.numeric(sprinkles[["height"]]))
    ArgumentCheck::addError(
      msg = "The 'height' argument must be numeric",
      argcheck = Check)
  
  if ("height_units" %in% names(sprinkles))
    sprinkles$height_units <- ArgumentCheck::match_arg(sprinkles[["height_units"]],
                                                       c("px", "%"),
                                                       argcheck = Check)
  
  if ("italic" %in% names(sprinkles) & !is.logical(sprinkles$italic))
    ArgumentCheck::addError(
      msg = "The 'italic' argument must be logical",
      argcheck = Check)
  
  if ("pad" %in% names(sprinkles) & !is.numeric(sprinkles$pad))
    ArgumentCheck::addError(
      msg = "The 'pad' argument must be numeric",
      argcheck = Check)
  
  if ("rotate_degree" %in% names(sprinkles) & !is.numeric(sprinkles$rotate_degree))
    ArgumentCheck::addError(
      msg = "The 'rotate_degree' argument must be numeric",
      argcheck = Check)
  
  if ("round" %in% names(sprinkles) & !is.numeric(sprinkles$round))
    ArgumentCheck::addError(
      msg = "The 'round' argument must be numeric",
      argcheck = Check)
  
  if ("valign" %in% names(sprinkles))
    sprinkles$valign <- ArgumentCheck::match_arg(sprinkles[["valign"]],
                                                 c("middle", "top", "bottom"),
                                                 argcheck = Check)
  
  if ("width" %in% names(sprinkles) & !is.numeric(sprinkles$width))
    ArgumentCheck::addError(
      msg = "The 'width' argument must be numeric",
      argcheck = Check)
  
  if ("width_units" %in% names(sprinkles))
    sprinkles$width_units <- ArgumentCheck::match_arg(sprinkles[["width_units"]],
                                                       c("px", "%"),
                                                       argcheck = Check)
  ArgumentCheck::finishArgCheck(Check)
  
  #* For borders, set unspecified attributes to their defaults
  border_attributes <- c("border", "border_thickness", "border_units", "border_style", "border_color")
  if (any(border_attributes %in% names(sprinkles)))
  {
    border_not_given <-  border_attributes[!border_attributes %in% names(sprinkles)]
    sprinkles[border_not_given] <- lapply(border_not_given,
                                            default_sprinkles)
  }
  
  if (any(sprinkles[["border"]] == "all")) sprinkles$border <- c("left", "right", "top", "bottom")
  
  if (is.null(sprinkles[["bg_pattern"]]) & !is.null(sprinkles$bg_pattern_by))
    sprinkles[["bg_pattern"]] <- default_sprinkles("bg_pattern")
  
  if (!is.null(sprinkles[["bg_pattern"]]) & is.null(sprinkles$bg_pattern_by))
    sprinkles$bg_pattern_by <- default_sprinkles("bg_pattern_by")
  
  if (!is.null(sprinkles[["font_size"]]) & is.null(sprinkles$font_size_units))
    sprinkles$font_size_units <- default_sprinkles("font_size_units")
  
  if (!is.null(sprinkles[["height"]]) & is.null(sprinkles$height_units))
    sprinkles$height_units <- default_sprinkles("height_units")
  
  if (!is.null(sprinkles[["width"]]) & is.null(sprinkles$width_units))
    sprinkles$width_units <- default_sprinkles("width_units")
  
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
  Check <- ArgumentCheck::newArgCheck()
  print_method <- ArgumentCheck::match_arg(print_method,
                                           c("console", "markdown", "html", "latex"),
                                           argcheck = Check)
  ArgumentCheck::finishArgCheck(Check)
  
  structure(print_method,
            class = c("print_method", "sprinkle"))
}

#****************
sprinkle_names <- function()
{
  c("bg", "bg_pattern", "bg_pattern_by", 
    "bold", "border", "border_thickness", 
    "border_units", "border_style", "border_color", 
    "border_collapse",
    "fn", "font_color", "font_size", "font_size_units", "halign", 
    "height", "height_units", "italic", "pad", "rotate_degree", 
    "round", "valign", "width", "width_units")
}

default_sprinkles <- function(setting)
{
  switch(setting,
         "bg_pattern" = c("white", "gainsboro"),
         "bg_pattern_by" = "rows",
         "border" = "all",
         "border_thickness" = 1,
         "border_units" = "px",
         "border_style" = "solid",
         "border_color" = "black",
         "font_size_units" = "px",
         "height_units" = "px",
         "width_units" = "px")
}
