#' @name sprinkle
#' @export sprinkle
#' @importFrom ArgumentCheck newArgCheck
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck addMessage
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom ArgumentCheck match_arg
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom tidyr spread_
#' 
#' @title Define Customizations to a Table
#' @description Customizations to a \code{dust} table are added by "sprinkling"
#'   with a little extra pixie dust.  Sprinkles are a collection of attributes
#'   to be applied over a subset of table cells.  They may be added to any 
#'   part of the table, or to the table as a whole.
#'   
#' @param x A dust object
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
#'   and the attributes are applied to the entire table. This feature is not
#'   yet implemented (2015-08-05) and may be removed, depending on how useful 
#'   it turns out to be.
#'
#'   If at least one of \code{border}, \code{border_thickness}, \code{border_units},
#'   \code{border_style} or \code{border_color} is specified, the remaining
#'   unspecified attributes assume their default values.
#'   
#'   Other sprinkle pairings are \code{height} and \code{height_units}; 
#'   \code{width} and \code{width_units}; \code{font_size} and \code{font_size_units};
#'   \code{bg_pattern} and \code{bg_pattern_by}
#'   
#'   The sprinkles \code{bg} and \code{bg_pattern} may not be used together.
#'   
#'   A more detailed demonstration of the use of sprinkles is available in 
#'   \code{vignette("pixiedust", package = "pixiedust")}
#'   
#'   In \code{sprinkle}, when \code{part = "table"}, the attributes are assigned to 
#'   the entire table.  This is not yet active and may be removed entirely.
#'   
#'   The \code{sprinkle_table}, sprinkles may be applied to the columns of multiple tables. Table
#'   parts are required to have the same number of columns, but not necessarily the same number 
#'   of rows, which is why the \code{rows} argument is not available for the \code{sprinkle_table}.
#'   In contrast to \code{sprinkle}, the \code{part} argument in \code{sprinkle} table will 
#'   accept multiple parts.  If any of the named parts is \code{"table"}, the sprinkle will be 
#'   applied to the columns of all of the parts.
#'   
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
#'      specified, this will default to \code{c("White", "Gray")}.}
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
#'      to the sides indicated. In LaTeX output with merged cells, horizontal 
#'      borders are placed on all rows identified, making it possible to run 
#'      borders through the merged cell.}
#'   \item{\code{border_thickness} }{A numeric value denoting the thickness
#'      of the border.  Defaults to \code{1}.  This setting is ignored in
#'      LaTeX output when \code{border_style = "dashed"} and for all
#'      horizontal borders.}
#'   \item{\code{border_units} }{A character string taking any one of the
#'      values \code{"px"} or \code{"pt"} with partial matching.  Defaults
#'      to \code{"px"}.  For LaTeX output, both \code{"px"} and \code{"pt"},
#'      are rendered as \code{"pt"}.}
#'   \item{\code{border_style} }{A character string taking any one of the
#'      values \code{"solid"}, \code{"dashed"}, \code{"dotted"}, 
#'      \code{"double"}, \code{"groove"}, \code{"ridge"}, \code{"inset"},
#'      \code{"outset"}, \code{"hidden"}, or \code{"none"}.  Defaults
#'      to \code{"solid"}. LaTeX output only makes use of \code{"solid"}, 
#'      \code{"dashed"}, and \code{"none"}. If \code{"dotted"},
#'      is passed to LaTeX output, it is quietly changed to \code{"dashed"}. All
#'      other options are quietly changed to \code{"solid"}.}
#'   \item{\code{border_color} }{A character string denoting the color 
#'      for the border.  See "Colors".}
#'   \item{\code{fn} }{A function to apply to values in cells.  The function 
#'      should be an expression that acts on the variable \code{value}. For
#'      example, \code{quote(round(value, 3))}.}
#'   \item{\code{font_family} }{A character string denoting the font family 
#'      for the text. This option is only recognized in HTML output.
#'      A short list of web safe fonts is available at 
#'      http://www.w3schools.com/cssref/css_websafe_fonts.asp}
#'   \item{\code{font_color} }{A character string denoting the color of the
#'      font.  See "Colors".}
#'   \item{\code{font_size} }{A numeric value denoting the size of the font.}
#'   \item{\code{font_size_units} }{A character string giving the units 
#'     of the font size.  Accepts values \code{"px"}, \code{"pt"}, \code{"\%"},
#'     and \code{"em"}.  Defaults to \code{"pt"}.  LaTeX formats only recognize
#'     \code{"pt"} and \code{"em"}, and other units specifications will be 
#'     coerced to \code{"pt"}, which may result in an unexpected appearance.}
#'   \item{\code{halign} }{A character string denoting the horizontal alignment.
#'     Accepts any one of the values \code{"left"}, \code{"center"}, or 
#'     \code{"right"}, with partial matching.}
#'   \item{\code{height} }{A numerical value giving the height of the cells.}
#'   \item{\code{height_units} }{A character string giving the units for the 
#'     \code{height} argument.  Accepts \code{"px"}, \code{"pt"}, \code{"cm"}, 
#'     \code{"in"} and \code{"\%"}. Defaults to \code{"pt"}.  LaTeX formats
#'     do not recognize \code{"px"} and this will be coerced to \code{"pt"} when
#'     submitted for LaTeX output.}
#'   \item{\code{italic} }{Logical value.  If \code{TRUE}, text is rendered in italics.}
#'   \item{\code{longtable} }{ Allows the user to print a table in multiple sections.  
#'     This is useful when 
#'     a table has more rows than will fit on a printed page.  Acceptable inputs are \code{FALSE},
#'     indicating that only one table is printed (default); \code{TRUE} that the table should be 
#'     split into multiple tables with the default number of rows per table (see "Longtable"); or a 
#'     positive integer indicating how many rows per table to include. All other values are 
#'     interpreted as \code{FALSE}.  In LaTeX output, remember that after each section, a page 
#'     break is forced.}
#'   \item{\code{merge} }{Logical.  If \code{TRUE}, the cells indicated in 
#'     \code{rows} and \code{cols} are merged into a single cell.  An error is
#'     cast if the cells do not form an adjacent block. Specifying 
#'     \code{merge_rowval} or \code{merge_colval} without \code{merge} results
#'     in an error; \code{pixiedust} is conservative and will not assume you 
#'     mean to merge cells--it must be explicitly declared.}
#'   \item{\code{merge_rowval} }{A numeric value of length 1 indicating the 
#'     row position of the merged cells with the desired display text.  The 
#'     value given must be an element of \code{rows}.  If no value is provided,
#'     the smallest value of \code{rows} is used.}
#'   \item{\code{merge_colval} }{A numeric value of length 1 indicating the 
#'     column position of the merged cells with the desired display text. The
#'     value given must be an element of \code{cols}.  If no value is provided,
#'     the smallest value of \code{cols} is used.}
#'   \item{\code{na_string} }{A character value of length 1. Specifies how missing
#'     values (\code{NA}) are to be represented in the table.  Defaults to an 
#'     empty character string (\code{""}).}
#'   \item{\code{pad} }{A numerical value giving the cell padding in pixels.}
#'   \item{\code{replace} }{A character vector (or vector to be coerced to character) that
#'     will replace the cells identified by \code{rows} and \code{cols}.  Replacement 
#'     is always performed moving down columns first, then across rows from left to right.
#'     The operating assumption is that the most frequent use of this argument will be 
#'     to replace entire columns.}
#'   \item{\code{rotate_degree} }{A numerical value that determines the angle of rotation
#'      in the clockwise direction.  Use negative values to rotate counter clockwise.}
#'   \item{\code{round} }{A numerical value for the number of decimal places to 
#'      which numerical values are rounded.  This can also be accomplished through
#'      the \code{fn} argument, but this argument makes it a bit easier to do.  In cases where
#'      character values are indicated for rounding (such a a term name), no action is taken.  
#'      This means that `sprinkle(x, round=3)` would round all numerical values in a table to three 
#'      decimal places without affecting any true character values; there is no need to limit
#'      the `round` sprinkle to known numerical values.}
#'   \item{\code{tabcolsep} }{A numerical value setting the space in \code{pt} between 
#'      columns in the table.}
#'   \item{\code{valign} }{A character string giving the vertical alignment for the
#'      cells.  Accepts the values \code{"top"}, \code{"middle"}, or \code{"bottom"}
#'      with partial matching.}
#'   \item{\code{width} }{A numerical value giving the width of the cells.}
#'   \item{\code{width_units} }{A character string giving the units for the 
#'     \code{width} argument.  Accepts \code{"px"}, \code{"pt"}, \code{"cm"}, 
#'     \code{"in"} and \code{"\%"}. Defaults to \code{"px"}.  LaTeX formats
#'     do not recognize \code{"px"} and this will be coerced to \code{"pt"} when
#'     submitted for LaTeX output.}
#' }
#' 
#' @section Longtable:
#' The \code{longtable} feature is named for the LaTeX package used to break very large 
#' tables into multiple pages.  
#' 
#' When using the \code{longtable=TRUE} option, the default number of rows per table is 25 for 
#' console, HTML, and markdown output.  For LaTeX output, the number of rows is determined by 
#' the LaTeX \code{longtable} package's algorithm. The number of rows per table only considers 
#' the content in the body of the table.  Consideration for the number of rows in the head and 
#' foot are the responsibility of the user.
#'   
#' Whenever a table is broken into multiple parts, each part retains the table head.  If any 
#' \code{interfoot} is provided, it is appended to the bottom of each section, with the 
#' exception of the last section.  The last section has the \code{foot} appended.
#'
#' @section HTML Colors:
#' Color specifications accept X11 color names (\code{"orchid"}), 
#' hexidecimal names (\code{"#DA70D6"}), rgb names (\code{"rgb(218 112 214)"}),
#' and rgba (rgb+alpha transparency; \code{"rgba(218, 112, 214, .75)"}).
#' Refer to \url{https://en.wikipedia.org/wiki/Web_colors#X11_color_names}.
#' 
#' HTML color names are not case sensitive, but the color names in LaTeX output
#' are.  If you desire to be able to toggle your output between HTML and LaTeX,
#' it is recommended that you use the color names under the dvips section of 
#' page 38 of the LaTeX package \code{xcolor} manual 
#' (\url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/xcolor/xcolor.pdf}.
#' 
#' @section LaTeX Colors:
#' Use of color in LaTeX requirements requires that you have the LaTeX \code{color}
#' package included in your document preamble (\code{\\usepackage\{color\}}). 
#' Rmarkdown documents include the color package automatically. The 
#' standard colors available in LaTeX are "white", "black", "red", "green", 
#' "blue", "cyan", "magenta", and "yellow".
#' 
#' Additional colors may be made available using the LaTeX package \code{xcolor}.
#' To be consistent with color names used in the HTML tables, it is recommended
#' that you use the option \code{\\usepackage[dvipsnames]\{xcolor\}} in your 
#' preamble.  Please note that color names in LaTeX are case-sensitive, but the 
#' HTML names are not.  If the ability to switch between output methods is 
#' something you desire, you should adopt the capitalization used in the dvips 
#' names (See page 38 of the \code{xcolor} manual; 
#' \url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/xcolor/xcolor.pdf}). 
#' 
#' If desired, you may also use the \code{[x11names]} option to have the X11 
#' color names available to you.
#' 
#' The LaTeX output will accept hexidecimal names (\code{"#DA70D6"}) and 
#' rgb names (\code{"rgb(218 112 214)"}), similar to the HTML colors described
#' above.  However, transparency is not supported.  If the transparency 
#' value is provided, it is silently ignored.  
#' 
#' Custom color definitions may also be defined by defining the color in the
#' preamble.  The process for color definitions is described in the \code{xcolor}
#' documentation.  Keep in mind that custom color designations in LaTeX output
#' will not transfer the other output formats.
#' 
#' @section Required LaTeX Packages:
#' If you will be using the LaTeX output, some sprinkles will require you 
#' to include additional LaTeX packages in your document preamble.  In 
#' \code{.Rnw} files, additional packages can be included with the 
#' \code{\\usepackage\{[package]\}} syntax.  In markdown, additional packages
#' are included using \code{header-includes:} in the YAML front matter with 
#' a line of the format \code{\\usepackage\{[package]\}} for each package to 
#' be used.  Sprinkles that require additional packages, and the LaTeX packages
#' required, are listed below:
#' 
#' \tabular{ll}{
#'   Sprinkle \tab LaTeX Package(s) \cr
#'   \code{font_color} \tab \code{\\usepackage[dvipsnames]\{xcolor\}} \cr
#'   \code{bg, bg_pattern} \tab \code{\\usepackage[dvipsnames,table]\{xcolor\}} \cr
#'   \code{border_style} \tab \code{\\usepackage\{arydshln\}} \cr
#'       \tab  \code{\\usepackage\{amssymb\}} \cr
#'       (with vertical dashed lines) \tab \\usepackage\{graphicx\} \cr
#'       \tab \code{\\makeatletter} \cr
#'       \tab \code{\\newcommand*\\vdashline\{\\rotatebox[origin=c]\{90\}\{\$\\dabar@@\\dabar@@\\dabar@@\$\}\}} \cr
#'       \tab \code{\\makeatother} \cr
#'   \code{longtable} \tab \code{\\usepackage\{longtable\}} \cr
#'       \tab (Must be loaded before \code{arydshln}) \cr
#'   \code{merge} \tab \code{\\usepackage\{multirow\}}
#' }
#' 
#' In order to ensure all features are available, the recommended code block (accounting for 
#' the proper order to load packages) is:
#' 
#' \code{header-includes:} \cr
#' \code{ - \\usepackage[dvipsnames,table]\{xcolor\}} \cr
#' \code{ - \\usepackage\{longtable\}} \cr
#' \code{ - \\usepackage\{arydshln\}} \cr
#' \code{ - \\usepackage\{amssymb\}} \cr
#' \code{ - \\usepackage\{graphicx\}} \cr
#' \code{ - \\usepackage\{multirow\}} \cr
#' \code{ - \\makeatletter} \cr
#' \code{ - \\newcommand*\\vdashline\{\\rotatebox[origin=c]\{90\}\{\$\\dabar@@\\dabar@@\\dabar@@\$\}\}} \cr
#' \code{ - \\makeatother}
#'
#' @seealso \code{\link{sprinkle_colnames}} for changing column names in a table.
#' 
#' @source 
#' Altering the number of rows in a LaTeX longtable \cr
#' http://tex.stackexchange.com/questions/19710/how-can-i-set-the-maximum-number-of-rows-in-a-page-for-longtable
#' 
#' Vertical dashed cell borders in LaTeX table \cr
#' http://www.latex-community.org/forum/viewtopic.php?f=45&t=3149
#' 
#' Colored Cell border \cr
#' http://tex.stackexchange.com/questions/40666/how-to-change-line-color-in-tabular
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' x %>% sprinkle(cols = 2:4, round = 3) %>% 
#'   sprinkle(cols = 5, fn = quote(pvalString(value))) %>% 
#'   sprinkle(rows = 2, bold = TRUE)
#'

sprinkle <- function(x, rows=NULL, cols=NULL, ..., 
                          part = c("body", "head", "foot", "interfoot", "table"))
{
  Check <- ArgumentCheck::newArgCheck()
  
  #* Buckle up.  There's a lot of argument checking happening here.
  
  #* x must have class 'dust'
  if (class(x) != "dust")
    ArgumentCheck::addError(
      msg = "Sprinkles may only be added to objects of class 'dust'",
      argcheck = Check)
  
  #* Make sure the given 'part_name' can be found in the dust object
  part_name <- ArgumentCheck::match_arg(part, 
                                   c("body", "head", "foot", "interfoot", "table"),
                                   argcheck = Check)
  
  #* The ArgumentCheck version of match_arg returns a character(0) if no match
  #* is found.  This allows the error to be delayed until all checks are done.
  #* However, we need to generate a couple of objects before all the checks are
  #* done in order to get the 'replace' sprinkle to work correctly.  This
  #* block creates these objects only when a suitable match for the 'part' 
  #* argument has been provided.
  if (length(part_name) > 0)
  {
    #* The table part
    part <- x[[part_name]]
    
    #* The cols argument allows character and numeric values to be 
    #* given simultaneously. This block matches the character values
    #* to numeric column indices
    if (!is.null(cols)){
      cols_num <- suppressWarnings(as.numeric(cols))
      cols_num <- cols_num[!is.na(cols_num)]
    
      cols_str <- match(cols, unique(x$head$col_name))
      cols <- unique(c(cols_num, cols_str))
      cols <- cols[!is.na(cols)]
    }

    #* If rows or cols isn't given, assume the sprinkle should be applied
    #* across the entire dimension.
    if (is.null(rows) | length(rows) == 0) rows <- 1:max(part$row)
    if (is.null(cols) | length(cols) == 0) cols <- 1:max(part$col)
  
  }
  
  #* Start checking the sprinkles.  The messages in the argument checks should
  #* describe what the checks are looking for.
  
  sprinkles <- list(...)
  
  if (length(sprinkles) == 0)
    ArgumentCheck::addError(
      msg = "No sprinkles declared. You must define at least one sprinkle in '...'",
      argcheck = Check)
  
  if (any(names(sprinkles) %in% "") | is.null(names(sprinkles))){
    unnamed_pos <- which(names(sprinkles) %in% "")
    if (length(unnamed_pos) == 0) unnamed_pos = 1:length(sprinkles)
    ArgumentCheck::addError(
      msg = paste0("All arguments in '...' must be named. ",
                   "Check arguments in position(s) ", 
                   paste0(unnamed_pos, collapse=", ")),
      argcheck = Check)
  }
  
  #* No actual argument checking is performed on the 'fn' sprinkle.
  #* It is converted to a character string so that it can pass the 
  #* length 1 check.
  if ("fn" %in% names(sprinkles))
    sprinkles$fn <- deparse(sprinkles$fn)
  
  #* Identify sprinkles with length > 1
  too_long <- names(sprinkles)[vapply(sprinkles, 
                                      function(x) length(x) != 1,
                                      TRUE)]
  
  #* Exempt a few sprinkles that accept vectors with length > 1
  too_long <- too_long[!too_long %in% c("bg_pattern", "border", "replace")]
  
  if (length(too_long) > 0)
    ArgumentCheck::addError(
      msg = paste0("Arguments in '...' must have length 1.",
                   " Please check ", paste0(too_long, collapse=", "), "."),
      argcheck = Check)
  
  #* Reject any sprinkles not in the allowable list.
  #* The allowable list is accessed as a utility function
  #* 'sprinkle_names()' at the end of this script.
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
  
  if ("font_family" %in% names(sprinkles) & !is.character(sprinkles$font_family))
    ArgumentCheck::addError(
      msg = "The 'font_family' argument must be a character string",
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
                                                       c("px", "pt", "in", "cm", "%"),
                                                       argcheck = Check)
  
  if ("italic" %in% names(sprinkles) & !is.logical(sprinkles$italic))
    ArgumentCheck::addError(
      msg = "The 'italic' argument must be logical",
      argcheck = Check)
  
  if ("longtable" %in% names(sprinkles)){
    if (!is.logical(sprinkles$longtable)){
      if (is.numeric(sprinkles$longtable) & sprinkles$longtable < 1) sprinkles$longtable <- FALSE
      else if (!is.numeric(sprinkles$longtable)) sprinkles$longtable <- FALSE
    }
    
    x$longtable <- sprinkles$longtable
    sprinkles$longtable <- NULL
  }
  
  if ("merge" %in% names(sprinkles)){
    if (!is.logical(sprinkles$merge)){
      ArgumentCheck::addError(
        msg = "The 'merge' sprinkle must be logical with length 1",
        argcheck = Check)
    }
    if (is.null(sprinkles$merge_rowval))
      sprinkles$merge_rowval <- if (is.null(rows)) 1 else min(rows)
    if (is.null(sprinkles$merge_colval))
      sprinkles$merge_colval <- if (is.null(cols)) 1 else min(cols)
  }
  
  if (("merge_rowval" %in% names(sprinkles) | "merge_colval" %in% names(sprinkles)) &
      !"merge" %in% names(sprinkles)){
    ArgumentCheck::addError(
      msg = paste0("The sprinkles 'merge_rowval' and 'merge_colval' ",
                   "can only be used when the 'merge' sprinkle is used"),
      argcheck = Check)
  }
  
  if ("na_string" %in% names(sprinkles)){
    if (!is.character(sprinkles$na_string)){
      ArgumentCheck::addError(
        msg = "The 'na_string' sprinkle must be character with length 1",
        argcheck = Check)
    }
  }
  
  if ("pad" %in% names(sprinkles) & !is.numeric(sprinkles$pad))
    ArgumentCheck::addError(
      msg = "The 'pad' argument must be numeric",
      argcheck = Check)
  
  #* The replacement sprinkle takes some special care.
  #* First, if an invalid 'part_name' was given, a warning is 
  #* cast that the 'replace' argument isn't being checked.  
  #* The actual error check compares the number of cells to be replaced
  #* (as identified by rows and cols) with the length of the vector
  #* in 'replace'.  We can't determine if these lengths match without knowing
  #* the part of the table to work in.
  if (length(part_name) == 0 & "replace" %in% names(sprinkles))
    ArgumentCheck::addMessage(
      msg = paste0("'replace' argument could not be checked because the ",
                   "value to 'part' was invalid."),
      argcheck = Check)
  
  #* Generate the replacement table
  if ("replace" %in% names(sprinkles)){
    ReplaceTable <- expand.grid(row = rows, col = cols)
           
    if (length(sprinkles$replace) != nrow(ReplaceTable))
      ArgumentCheck::addError(
        msg = paste0("The 'replace' argument should have length ", nrow(ReplaceTable), 
                     " (based on the cross section of 'rows' and 'cols')"),
        argcheck = Check)
    else {
      #* There is no column for the 'replace' sprinkle.  Instead, the replacements
      #* are placed directly into the table.  
      ReplaceTable$value = sprinkles$replace
      sprinkles$replace <- NULL
    }
  }
  
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
  
  if ("tabcolsep" %in% names(sprinkles)){
    if (!is.numeric(sprinkles$tabcolsep)){
      ArgumentCheck::addError(
        msg = "The 'tabcolsep' argument must be numeric",
        argcheck = Check)
    }
    
    x$tabcolsep <- sprinkles$tabcolsep
    sprinkles$tabcolsep <- NULL
  }
  
  if ("width" %in% names(sprinkles) & !is.numeric(sprinkles$width))
    ArgumentCheck::addError(
      msg = "The 'width' argument must be numeric",
      argcheck = Check)
  
  if ("width_units" %in% names(sprinkles))
    sprinkles$width_units <- ArgumentCheck::match_arg(sprinkles[["width_units"]],
                                                      c("px", "pt", "cm", "in", "%"),
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
  
  #* The next several lines set default sprinkle values.
  #* Some sprinkles come in groups, like font_size and font_size units.
  #* If one member of the pair is specified, but the second is not, we assume
  #* the user wants the default specified.
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

  #* Generate the border_collapse sprinkle
  if (!is.null(sprinkles$border_collapse))
  {
    x$border_collapse <- sprinkles$border_collapse
    sprinkles$border_collapse <- NULL
  }
  
  #* Generate the background pattern sprinkle
  if (!is.null(sprinkles[["bg_pattern"]])){
    x$bg_pattern <- sprinkles$bg_pattern
    x$bg_pattern_by <- sprinkles$bg_pattern_by
    
    bg_pattern <- sprinkles$bg_pattern
    pattern_by <- sprinkles$bg_pattern_by
    
    sprinkles$bg_pattern <- NULL
    sprinkles$bg_pattern_by <- NULL
  }

  #* Generate a table of all of the other sprinkles to be implemented.
  #* This occurs after the border and bg_pattern sprinkles are constructed so 
  #* that those sprinkles can be set to NULL.  Those are multiple column
  #* sprinkles that aren't going to play nicely in expand.grid.
  Cells <- expand.grid(c(list(row = rows,
                              col = cols),
                         sprinkles),
                       stringsAsFactors = FALSE)
  
  #* merge in border specifications, if any
  if ("border" %in% names(Cells))
    Cells <- dplyr::mutate_(Cells,
                            border = ~paste0(border, "_border"),
                            border_spec = ~paste0(border_thickness, border_units, " ",
                                                  border_style, " ", border_color)) %>%
    dplyr::select_("-border_thickness", "-border_units", "-border_style", "-border_color") %>%
    tidyr::spread_("border", "border_spec")

  #* Merge in background patterns, if any
  if (exists("bg_pattern")){
    #* pattern by row
    if (pattern_by == "rows"){
      bg_frame <- dplyr::data_frame(row = unique(Cells$row))
      bg_frame[["bg"]] <- rep(bg_pattern, length.out = nrow(bg_frame))
      Cells <- dplyr::left_join(Cells, bg_frame, by = c("row" = "row"))
      if ("bg.x" %in% names(Cells)){
        Cells <- dplyr::rename_(Cells, "bg" = "bg.y") %>%
          dplyr::select_("-bg.x")
      }
    }
    else {
      #* pattern by column
      bg_frame <- dplyr::data_frame(col = unique(Cells$col))
      bg_frame[["bg"]] <- rep(bg_pattern, length.out = nrow(bg_frame))
      Cells <- dplyr::left_join(Cells, bg_frame, by = c("col" = "col"))
      if ("bg.x" %in% names(Cells)){
        Cells <- dplyr::rename_(Cells, "bg" = "bg.y") %>%
          dplyr::select_("-bg.x")
      }
    }
  }

  #* merge in replacement sprinkle
  if (exists("ReplaceTable")){
    Cells <- dplyr::left_join(Cells, ReplaceTable,
                              by = c("row" = "row", "col" = "col"))
  }
  
  #* Set rowspan and colspan values
  if ("merge" %in% names(Cells)){
    Cells <- dplyr::mutate_(Cells,
                     rowspan = ~ifelse(row == merge_rowval, 
                                       length(rows),
                                       0),
                     colspan = ~ifelse(col == merge_colval,
                                       length(cols),
                                       0),
                     html_row = min(rows),
                     html_col = min(cols),
                     merge_rowval = ~NULL,
                     merge_colval = ~NULL)
  }
 
  #* determine the cell indices to replace
  replace <- vapply(1:nrow(Cells), 
                    function(r) which(part$row == Cells$row[r] & part$col == Cells$col[r]), 
                    1)

  #* Implement the sprinkles, assign the new part back into the dust object
  #* and return.
  part[replace, names(Cells)[-(1:2)]] <- Cells[, -(1:2), drop=FALSE]

  x[[part_name]] <- part
  x
}

#****************
#* List of acceptable sprinkle names.
#* Whenever I add a sprinkle, I'll likely forget to add 
#* it to this list.  And it will likely take me way 
#* too long to figure that out.

sprinkle_names <- function()
{
  c("bg", "bg_pattern", "bg_pattern_by", 
    "bold", "border", "border_thickness", 
    "border_units", "border_style", "border_color", 
    "border_collapse",
    "fn", "font_family", "font_color", "font_size", "font_size_units", "halign", 
    "height", "height_units", "italic", "longtable", 
    "merge", "merge_rowval", "merge_colval", "na_string", "pad", 
    "replace", "rotate_degree", 
    "round", "tabcolsep", "valign", "width", "width_units")
}

#* Default sprinkle values.  Used mostly for sprinkles that come in
#* pairs, but only one of the pair is specified.
default_sprinkles <- function(setting)
{
  switch(setting,
         "bg_pattern" = c("White", "Gray"),
         "bg_pattern_by" = "rows",
         "border" = "all",
         "border_thickness" = 1,
         "border_units" = "px",
         "border_style" = "solid",
         "border_color" = "Black",
         "font_size_units" = "pt",
         "height_units" = "pt",
         "width_units" = "pt")
}
