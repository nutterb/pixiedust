#' @name sprinkle
#' @export sprinkle
#' 
#' @title Define Customizations to a Table
#' @description Customizations to a \code{dust} table are added by "sprinkling"
#'   with a little extra pixie dust.  Sprinkles are a collection of attributes
#'   to be applied over a subset of table cells.  They may be added to any 
#'   part of the table, or to the table as a whole.
# Parameters --------------------------------------------------------  
#' @param x A dust object
#' @param rows A numeric vector specifying the rows of the table to sprinkle.
#'   See details for more about sprinkling.
#' @param cols A numeric (or character) vector specifying the columns (or 
#'   column names) to sprinkle.  See details for more about sprinkling.
#' @param part A character string denoting which part of the table to modify.
#' @param fixed \code{logical(1)} indicating if the values in \code{rows} 
#'   and \code{cols} should be read as fixed coordinate pairs.  By default, 
#'   sprinkles are applied at the intersection of \code{rows} and \code{cols}, 
#'   meaning that the arguments do not have to share the same length.  
#'   When \code{fixed = TRUE}, they must share the same length.
#' @param recycle A \code{character} one that determines how sprinkles are 
#'   managed when the sprinkle input doesn't match the length of the region
#'   to be sprinkled.  By default, recycling is turned off.  Recycling 
#'   may be performed across rows first (left to right, top to bottom), 
#'   or down columns first (top to bottom, left to right).
#' @param ... named arguments, each of length 1, defining the customizations
#'   for the given cells.  See "Sprinkles" for a listing of these arguments.
#'   
# Details -----------------------------------------------------------
#' @details Sprinkling is done over the intersection of rows and columns 
#'   (unless \code{fixed = TRUE}.  If
#'   rows but no columns are specified, sprinkling is performed over all columns
#'   of the given given rows. The reverse is true for when columns but no rows
#'   are specified.  If neither columns nor rows are specified, the attribute 
#'   is applied over all of the cells in the table part denoted in \code{part}.
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
#'   Using \code{sprinkle_table}, sprinkles may be applied to the columns of multiple tables. Table
#'   parts are required to have the same number of columns, but not necessarily the same number 
#'   of rows, which is why the \code{rows} argument is not available for the \code{sprinkle_table}.
#'   In contrast to \code{sprinkle}, the \code{part} argument in \code{sprinkle_table} will 
#'   accept multiple parts.  If any of the named parts is \code{"table"}, the sprinkle will be 
#'   applied to the columns of all of the parts.
#'   
# Sprinkles ---------------------------------------------------------  
#' @section Sprinkles:
#' The following table describes the valid sprinkles that may be defined in the 
#' \code{...} dots argument.  All sprinkles may be defined for any output type, but 
#' only sprinkles recognized by that output type will be applied when printed.  
#' A more readable format of this information is available in  
#' \code{vignette("sprinkles", package = "pixiedust")}.
#' 
#' \tabular{lll}{
#' bg  \tab           \tab  \cr
#'     \tab action    \tab Modifies the background color of a cell. \cr
#'     \tab default   \tab  \cr
#'     \tab accepts   \tab dvips color names; rgb(R,G,B); rgba(R,G,B,A); \cr
#'     \tab           \tab  #RRGGBB; #RRGGBBAA. See the "Colors" section \cr
#'     \tab           \tab  for further details or \cr 
#'  \tab              \tab  \url{http://nutterb.github.io/pixiedust/colors.html}. \cr
#'     \tab console   \tab Not recognized \cr
#'     \tab markdown  \tab Not recognized \cr
#'     \tab html      \tab Accepts any of the listed formats; \cr
#'     \tab           \tab recognizes transparency \cr
#'     \tab latex     \tab Accepts any of the listed formats, \cr
#'     \tab           \tab but ignores transparency \cr
#' bg_pattern \tab    \tab  \cr
#'  \tab action       \tab Generates a pattern of background colors.  \cr
#'  \tab              \tab Can be used to make striping \cr
#'  \tab              \tab by rows or by columns. \cr
#'  \tab default      \tab c("#FFFFFF", "#DDDDDD") \cr
#'  \tab accepts      \tab A vector of color names: \cr
#'  \tab              \tab dvips color names; rgb(R,G,B); rgba(R,G,B,A); \cr 
#'  \tab              \tab #RRGGBB; #RRGGBBAA \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Accepts any of the listed formats; \cr
#'  \tab              \tab recognizes transparency \cr
#'  \tab latex        \tab Accepts any of the listed formats, \cr
#'  \tab              \tab but ignores transparency \cr
#' bg_pattern_by  \tab  \tab  \cr
#'  \tab action       \tab Determines if a `bg_pattern` is patterned \cr 
#'  \tab              \tab by row or by columns. \cr
#'  \tab default      \tab "rows" \cr
#'  \tab accepts      \tab "rows", "columns", "cols" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' bold \tab  \tab  \cr
#'  \tab action       \tab Renders text within a cell in bold. \cr
#'  \tab default      \tab FALSE \cr
#'  \tab accepts      \tab logical(1) \cr
#'  \tab console      \tab Recognized; rendered as double asterisks on either\cr
#'  \tab              \tab side of the text \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' border_collapse \tab  \tab  \cr
#'  \tab action       \tab Sets the `border-collapse` property in an \cr
#'  \tab              \tab HTML table.  The property sets whether the \cr
#'  \tab              \tab table borders are collapsed into a  \cr
#'  \tab              \tab single border or detached as in standard HTML. \cr
#'  \tab default      \tab TRUE \cr
#'  \tab accepts      \tab logical(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Not recognized \cr
#' border \tab  \tab  \cr
#'  \tab action       \tab Sets a border on the specified side of a cell. \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab Any combination of "all", "bottom", "left", "top",\cr
#'  \tab              \tab "right".  Using  "all" results in all borders \cr
#'  \tab              \tab being drawn, regardless of what other values are \cr
#'  \tab              \tab passed with it. \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' border_color \tab  \tab  \cr
#'  \tab action       \tab Sets the color of the borders specified for a cell. \cr
#'  \tab default      \tab "Black" \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab              \tab dvips color names; rgb(R,G,B); rgba(R,G,B,A); \cr
#'  \tab              \tab #RRGGBB; #RRGGBBAA. See the "Colors" section \cr
#'     \tab           \tab  for further details or \cr 
#'  \tab              \tab  \url{http://nutterb.github.io/pixiedust/colors.html}. \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' border_style \tab  \tab  \cr
#'  \tab action       \tab Sets the border style for a specified cell \cr
#'  \tab default      \tab "solid" \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab              \tab "solid", "dashed", "dotted", "double", "groove", \cr
#'  \tab              \tab "ridge", "inset", "outset", "hidden", "none" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Accepts any of the values listed. \cr
#'  \tab latex; hhline = FALSE \tab accepts "solid", "dashed", "dotted",  \cr
#'  \tab              \tab  "hidden", "none" \cr
#'  \tab              \tab "dotted" is silently changed to "dashed" \cr
#'  \tab              \tab "hidden" and "none" are equivalent. \cr
#'  \tab latex; hhline = TRUE \tab accepts "solid", "double", "hidden", "none" \cr
#'  \tab              \tab "hidden" and "none" are equivalent. \cr
#' border_thickness \tab  \tab  \cr
#'  \tab action       \tab Sets the thickness of the specified border \cr
#'  \tab default      \tab 1 \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' border_units \tab  \tab  \cr
#'  \tab action       \tab Sets the unit of measure for the specified border \cr
#'  \tab              \tab thickness \cr
#'  \tab default      \tab "pt" \cr
#'  \tab accepts      \tab "pt", "px" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Silently changes "px" to "pt" \cr
#' caption \tab  \tab  \cr
#'  \tab action       \tab Adds or alters the `caption` property \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' discrete \tab \tab \cr
#'  \tab action       \tab Adds distinct background colors based on \cr
#'  \tab              \tab discrete values in the selected region. \cr
#'  \tab              \tab May not be used concurrently with \code{bg}. \cr
#'  \tab              \tab \code{"font"} is an alias for \code{"font_color"} \cr
#'  \tab              \tab and \code{"border"} is an alias for \cr
#'  \tab              \tab all borders. \cr
#'  \tab default      \tab "bg" \cr
#'  \tab accepts      \tab "bg", "font", "font_color", "border", \cr
#'  \tab              \tab "left_border", "top_border", "right_border", \cr
#'  \tab              \tab "bottom_border" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' discrete_color \tab \tab \cr
#'  \tab action       \tab Sets the color palette from which \code{discrete} \cr
#'  \tab              \tab selects background colors. If \code{NULL} \cr
#'  \tab              \tab colors are automatically selected using \cr
#'  \tab              \tab the \code{scales} package. \cr
#'  \tab default      \tab \code{getOption("pixie_discrete_pal", NULL)} \cr
#'  \tab accepts      \tab character \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' float \tab  \tab  \cr
#'  \tab action       \tab Sets the `float` property \cr
#'  \tab default      \tab TRUE \cr
#'  \tab accepts      \tab logical(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Not recognized \cr
#'  \tab latex        \tab Recognized \cr
#' fn \tab  \tab  \cr
#'  \tab action       \tab Applies a function to the value of a cell. \cr
#'  \tab              \tab The function should be an \cr
#'  \tab              \tab expression that acts on the variable `value`.  \cr 
#'  \tab              \tab For example, \code{quote(format(value, nsmall = 3))} \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab call \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' font_color \tab  \tab  \cr
#'  \tab action       \tab Sets the color of the cell text \cr
#'  \tab default      \tab Black \cr
#'  \tab accepts      \tab dvips color names; rgb(R,G,B); rgba(R,G,B,A); \cr
#'  \tab              \tab #RRGGBB; #RRGGBBAA. See the "Colors" section \cr
#'     \tab           \tab  for further details or \cr 
#'  \tab              \tab  \url{http://nutterb.github.io/pixiedust/colors.html}. \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized; transparency recognized \cr
#'  \tab latex        \tab Recognized; transparency ignored \cr
#' font_family \tab  \tab  \cr
#'  \tab action       \tab Sets the font for the text \cr
#'  \tab default      \tab Times New Roman \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab              \tab http://www.w3schools.com/cssref/css_websafe_fonts.asp \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Not recognized \cr
#' font_size \tab  \tab  \cr
#'  \tab action       \tab Sets the size of the font in the cell \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' font_size_units \tab  \tab  \cr
#'  \tab action       \tab Determines the units in which `font_size` \cr
#'  \tab              \tab is measured \cr
#'  \tab default      \tab "px" \cr
#'  \tab accepts      \tab "px", "pt", "\%", "em" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Only recognizes "pt" and "em".  \cr
#'  \tab              \tab All others are coerced to "pt" \cr
#' gradient \tab  \tab  \cr
#'  \tab action       \tab Adds distinct background colors based on \cr
#'  \tab              \tab progressively increasing values in the \cr
#'  \tab              \tab selected region. May not be used concurrently \cr 
#'  \tab              \tab with \code{bg}. \cr
#'  \tab              \tab \code{"font"} is an alias for \code{"font_color"} \cr
#'  \tab              \tab and \code{"border"} is an alias for \cr
#'  \tab              \tab all borders. \cr
#'  \tab default      \tab "bg" \cr
#'  \tab accepts      \tab "bg", "font", "font_color", "border", \cr
#'  \tab              \tab "left_border", "top_border", "right_border", \cr
#'  \tab              \tab "bottom_border" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' gradient_colors \tab \tab \cr
#'  \tab action       \tab Provides the colors between which to \cr
#'  \tab              \tab shade gradients. \cr
#'  \tab default      \tab \code{getOptions("pixie_gradient_pal", NULL)} \cr
#'  \tab accepts      \tab character \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' gradient_cut \tab  \tab  \cr
#'  \tab action       \tab Determines the breaks points for the \cr
#'  \tab              \tab gradient shading. When \code{NULL}  \cr
#'  \tab              \tab equally spaced quantiles are used, the \cr
#'  \tab              \tab number of which are determined by \cr
#'  \tab              \tab \code{gradient_n}. \cr
#'  \tab default      \tab NULL \cr
#'  \tab accepts      \tab numeric \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' gradient_n \tab  \tab  \cr
#'  \tab action       \tab Determines the number of shades to use \cr
#'  \tab              \tab between the colors in \code{gradient_colors}.\cr
#'  \tab default      \tab 10 \cr
#'  \tab accepts      \tab numeric \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' gradient_na \tab  \tab  \cr
#'  \tab action       \tab Sets the color of NA values when gradients \cr
#'  \tab              \tab are shaded. \cr
#'  \tab default      \tab grey \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' halign \tab  \tab  \cr
#'  \tab action       \tab Sets the horizontal alignment of the text in \cr
#'  \tab              \tab the cell \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab "left", "center", "right" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Recognized; numeric values will auto align to the \cr
#'  \tab              \tab right if no value given. \cr
#'  \tab html         \tab Recognized.  Does not currently employ auto \cr
#'  \tab              \tab alignment of numeric values, but this may change. \cr
#'  \tab latex        \tab Recognized; numeric values will auto align to \cr
#'  \tab              \tab the right if no value given. \cr
#' height \tab  \tab  \cr
#'  \tab action       \tab Sets the height of the cell \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' height_units \tab  \tab  \cr
#'  \tab action       \tab Determines the units in which `height` is measured \cr
#'  \tab default      \tab "pt" \cr
#'  \tab accepts      \tab "px", "pt", "cm", "in", "\%" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized; "px" is coerced to "pt" \cr
#' hhline \tab  \tab  \cr
#'  \tab action       \tab Toggles the option for cell border drawing with \cr 
#'  \tab              \tab the `hhline` LaTeX package \cr
#'  \tab default      \tab FALSE \cr
#'  \tab accepts      \tab logical(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Not recognized \cr
#'  \tab latex        \tab Recognized.  When `FALSE` double borders are \cr 
#'  \tab              \tab not available. \cr
#'  \tab              \tab When `TRUE`, colored and dashed borders are not \cr
#'  \tab              \tab available. This is usually the better option \cr
#'  \tab              \tab when using colored backgrounds in table cells. \cr
#' italic \tab  \tab  \cr
#'  \tab action       \tab Renders the text in the cell in italic \cr
#'  \tab default      \tab FALSE \cr
#'  \tab accepts      \tab logical(1) \cr
#'  \tab console      \tab Recognized; rendered as an underscore on either \cr
#'  \tab              \tab side of the text. \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' justify \tab  \tab  \cr
#'  \tab action       \tab Justifies the entire table on the page. \cr
#'  \tab default      \tab "center" \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognizes "center", but both "left" and "right" \cr
#'  \tab              \tab are rendered as left justified.  This may change \cr
#'  \tab              \tab if a satisfactory solution is found.  \cr
#'  \tab              \tab Usually, tables are best left centered. \cr
#' longtable \tab  \tab  \cr
#'  \tab action       \tab Toggles the use of the LaTeX `longtable` style \cr
#'  \tab              \tab tables, namely allowing long tables to be broken \cr
#'  \tab              \tab into multiple sections. The table header appears \cr 
#'  \tab              \tab at the top of each section. The table interfoot \cr
#'  \tab              \tab appears at the bottom of each section, except \cr
#'  \tab              \tab for the last. \cr
#'  \tab              \tab The table foot appears at the bottom of the \cr
#'  \tab              \tab last section. \cr
#'  \tab              \tab May accept either a logical or a numerical value.  \cr
#'  \tab              \tab If numerical, each section will have the specified \cr
#'  \tab              \tab number of rows. \cr
#'  \tab default      \tab FALSE \cr
#'  \tab accepts      \tab logical(1); numeric(1) \cr
#'  \tab console      \tab Recognized; when `TRUE`, defaults to 25 rows \cr
#'  \tab              \tab per section. \cr
#'  \tab markdown     \tab Recognized; when `TRUE`, defaults to 25 rows \cr
#'  \tab              \tab per section. \cr
#'  \tab html         \tab Recognized; when `TRUE`, defaults to 25 rows \cr
#'  \tab              \tab per section. \cr
#'  \tab latex        \tab Recognized; when `TRUE`, `longtable`'s own algorithm \cr 
#'  \tab              \tab will determine the number of rows per section. \cr
#'  \tab              \tab When numeric, breaks are forced at the specified \cr
#'  \tab              \tab number of rows. \cr
#' merge \tab  \tab  \cr
#'  \tab action       \tab Merges cells in the specified range into a \cr
#'  \tab              \tab single cell. In cases where \cr
#'  \tab              \tab either `merge_rowval` or `merge_colval` is \cr
#'  \tab              \tab specified, they will only be  \cr
#'  \tab              \tab honored if `merge = TRUE`.  You must opt in to \cr
#'  \tab              \tab this action. \cr
#'  \tab default      \tab FALSE \cr
#'  \tab accepts      \tab logical(1) \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' merge_rowval \tab  \tab  \cr
#'  \tab action       \tab Specifies the row value of the merged range to \cr
#'  \tab              \tab print in the table \cr
#'  \tab default      \tab minimum row value of the merged range \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' merge_colval \tab  \tab  \cr
#'  \tab action       \tab Specifies the column value of the merged range \cr
#'  \tab              \tab to print in the table \cr
#'  \tab default      \tab minimum col value of the merged range \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' na_string \tab  \tab  \cr
#'  \tab action       \tab Designates the character string to use in place \cr
#'  \tab              \tab of missing values \cr
#'  \tab default      \tab NA \cr
#'  \tab accepts      \tab character(1) \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' pad \tab  \tab  \cr
#'  \tab action       \tab Designates the padding to place between cell \cr
#'  \tab              \tab text and boundaries \cr
#'  \tab              \tab Measured in pixels. \cr
#'  \tab default      \tab 0 \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Not recognized \cr
#' replace \tab  \tab  \cr
#'  \tab action       \tab Replaces existing cell values with user-specified \cr
#'  \tab              \tab content. Replacement occurs moving down columns \cr
#'  \tab              \tab from left to right. \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab character vector of the same length as the number \cr
#'  \tab              \tab of cells being replaced. \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' rotate_degree \tab  \tab  \cr
#'  \tab action       \tab Rotates text in cells by the designated angle \cr
#'  \tab              \tab in degrees \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' round \tab  \tab  \cr
#'  \tab action       \tab Applies the `round` function to values in the \cr 
#'  \tab              \tab cell.  Skips any character values it encounters. \cr
#'  \tab default      \tab \code{getOption("digits")} \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Recognized \cr
#'  \tab markdown     \tab Recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' sanitize \tab  \tab  \cr
#'  \tab action	      \tab Sanitizes character values that may cause \cr
#'  \tab              \tab difficulties for the rendered format.  \cr
#'  \tab default	    \tab FALSE      \cr
#'  \tab accepts	    \tab logical(1) \cr
#'  \tab console	    \tab Not recognized \cr
#'  \tab markdown	    \tab Not recognized \cr
#'  \tab html         \tab Not recognized \cr
#'  \tab latex	      \tab Recognized.  Sanitization is performed using \cr
#'  \tab              \tab \code{\link[Hmisc]{latexTranslate}} \cr
#' sanitize_args	\tab \tab \cr
#'  \tab action	      \tab Passes additional arguments to \code{\link[Hmisc]{latexTranslate}} \cr
#'  \tab default	    \tab \code{list()} \cr
#'  \tab accepts	    \tab list.  See documentation for \code{\link[Hmisc]{latexTranslate}} \cr
#'  \tab              \tab for details \cr
#'  \tab console	    \tab Not recognized \cr
#'  \tab markdown	    \tab Not recognized \cr
#'  \tab html	        \tab Not recognized \cr
#'  \tab latex	      \tab Recognized \cr
#' tabcolsep \tab  \tab  \cr
#'  \tab action       \tab Modifies the LaTeX `tabcolsep` parameter of tables \cr
#'  \tab              \tab This is similar to `pad` for HTML tables, but only  \cr
#'  \tab              \tab affects the space between columns. Measured in "pt" \cr
#'  \tab default      \tab 6 \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Not recognized \cr
#'  \tab latex        \tab Recognized \cr
#' valign \tab  \tab  \cr
#'  \tab action       \tab Designates the vertical alignment of a cell. \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab "top", "middle", "bottom" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' width \tab  \tab  \cr
#'  \tab action       \tab Sets the width of the cell \cr
#'  \tab default      \tab  \cr
#'  \tab accepts      \tab numeric(1) \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized \cr
#' width_units \tab  \tab  \cr
#'  \tab action       \tab Determines the units in which `width` is measured \cr
#'  \tab default      \tab "pt" \cr
#'  \tab accepts      \tab "px", "pt", "cm", "in", "\%" \cr
#'  \tab console      \tab Not recognized \cr
#'  \tab markdown     \tab Not recognized \cr
#'  \tab html         \tab Recognized \cr
#'  \tab latex        \tab Recognized; "px" is coerced to "pt" \cr
#' }
#' 
# Longtable details -------------------------------------------------
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
# Colors ------------------------------------------------------------
#' @section Colors:
#' Colors may be declared as any of the color names in \code{colors()}, 
#' as rgb character strings such as \code{"rgb(rrr,ggg,bbb)"} or as 
#' hexidecimal character strings such as \code{"#rrggbb"}.  
#' 
#' Transparency is also recognized by HTML output, and may be indicated 
#' in the rgba format \code{"rgba(rrr,ggg,bbb,aa)"}, where \code{aa} is a 
#' number between 0 and 1, inclusive.  Alternative, transparency may be 
#' given as \code{"#rrggbbAA"}, where \code{AA} is a hexidecimal 
#' representation of transparency with "00" being completely transparent 
#' and "FF" being completely opaque.
#' 
#' LaTeX output does not recognize transparency and will quietly drop the 
#' transparency parameter.
#'
#' All colors are internally translated into rgb format and are case insensitive.
#' 
# LaTeX Packages ----------------------------------------------------
#' @section Required LaTeX Packages:
#' (Read more about \code{pixiedust} with LaTeX at 
#' http://nutterb.github.io/pixiedust/latex-configuration.html)
#' 
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
#'       \tab  \code{\\usepackage\{hhline\}} \cr
#'       (with vertical dashed lines) \tab \\usepackage\{graphicx\} \cr
#'       \tab \code{\\makeatletter} \cr
#'       \tab \code{\\newcommand*\\vdashline\{\\rotatebox[origin=c]\{90\}\{\$\\dabar@@\\dabar@@\\dabar@@\$\}\}} \cr
#'       \tab \code{\\makeatother} \cr
#'   \code{longtable} \tab \code{\\usepackage\{longtable\}} \cr
#'       \tab (Must be loaded before \code{arydshln}) \cr
#'   \code{merge} \tab \code{\\usepackage\{multirow\}} \cr
#'   \code{captions} for non floats \tab \code{\\usepackage\{caption\}} 
#' }
#' 
#' Note that \code{hhline} is used to make horizontal lines when 
#' \code{options(pixiedust_latex_hhline = TRUE)} (the package default is \code{FALSE}), 
#' otherwise the \code{cline} command is used.  
#' 
#' Use of \code{cline} permits colored borders and dashed borders, but 
#' borders around cells with background colors are sometimes (often) lost.
#' 
#' Use of \code{hhline} preserves borders around cells with background colors 
#' and permits double borders, but colored and dashed borders are not available.
#' 
#' In order to ensure all features are available, the recommended code block (accounting for 
#' the proper order to load packages) is:
#' 
#' \code{header-includes:} \cr
#' \code{ - \\usepackage\{amssymb\}} \cr
#' \code{ - \\usepackage\{arydshln\}} \cr
#' \code{ - \\usepackage\{caption\}}  \cr
#' \code{ - \\usepackage\{graphicx\}} \cr
#' \code{ - \\usepackage\{hhline\}} \cr
#' \code{ - \\usepackage\{longtable\}} \cr
#' \code{ - \\usepackage\{multirow\}} \cr
#' \code{ - \\usepackage[dvipsnames,table]\{xcolor\}} \cr
#' \code{ - \\makeatletter} \cr
#' \code{ - \\newcommand*\\vdashline\{\\rotatebox[origin=c]\{90\}\{\$\\dabar@@\\dabar@@\\dabar@@\$\}\}} \cr
#' \code{ - \\makeatother}
#' 
# Remaining Documentation -------------------------------------------
#' @seealso 
#' \code{\link{sprinkle_colnames}} for changing column names in a table.
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

#' @rdname sprinkle
#' @export

# sprinkle method ---------------------------------------------------
sprinkle <- function(x, rows = NULL, cols = NULL, ...,
                     part = c("body", "head", "foot", "interfoot", "table"))
{
  UseMethod("sprinkle")
}

# sprinkle.default --------------------------------------------------
#' @rdname sprinkle
#' @export

sprinkle.default <- function(x, rows = NULL, cols = NULL, ...,
                             part = c("body", "head", "foot", "interfoot", "table"),
                             fixed = FALSE, 
                             recycle = c("none", "rows", "cols", "columns"))
{
  
# Argument validations ----------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertClass(x = x,
                         classes = "dust",
                         add = coll)
  
  checkmate::assertSubset(x = part,
                          choices = c("body", "head", "foot", "interfoot", "table"),
                          add = coll)

  recycle <- assert_match_arg(x = recycle, 
                              choices = c("none", "rows", "cols", "columns"),
                              add = coll,
                              .var.name = "recycle")

  if (recycle == "columns") recycle <- "cols"

  recycle_arrange <- 
    if (recycle == "rows") 
      c("row", "col")
    else
      c("col", "row")
  
  part <- part[1]
  
  x[[part]] <- dplyr::arrange_(x[[part]], recycle_arrange)
  
  if (length(part))
  {
    #* The cols argument allows character and numeric values to be 
    #* given simultaneously. This block matches the character values
    #* to numeric column indices
    if (!is.null(cols))
    {
      cols_num <- suppressWarnings(as.numeric(cols))
      cols_num <- cols_num[!is.na(cols_num)]
      
      cols_str <- match(cols, 
                        unique(x[["head"]][["col_name"]]))
      
      #* We don't want to restrict ourselves to just the unique 
      #* columns if we are doing fixed coordinate pairs
      if (!fixed) cols <- unique(c(cols_num, cols_str))
      
      cols <- cols[!is.na(cols)]
    }
    
    #* If rows or cols isn't given, assume the sprinkle should be applied
    #* across the entire dimension.
    if (is.null(rows))
    {
      rows <- 1:max(x[[part]][["row"]])
    }
    
    if (is.null(cols) | length(cols) == 0)
    {
      cols <- 1:max(x[[part]][["col"]])
    }
  }  # End if (length(part))

  #* Determine the indices of the table part to be changed.
  if (fixed)
  {
    indices <- 
      data.frame(rows = rows,
                 cols = cols) %>%
      dplyr::mutate(i = TRUE) %>%
      dplyr::left_join(x[[part]],
                       .,
                       by = c("row" = "rows", 
                              "col" = "cols")) %>%
      dplyr::arrange_(recycle_arrange) %>%
      `[[`("i")
    indices[is.na(indices)] <- FALSE
  }
  else
  {
    indices <- x[[part]][["row"]] %in% rows & 
                x[[part]][["col"]] %in% cols
  }
  
  checkmate::assertNumeric(x = rows,
                           add = coll)
  
  sprinkles <- list(...)

  if (!length(sprinkles))
  {
    coll$push("No sprinkles in `...` to `sprinkle`")
  }
  else
  {
    sprinkle_match <- 
      SprinkleRef$sprinkle[pmatch(names(sprinkles), 
                                  SprinkleRef$sprinkle)]
    
    unmatched_sprinkle <- 
      names(sprinkles)[which(is.na(sprinkle_match))]
    
    names(sprinkles) <- sprinkle_match
    
    if (length(unmatched_sprinkle))
    {
      coll$push(
        sprintf("The following arguments could not be matched to an existing sprinkle (check spelling and partial matching): %s",
                paste(unmatched_sprinkle, collapse = ", "))
      )
      
      sprinkles <- sprinkles[!is.na(names(sprinkles))]
    }
  }
  
  #* Some love for longtable.  Characters given to longtable
  #* are assumed to be FALSE
  if ("longtable" %in% names(sprinkles))
  {
    if (!is.logical(sprinkles[["longtable"]]))
    {
      if (is.numeric(sprinkles[["longtable"]]) & sprinkles[["longtable"]] < 1)
      {
        sprinkles[["longtable"]] <- FALSE
      }
      else if (!is.numeric(sprinkles[["longtable"]]))
      {
        sprinkles[["longtable"]] <- FALSE
      }
    }
  }
  #* Use the unexported function `assert_sprinkles` to test
  #* the assertions defined in the `SprinkleRef` data frame.
  #* SprinkleRef is defined in the `inst/sprinkle_ref.csv' file
  #* and is used internally to expedite tests on sprinkles.

  assert_sprinkles(sprinkles = sprinkles,
                   coll = coll,
                   recycle = recycle)

  #* Additional assertions
  
  #* Cast an error if merge_rowval or merge_colval is given but not merge
  if (("merge_rowval" %in% names(sprinkles) | 
       "merge_colval" %in% names(sprinkles)) & 
      !"merge" %in% names(sprinkles))
  {
    coll$push("`merge` must be specified when `merge_rowval` or `merge_colval` is given")
  }

  #* Cast an error if `replace` is not the same length as `indices`
  if ("replace" %in% names(sprinkles))
  {
    if (length(sprinkles[["replace"]]) != sum(indices))
    {
      coll$push(paste0("The `replace` sprinkle should have length ",
                       sum(indices)))
    }
  }
  
  #* Convert colors to rgb
  given_color_sprinkles <- 
      !vapply(sprinkles[c("bg", "border_color", "font_color")], 
              is.null, 
              logical(1))
  given_color_sprinkles <- 
    c("bg", "border_color", "font_color")[given_color_sprinkles]

    
  if (length(given_color_sprinkles))
  {
    sprinkles[given_color_sprinkles] <- 
      color_sprinkles(sprinkles = sprinkles[given_color_sprinkles],
                      coll = coll)
  }
  
  # Prevent `discrete` and `bg` from being used together
  if (all(c("discrete", "bg") %in% names(sprinkles)))
  {
    if ("bg" %in% sprinkles[["discrete"]])
      coll$push("`discrete = 'bg'` and the `bg` sprinkle may not be used in the same call")
  }

  if (all(c("discrete", "border") %in% names(sprinkles)))
  {
    if ("border" %in% sprinkles[["discrete"]])
      coll$push("`discrete = 'border' and the `border` sprinkle may not be used in the same call")
  }

  #* Return any errors found.
  checkmate::reportAssertions(coll)

# Functional Code ---------------------------------------------------
  #* Sprinkles in the `option` group.
  #* These are sprinkles that affect options found in `dust`,
  #* such as longtable, caption, and label.

  x <- option_sprinkles(x = x, 
                        sprinkles = sprinkles)

  #* Special care for the `sanitize_args` sprinkle
  if ("sanitize_args" %in% names(sprinkles))
  {
    sprinkles[["sanitize_args"]] <- 
      deparse(sprinkles[["sanitize_args"]]) %>%
      paste0(collapse = "")
  }

  #* Sprinkles in the `simple` group.
  #* These sprinkles do not associate with any other sprinkles and may be
  #* directly modified without much difficulty.

  x <- simple_sprinkles(x = x, 
                        sprinkles = sprinkles, 
                        part = part, 
                        indices = indices)

  #* Sprinkles in the bg_pattern group
  if (any(names(sprinkles)  %in% 
      SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "bg_pattern"]))
  {
    x <- bg_pattern_sprinkles(x = x, 
                              part = part, 
                              indices = indices, 
                              bg_pattern = sprinkles[["bg_pattern"]], 
                              bg_pattern_by = sprinkles[["bg_pattern_by"]])
  }

  #* Sprinkles in the `border` group
  if (any(names(sprinkles) %in% 
      SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "border"]))
  {
    x <- border_sprinkles(x = x, 
                          part = part, 
                          indices = indices,
                          border = sprinkles[["border"]],
                          border_thickness = sprinkles[["border_thickness"]],
                          border_units = sprinkles[["border_units"]],
                          border_style = sprinkles[["border_style"]],
                          border_color = sprinkles[["border_color"]])
  }
  
  #* Sprinkles in the `discrete` group
  
  if (any(names(sprinkles) %in%
          SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "discrete"]))
  {
    x <- discrete_sprinkles(x = x,
                            part = part,
                            indices = indices,
                            discrete = sprinkles[["discrete"]],
                            discrete_colors = sprinkles[["discrete_colors"]],
                            border_style = sprinkles[["border_style"]],
                            border_thickness = sprinkles[["border_thickness"]],
                            border_units = sprinkles[["border_units"]])
  }

  #* Sprinkles in the `font_size` group
  if (any(names(sprinkles) %in% 
          SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "font_size"]))
  {
    x <- font_size_sprinkles(x = x, 
                             part = part, 
                             indices = indices,
                             font_size = sprinkles[["font_size"]],
                             font_size_units = sprinkles[["font_size_units"]])
  }
  
  #* Sprinkles in the `gradient` group
  
  if (any(names(sprinkles) %in%
          SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "gradient"]))
  {
    x <- gradient_sprinkles(x = x,
                            part = part,
                            indices = indices,
                            gradient = sprinkles[["gradient"]],
                            gradient_colors = sprinkles[["gradient_colors"]],
                            gradient_cut = sprinkles[["gradient_cut"]],
                            gradient_n = sprinkles[["gradient_n"]],
                            gradient_na = sprinkles[["gradient_na"]],
                            border_style = sprinkles[["border_style"]],
                            border_thickness = sprinkles[["border_thickness"]],
                            border_units = sprinkles[["border_units"]])
  }
  
  #* Sprinkles in the `height` group
  if (any(names(sprinkles) %in% 
          SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "height"]))
  {
    x <- height_sprinkles(x = x, 
                          part = part, 
                          indices = indices,
                          height = sprinkles[["height"]],
                          height_units = sprinkles[["height_units"]])
  }
  
  #* Sprinkles in the `merge` group
  if (any(names(sprinkles) %in% 
          SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "merge"]))
  {
    x <- merge_sprinkles(x = x, 
                         part = part, 
                         indices = indices,
                         merge = sprinkles[["merge"]],
                         merge_rowval = sprinkles[["merge_rowval"]],
                         merge_colval = sprinkles[["merge_colval"]])
  }

  #* Sprinkles in the `width` group
  if (any(names(sprinkles) %in% 
          SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "width"]))
  {
    x <- width_sprinkles(x = x, 
                         part = part, 
                         indices = indices,
                         width = sprinkles[["width"]],
                         width_units = sprinkles[["width_units"]])
  }
  
  #* Restore original sorting
  x[[part]] <- dplyr::arrange(x[[part]], col, row)
  
  x
}


# sprinkle.dust_list ------------------------------------------------
#' @rdname sprinkle
#' @export

sprinkle.dust_list <- function(x, rows = NULL, cols = NULL, ...,
                               part = c("body", "head", "foot", "interfoot", "table"))
{
  structure(
    lapply(X = x,
           FUN = sprinkle,
           rows = rows,
           cols = cols,
           part = part,
           ...),
    class = "dust_list"
  )
}

#**********************************************************
#**********************************************************
#* Sprinkle functions
#* These functions are not exported.  They perform the 
#* actual work of sprinkling.  Sprinkles have been divided
#* into groups of similar content for ease of management.
#* Sprinkles that are part of a system are grouped together,
#* and sprinkles that have similar features are grouped 
#* together.
#*
#* 1. option_sprinkles
#* 2. simple_sprinkles
#* 3. bg_pattern_sprinkles
#* 4. border_sprinkles
#* 5. font_size sprinkles
#* 6. height_sprinkles
#* 7. merge_sprinkles
#* 8. width_sprinkles


# option sprinkles --------------------------------------------------
#* These are sprinkles that can also be set in the `dust` call.
#* Generally, they apply to the entire table object, not just
#* to one of the parts. `longtable`, `float`, and `caption` 
#* are examples

option_sprinkles <- function(x, sprinkles)
{
  which_option <- 
    which(names(sprinkles) %in%
            SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "option"])
  
  for (spr in names(sprinkles)[which_option])
  {
    x[[spr]] <- sprinkles[[spr]]
  }
  
  x
}

# simple_sprinkles --------------------------------------------------
#* This group comprises the majority of sprinkles.  These 
#* are sprinkles that impact a component of a table but 
#* do not require interaction with other sprinkles.
#* Examples include `bold`, `italic`, and `font_color`

simple_sprinkles <- function(x, sprinkles, part, indices)
{
  which_simple <- 
    which(names(sprinkles) %in%
            SprinkleRef[["sprinkle"]][SprinkleRef[["group"]] == "simple"])

  for (spr in names(sprinkles)[which_simple])
  {
    x[[part]][[spr]][indices] <- 
      switch(spr,
             "fn" = deparse(sprinkles[[spr]]),
             "pad" = format(sprinkles[[spr]], scientific = FALSE),
             "rotate_degree" = format(sprinkles[[spr]], scientific = FALSE),
             sprinkles[[spr]])
  }
  
  x
}

# bg_pattern_sprinkles ----------------------------------------------
#* For striping by either row or column, both the 
#* `bg_pattern` and `bg_pattern_by` sprinkles require a 
#* value.  For convenience of the user, assigning one of 
#* these a value will still work and we assign the other
#* a default value.

bg_pattern_sprinkles <- function(x, part, indices, bg_pattern, bg_pattern_by)
{
  #* Assign default values
  if (is.null(bg_pattern)) bg_pattern <- c("#FFFFFF", "#DDDDDD")
  if (is.null(bg_pattern_by)) bg_pattern_by <- "rows"
  
  #* Use only the first element of `bg_pattern_by` (in case the user
  #* provided more than one value).
  if (length(bg_pattern_by) > 1) bg_pattern_by <- bg_pattern_by[1]
  
  if (bg_pattern_by == "rows")
  {
    pattern <- data.frame(row = sort(unique(x[[part]][["row"]][indices])))
    pattern[["bg"]] <- rep(bg_pattern, 
                           length.out = nrow(pattern))
    
    pattern <- 
      dplyr::left_join(pattern,
                       dplyr::select(x[[part]][indices, ], 
                                     row, col),
                     by = c("row" = "row")) %>%
      dplyr::arrange(col, row)
   
    x[[part]][["bg"]][indices] <- pattern[["bg"]]
  }
  else 
  {
    pattern <- data.frame(col = sort(unique(x[[part]][["col"]][indices])))
    pattern[["bg"]] <- rep(bg_pattern, 
                           length.out = nrow(pattern))
    
    pattern <- 
      dplyr::left_join(pattern,
                       dplyr::select(x[[part]][indices, ], 
                                     row, col),
                       by = c("col" = "col")) %>%
      dplyr::arrange(col,row)
    
    x[[part]][["bg"]][indices] <- pattern[["bg"]]
  }
  
  x
}

# border_sprinkles --------------------------------------------------
#* The cell borders are perhaps the most complex system
#* of sprinkles, requiring a side, thickness, unit, style,
#* and color to be defined.  To simplify calls for the 
#* user, defining at least one will result any remaining, 
#* undefined sprinkles to take a default value.

border_sprinkles <- function(x, part, indices,
                             border, border_thickness,
                             border_units, border_style, 
                             border_color)
{
  if (is.null(border)) border <- c("bottom", "left", "top", "right")
  if ("all" %in% border) border <- c("bottom", "left", "top", "right")
  if (is.null(border_thickness)) border_thickness <- 1
  if (is.null(border_units)) border_units <- "px"
  if (is.null(border_style)) border_style <- "solid"
  if (is.null(border_color)) border_color <- "Black"
  
  border_define <- sprintf("%s%s %s %s",
                           border_thickness,
                           border_units,
                           border_style,
                           border_color)
  for (side in border){
    x[[part]][[sprintf("%s_border", side)]][indices] <- border_define
  }
  
  x
}

# font_size_sprinkles -----------------------------------------------
#* The default font size is to allow the style/format of 
#* the document to dictate the size.  Thus, declaring a
#* font_size_unit without a font_size won't actually do
#* anything.  On the other hand, if a font_size is 
#* declared, but no units, the unit "pt" is assumed.

font_size_sprinkles <- function(x, part, indices,
                             font_size, font_size_units)
{
  if (is.null(font_size)) font_size <- ""
  if (is.null(font_size_units)) font_size_units <- "pt"

  x[[part]][["font_size"]][indices] <- format(font_size, scientific = FALSE)
  x[[part]][["font_size_units"]][indices] <- font_size_units
  
  x
}

# height_sprinkles --------------------------------------------------
#* Behaves similarly to `font_size_sprinkles`  With some
#* cleverness, I could probably come up with a way to 
#* work these into one function, but I'm not sure the
#* generalization would create much improvement in 
#* performance.

height_sprinkles <- function(x, part, indices,
                             height, height_units)
{
  if (is.null(height)) height = ""
  if (is.null(height_units)) height_units <- "pt"
  
  x[[part]][["height"]][indices] <- format(height, scientific = FALSE)
  x[[part]][["height_units"]][indices] <- height_units
  
  x
}

# merge_sprinkles ---------------------------------------------------
#* This is the most difficult sprinkle, conceptually speaking.
#* This handles merging cells.  
#* Merged cells require a few components
#*   a. display cell definition: The cell in the merged group 
#*      which has the content to be displayed.  By default, 
#*      this is the smallest row number/cell number combination.
#*   b. rowspan definition: The number of rows the merged
#*      cell should span.
#*   c. colspan definition: The number of cells the merged
#*      cell should span. 
#* 
#* Non-display cells are assigned a rowspan and colspan of 0.
#* This suppresses them from printing, but preserves the content 
#* of the cell within the `dust` object. Preservation is imporant
#* in case a merged cell is unmerged in a subsequent call.

merge_sprinkles <- function(x, part, indices,
                             merge, merge_rowval, merge_colval)
{
  #* If the `merge` argument is NULL or FALSE, there's nothing to 
  #* be done.  Return `x`
  if (is.null(merge)) return(x)
  if (!merge) return(x)
  
  #* If the display row and column aren't specified, choose the 
  #* minimum row or cell.
  if (is.null(merge_rowval)) merge_rowval <- min(x[[part]][["row"]][indices])
  if (is.null(merge_colval)) merge_colval <- min(x[[part]][["col"]][indices])
  
  #* Map the cells to the display cell
  x[[part]][["html_row"]][indices] <- as.integer(merge_rowval)
  x[[part]][["html_col"]][indices] <- as.integer(merge_colval)
  
  #* Set colspan and rowspan of non-display cells to 0.  This suppresses 
  #* them from display.
  x[[part]][["rowspan"]][indices] [x[[part]][["row"]][indices] != merge_rowval] <- 0L
  x[[part]][["colspan"]][indices] [x[[part]][["col"]][indices] != merge_colval] <- 0L
  
  #* Record the upper left most cell of the merged area.
  #* This will be needed for HTML table to place the cell in the correct
  #* location.
  x[[part]][["html_row_pos"]][indices] <- as.integer(min(x[[part]][["row"]][indices]))
  x[[part]][["html_col_pos"]][indices] <- as.integer(min(x[[part]][["col"]][indices]))
  
  #* Set the colspan and rowspan of the display cells.
  x[[part]][["rowspan"]][indices] [x[[part]][["row"]][indices] == merge_rowval] <- 
    x[[part]][["row"]][indices] %>%
    unique() %>%
    length() %>%
    as.integer()
  
  x[[part]][["colspan"]][indices] [x[[part]][["col"]][indices] == merge_colval] <- 
    x[[part]][["col"]][indices] %>%
    unique() %>%
    length() %>%
    as.integer()
  
  x
}

# width_sprinkles ---------------------------------------------------
#* See the note for '5. height_sprinkles'

width_sprinkles <- function(x, part, indices,
                             width, width_units)
{
  if (is.null(width)) width = ""
  if (is.null(width_units)) width_units <- "pt"
  
  x[[part]][["width"]][indices] <- format(width, scientific = FALSE)
  x[[part]][["width_units"]][indices] <- width_units
  
  x
}

# Color sprinkles ---------------------------------------------------

color_sprinkles <- function(sprinkles, coll)
{

  sprinkles <- 
    lapply(sprinkles,
           function(x)
           {
             x <- gsub("[[:space:]]", "", x)
             x[x == "transparent"] <- "rgba(255,255,255,0.0)"
             x
           }
    )
  for (i in seq_along(sprinkles))
  {
    is_color <- tolower(sprinkles[[i]]) %in% tolower(grDevices::colors())
    is_rgb <- grepl("^rgb[(]\\d{1,3},\\d{1,3},\\d{1,3}[)]",sprinkles[[i]]) | 
              grepl("^rgba[(]\\d{1,3},\\d{1,3},\\d{1,3},(\\d{1,4}|)[.]\\d{1,9}[)]$", sprinkles[[i]])  | 
              grepl("^rgba[(]\\d{1,3},\\d{1,3},\\d{1,3},0[)]$", sprinkles[[i]])
    is_hex <- grepl("^#[0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f]$", sprinkles[[i]]) | 
              grepl("^#[0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f][0-9,A-F,a-f]$", sprinkles[[i]])
    
    not_colors <- sprinkles[[i]][!(is_color | is_rgb | is_hex)]
    
    if (length(not_colors))
    {
      coll$push(sprintf("Colors in '%s' are not valid colors: %s",
                        names(sprinkles)[i],
                        paste0(not_colors, collapse = ", ")))
    }

    sprinkles[[i]][is_color | is_hex] <- 
      vapply(sprinkles[[i]][is_color | is_hex],
             function(x)
             {
               col <- grDevices::col2rgb(x, alpha = TRUE) 
               if (length(col) == 4) col[4] <- col[4] / 255
               
               paste0(col, collapse = ",") %>%
               sprintf(fmt = "rgba(%s)",
                       .)
             },
             character(1)
      )
               
  }
  sprinkles
}

# Discrete sprinkles ------------------------------------------------

discrete_sprinkles <- function(x, part, indices,
                               discrete, discrete_colors,
                               border_thickness, border_units,
                               border_style)
{
  discrete["font" %in% discrete] <- "font_color"
  
  if ("border" %in% discrete)
  {
    discrete <- c(sprintf("%s_border", 
                          c("top", "left", "right", "bottom")),
                  discrete)
    discrete <- unique(discrete[!discrete %in% "border"])
  }
  
  ux <- unique(x[[part]][["value"]][indices])
  
  if (is.null(discrete_colors)) 
  {
    discrete_colors <- getOption("pixie_discrete_pal", NULL)
  }
  
  if (is.null(discrete_colors))
  {
    discrete_colors <- scales::hue_pal()(length(ux))
  }

  checkmate::makeAssertion(x = discrete_colors,
                           if (length(discrete_colors) >= length(ux))
                           {
                             TRUE
                           }
                           else
                           {
                             sprintf("`discrete_color` must have at least the same length as the number of unique values (>= %s)",
                                           length(ux))
                           },
                           var.name = "discrete_color",
                           collection = NULL)
  
  if (is.null(border_thickness)) border_thickness <- 1
  if (is.null(border_units)) border_units <- "px"
  if (is.null(border_style)) border_style <- "solid"

  for (i in seq_along(discrete))
  {
    if (grepl("border", discrete[i]))
    {
      x[[part]][[discrete[i]]][indices] <- 
        sprintf("%s%s %s %s",
                border_thickness,
                border_units,
                border_style,
                discrete_colors[as.numeric(as.factor(x[[part]][["value"]][indices]))])
    }
    else 
    {
      x[[part]][[discrete[i]]][indices] <- 
        discrete_colors[as.numeric(as.factor(x[[part]][["value"]][indices]))]
    }
  }
  x
}

# Gradient Sprinkles ------------------------------------------------

gradient_sprinkles <- function(x, part, indices,
                               gradient, gradient_colors,
                               gradient_cut, gradient_n,
                               gradient_na,
                               border_thickness, border_units,
                               border_style)
{
  
  checkmate::assert_subset(x = x[[part]][["col_class"]][indices],
                           choices = c("numeric", "integer", "double"),
                           empty.ok = FALSE)
  
  gradient["font" %in% gradient] <- "font_color"
  
  if (is.null(gradient_n)) gradient_n <- 10
  
  if (is.null(gradient_na))
  {
    gradient_na <- "grey"
  }
  
  if ("border" %in% gradient)
  {
    gradient <- c(sprintf("%s_border", 
                          c("top", "left", "right", "bottom")),
                  gradient)
    gradient <- unique(gradient[!gradient %in% "border"])
  }
  
  if (is.null(gradient_colors))
  {
    gradient_colors <- getOption("pixie_gradient_pal", 
                                 c("#132B43", "#56B1F7"))
  }
  
  gradient_colors <- 
    scales::gradient_n_pal(gradient_colors)(seq(0, 1, length.out = gradient_n))
  
  if (is.null(border_thickness)) border_thickness <- 1
  if (is.null(border_units)) border_units <- "px"
  if (is.null(border_style)) border_style <- "solid"
  
  gradient_split <- 
    if (is.null(gradient_cut))
    {
    cut(as.numeric(x[[part]][["value"]][indices]),
        breaks = stats::quantile(as.numeric(x[[part]][["value"]][indices]), 
                                 probs = seq(0, 1, length.out = gradient_n),
                                 na.rm = TRUE),
        include.lowest = TRUE)
    }
    else
    {
      cut(as.numeric(x[[part]][["value"]][indices]),
          breaks = gradient_cut,
          include.lowest = TRUE,
          na.rm = TRUE)
    }
  
  na_val <- which(is.na(gradient_split))
  
  for (i in seq_along(gradient))
  {
    if (grepl("border", gradient[i]))
    {
      x[[part]][[gradient[i]]][indices] <- 
        sprintf("%s%s %s %s",
                border_thickness,
                border_units,
                border_style,
                gradient_colors[as.numeric(gradient_split)])
      
      x[[part]][[gradient[i]]][indices][na_val] <- 
        sprintf("%s%s %s %s",
                border_thickness,
                border_units,
                border_style,
                gradient_na)
    }
    else 
    {
      x[[part]][[gradient[i]]][indices] <- 
        gradient_colors[as.numeric(gradient_split)]
      
      x[[part]][[gradient[i]]][indices][na_val] <- 
        gradient_na
    }
  }
  
  x
}

# assert_sprinkles --------------------------------------------------
#* Early versions of `pixiedust` performed a long series
#* of checks that tested for the existence of a sprinkle 
#* and then tested the characteristics of the sprinkle.
#* This was time consuming and tedious to write.
#* The function below allows us to test only the 
#* sprinkles that are given, eliminating the need to 
#* test for the existence of sprinkles.  It also uses
#* `checkmate`, which is somewhat faster.
#* The `assert*` function used for each sprinkle is 
#* defined in the `SprinkleRef` data frame, which 
#* exists in the 'R/sysdata.rda` object, and is defined
#* by the `inst/sprinkle_ref.csv` file.  The 
#* arguments for the checks are also defined in that 
#* data frame.

assert_sprinkles <- function(sprinkles, coll, recycle)
{
  #* The longtable sprinkle needs some special love.
  #* It may be either logical or numerical, with values less
  #* than 0 being interpreted as FALSE
  if ("longtable" %in% names(sprinkles))
  {
    if (!is.numeric(sprinkles[["longtable"]]) & !is.logical(sprinkles[["longtable"]]))
    {
      coll$push("`longtable` must be either logical or numerical")
    }
  } #* END if ("longtable" %in% names(sprinkles))
  
  else
  {
    for (i in seq_along(sprinkles))
    {
      #* Determine the reference row in the `SprinkleRef` data frame
      #* This data frame is saved to /R/sysdata.rda
      ref_row <- which(SprinkleRef[["sprinkle"]] == names(sprinkles)[i])
      
      #* For the `fn` sprinkle, we need to rewrap it in `quote` 
      #* to prevent checkmate from trying to evaluate it 
      #* (this isn't a problem with checkmate, but a result 
      #* of how we're passing the call.
      if (inherits(sprinkles[[i]], "call"))
      {
        sprinkles[[i]] <- quote(sprinkles[[i]])
      }

      #* Arguments to the assert functions are prefixed with 'arg_'
      #* First we extract them from `SprinkleRef`, then we 
      #* remove any missing values.
      args <- SprinkleRef[ref_row, 
                          names(SprinkleRef)[grepl("arg_", names(SprinkleRef))],
                          drop = FALSE]
      
      args <- lapply(X = args,
                     FUN = function(x) if (is.na(x)) NULL else x)
      args <- args[!vapply(args, is.null, logical(1))]
      
      #* For assertions with a `choices` argument, convert the character string
      #* to a vector.
      if (any(names(args) == "arg_choices"))
      {
        args[["arg_choices"]] <- eval(parse(text = args[["arg_choices"]]))
      }

      #* If recycling, remove the length 1 constraint
      if (recycle != "none")
      {
        args[["arg_len"]] <- NULL
      }
      
      #* Remove the 'arg_' prefix from the argument names.
      names(args) <- sub(pattern = "arg_",
                         replacement = "",
                         x = names(args))


      do.call(
        what = #* generate the function call
          eval(
            parse(
              text = 
                sprintf("%s%s",
                        if (SprinkleRef[["assert_fn"]][ref_row] == "assert_match_arg")
                        {
                          ""
                        }
                        else
                        {
                          "checkmate::"
                        },
                        SprinkleRef[["assert_fn"]][ref_row])
            )
          ),
        args = c(list(sprinkles[[i]], 
                      add = coll,
                      .var.name = names(sprinkles)[i]),
                 args)
      )
    }  #* End for loop
  } #* End else
}