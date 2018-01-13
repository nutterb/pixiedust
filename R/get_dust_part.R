#' @name get_dust_part
#' @title Get a Portion of the Table Stored in a \code{dust} Object
# Documentation -----------------------------------------------------
#' @description Making customized table headers and footers requires a
#'   data frame be added to the \code{dust} object that has the same
#'   column dimension as the rest of the table.  In order to reduce the 
#'   inconvenience of counting columns, \code{get_dust_part} extracts the 
#'   data frame portion currently in use.  This ensures the column dimension
#'   is correct with the current values, and provides an object suitable
#'   for editing.
#'   
#' @param x An object of class \code{dust}
#' @param part \code{character(1)}, naming the part of the table to 
#'   retrieve.  May be one of \code{"head"}, \code{"foot"}, \code{"interfoot"},
#'   or \code{"body"}.
#'   
#' @return an object of class \code{data.frame}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return, as a data frame, the part of the table requested in \code{part}
#'   \item Cast an error if \code{x} is not a \code{dust} object.
#'   \item Cast an error if \code{part} is not one of \code{c("head", "foot",
#'     "interfoot", "body")}
#' }
#' 
#' @export

# Function Definition -----------------------------------------------
get_dust_part <- function(x, part = c("head", "foot", "interfoot", "body"))
{
  
# Argument Validations ----------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  part <- 
    checkmate::matchArg(x = part,
                        choices = c("head", "foot", "interfoot", "body"),
                        add = coll)
  
  checkmate::reportAssertions(coll)
  
# Functional Code ---------------------------------------------------
  
  if (!is.null(x[[part]]))
  {
    X <- x[[part]][c("row", "col", "value")]
    X <- stats::reshape(X, 
                 direction = "wide",
                 timevar = "col",
                 idvar = "row")
    X <- X[-1]
    names(X) <- unique(x[[part]][["col_name"]])
  }
  else
  {
    col_names <- unique(x[["body"]][["col_name"]])
    X <- matrix(nrow=0, 
                ncol=length(col_names)) 
    X <- data.frame(X)
    names(X) <- col_names
  }
  
  X
}