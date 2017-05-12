#' @name glance_foot
#' 
#' @title Prepare Glance Statistics for \code{pixiedust} Table Footer
#' @description Retrieves the \code{broom::glance} output for a model object and 
#'   structures it into a table suitable to be placed in the footer.  By default,
#'   the statistics are displayed in two column-pairings (see Details).  This 
#'   function is not exported but is documented to maintain clarity of its 
#'   behavior.  It is intended for use within \code{dust}, but may be useful
#'   elsewhere if used with caution.
#'   
#' @param fit A model object with a \code{broom::glance} method.
#' @param col_pairs An integer indicating the number of column-pairings for the 
#'   glance output.  This must be less than half the total number of columns,
#'   as each column-pairing includes a statistic name and value.
#' @param total_cols The total number of columns in the body of the pixiedust table
#' @param glance_stats A character vector giving the names of the glance statistics
#'   to put in the output.  When \code{NULL}, the default, all of the available 
#'   statistics are retrieved.  In addition to controlling which statistics are 
#'   printed, this also controls the order in which they are printed.
#' @param byrow A logical, defaulting to \code{FALSE}, that indicates if the 
#'   requested statistics are placed with priority to rows or columns.  See Details.
#'   
#' @details Statistics are placed in column-pairings.  Each column pair consists of 
#'   two columns named \code{stat_name_x} and \code{stat_value_x}, where \code{x} is 
#'   the integer index of the column pair.  The column-pairings are used to allow 
#'   the user to further customize the output, more-so than pasting the name and 
#'   value together would allow.  With this design, statistics can be rounded 
#'   differently by applying sprinkles to the resulting table.
#'   
#'   The total number of column-pairings must be less than or equal to half the 
#'   number of total columns.  This constraint prevents making glance tables that 
#'   have more columns than the model table it accompanies.  
#'   
#'   When the total number of column-parings is strictly less than half the total 
#'   number of columns, "filler" columns are placed between the column pairings.
#'   As much as possible, the filler columns are placed evenly between the 
#'   column pairings, but when the number of filler columns is unequal between 
#'   column-pairings, there will be more space placed on the left side.  For example,
#'   if a table has 7 columns and 3 column-pairings, the order of placement would be
#'   column-pair-1, filler, column-pair-2, column-pair-3.  Since there was only room
#'   for one column of filler, it was placed in the left most fill position.
#'   
#'   The \code{byrow} arguments acts similarly to the \code{byrow} argument in the
#'   \code{matrix} function, but defaults to \code{FALSE}.  If four statistics are 
#'   requested and \code{byrow = FALSE}, the left column-pair will have statistics 
#'   one and two, while the right column-pair will have statistics three and four.
#'   If \code{byrow = TRUE}, however, the left column-pair will have statistics
#'   one and three, while the right column-pair will have statistics two and four.
#'   
#' @author Benjamin Nutter
#' 

glance_foot <- function(fit, col_pairs, total_cols, 
                        glance_stats = NULL, byrow = FALSE){
  #* col_pairs is less then half of total_cols
  #* glance_stats are all found in names(tidy(fit))
  
  g <- broom::glance(fit)
  
  coll <- checkmate::makeAssertCollection()
  
  if (col_pairs > total_cols/2)
  {
    coll$push("'col_pairs' must be less than 'total_cols/2'")
  }
  
  if (is.null(glance_stats)) 
    glance_stats <- names(g)
  else 
  {
    invalid_stats <- glance_stats[!glance_stats %in% names(g)]
    glance_stats <- glance_stats[glance_stats %in% names(g)]
    if (length(invalid_stats) > 0)
    {
      warning("The following statistics were requested but are not ",
              "available for models of class ", 
              paste0(class(fit), collapse = "; "), ":",
              "\n    ", paste0(invalid_stats, collapse = ", "))
    }
    
    if (length(glance_stats) == 0)
    {
      coll$push(
        sprintf("None of the statistics requested are available for models of class %s", 
                paste0(class(fit), collapse = "; "))
      )
    }
  }
  
  checkmate::reportAssertions(coll)
  
  g <- broom::tidy(t(g[glance_stats])) 
  # return(g)
  if (nrow(g) %% col_pairs > 0){
    n_fill <- (col_pairs - nrow(g) %% col_pairs)
    stat_fill <- data.frame(.rownames = rep("", n_fill),
                            x = rep(NA, n_fill),
                            stringsAsFactors = FALSE)
    g <- dplyr::bind_rows(g, stat_fill)
  }

  g$col <- 
    if (byrow) rep(1:col_pairs, length.out = nrow(g))
    else rep(1:col_pairs, each = nrow(g) / col_pairs)
  g$col <- factor(g$col)

  fill_cols <- total_cols - (col_pairs * 2)
  fill_gaps <- col_pairs - 1
  
  cols_per_gap <- ceiling(fill_cols / fill_gaps)
  total_fills <- fill_gaps * cols_per_gap
  
  fills_per_gap_times <- fill_cols %/% cols_per_gap
  if (!is.finite(fills_per_gap_times)) fills_per_gap_times <- 0
  fills_per_gap <- 
    c(rep(cols_per_gap, fills_per_gap_times),
      fill_cols %% cols_per_gap)
  fills_per_gap <- fills_per_gap[fills_per_gap > 0]
  
  Filler <- lapply(fills_per_gap,
         build_fill,
         rows = nrow(g) / col_pairs)
  
  G <- split(g, g$col)
  
  if (length(Filler) < length(G)) 
    Filler <- c(Filler, lapply(1:(length(G) - length(Filler)),
                               function(i) NULL))
  
  G <- mapply(intersplice_fill,
         G,
         Filler,
         SIMPLIFY = FALSE) %>%
    dplyr::bind_cols()
  
  names(G) <- make.unique(names(G))
  G

}
  

build_fill <- function(fills_per_gap, rows){
  if (is.na(fills_per_gap)) return(NULL)
  Fills <- lapply(1:fills_per_gap,
                  function(f)
                    data.frame(fill = rep("", rows),
                               stringsAsFactors = FALSE)) 
  dplyr::bind_cols(Fills)
}

intersplice_fill <- function(G, Fill){
  if (!is.null(Fill)) return(dplyr::bind_cols(G[1:2], Fill))
  else return(G[1:2])
}
