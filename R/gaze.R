#' @name gaze
#' @title Mimic Stargazer Output to Display Multiple Models
#' 
#' @description Tidy multiple models and display coefficients and 
#'   test statistics in a side-by-side format.
#'   
#' @param ... models to be tidied.  Arguments may be named or unnamed.
#'   For named arguments, the model will be identfied by the argument 
#'   name; for unnamed arguments, the object name will be the identifier.
#' @param include_glance \code{logical(1)} Determines if \code{glance} (fit)
#'   statistics are displayed under the models.
#' @param glance_vars \code{character}. A vector of statistics returned by
#'   \code{glance} that are to be displayed for each model. Defaults are 
#'   subject to change in future versions.
#' @param digits \code{numeric(1)} The number of digits used for rounding.
#' 
#' @details This function is still in development.  Significant stars 
#'   will be added in a future version. Note that function defaults may 
#'   be subject to change.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return a data frame object
#'   \item Cast an error if \code{include_glance} is not \code{logical(1)}
#'   \item Cast an error if \code{glance_vars} is not a \code{character} 
#'     vector.
#'   \item Cast an error if \code{digits} is not \code{"integerish(1)"}.
#' }
#'
#' @examples 
#' fit1 <- lm(mpg ~ qsec + am + wt + gear + factor(vs), data = mtcars)
#' fit2 <- lm(mpg ~ am + wt + gear + factor(vs), data = mtcars)
#' 
#' gaze(fit1, fit2)
#' gaze(with_qsec = fit1, 
#'      without_qsec = fit2)
#' gaze(fit1, fit2, include_glance = FALSE)
#' gaze(fit1, fit2, glance_vars = c("AIC", "BIC"))
#' 
#' @export

gaze <- function(..., include_glance = TRUE,
                 glance_vars = c("adj.r.squared", "sigma", "AIC"),
                 digits = 3){
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_logical(x = include_glance,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = glance_vars,
                              add = coll)
  
  checkmate::assert_integerish(x = digits,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  fits <- list(...)
  if (is.null(names(fits))) names(fits) <- character(length(fits))
  
  # If a fit isn't named, use the object name
  dots <- match.call(expand.dots = FALSE)$...
  fit_names <- vapply(dots, deparse, character(1))
  names(fits)[names(fits) == ""] <- fit_names[names(fits) == ""]

  res <- prep_gaze_tidy(fits, names(fits), digits)
  if (include_glance){
    res <- rbind(res, 
                 prep_gaze_glance(fits, names(fits), glance_vars, digits))
  }
  res
}


# UNEXPORTED METHODS ------------------------------------------------

prep_gaze_tidy <- function(fits, fit_names, digits){
  res <- 
    mapply(
      FUN = 
        function(fit, name)
        {
          data.frame(model = name,
                     broom::tidy(fit),
                     stringsAsFactors = FALSE)
        },
      fit = fits,
      name = fit_names,
      SIMPLIFY = FALSE
    ) 
  
  res <- dplyr::bind_rows(res)
  
  res <- res[c("model", "term", "estimate", "statistic")]
  res[["term"]] <- factor(res[["term"]], 
                          levels = unique(res[["term"]]))
  
  res <-
    stats::reshape(
      data = res,
      direction = "long",
      varying = list(value = c("estimate", "statistic")),
      v.names = "value",
      timevar = "variable",
      times = c("estimate", "statistic")
    )
  
  rownames(res) <- NULL
  
  res[["value"]] <- round(res[["value"]], digits)
  statistic_row <- res[["variable"]] == "statistic"
  res[["value"]][statistic_row] <- 
    sprintf("(%s)",
            res[["value"]][statistic_row])
  
  res <- 
    stats::reshape(
      data = res[!names(res) %in% "id"],
      direction = "wide",
      v.names = "value",
      idvar = c("term", "variable"),
      timevar = c("model"))
  
  res <- res[order(res[["term"]], res[["variable"]]), ]
  names(res) <- sub("^value\\.", "", names(res))
  res[!names(res) %in% "variable"]
}


prep_gaze_glance <- function(fits, fit_names, glance_vars, digits){
  res <- 
    mapply(
      FUN = 
        function(fit, name)
        {
          data.frame(model = name,
                     broom::glance(fit),
                     stringsAsFactors = FALSE)
        },
      fit = fits,
      name = fit_names,
      SIMPLIFY = FALSE
    ) 
  
  res <- dplyr::bind_rows(res)
  res <- res[c("model", glance_vars)]
  
  res <- 
    stats::reshape(
      data = res,
      direction = "long",
      times = glance_vars,
      varying = list(value = glance_vars)
    )
  
  names(res)[2:3] <- c("term", "value")
  res[["value"]] <- round(res[["value"]], digits)
  
  
  res <-
    stats::reshape(
      data = res[!names(res) %in% "id"],
      direction = "wide",
      v.names = "value",
      idvar = c("term"),
      timevar = c("model"))
  
  names(res) <- sub("^value\\.", "", names(res))
  rownames(res) <- NULL
  res
}
