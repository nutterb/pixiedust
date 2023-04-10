#' @name tidy_levels_labels 
#' 
#' @title Term and Level Descriptions for \code{pixiedust} Tables
#' 
#' @description Default model objects identify rows of results with 
#'   appropriate term name.  More often than not, the term name is 
#'   not suitable for formally reported output.  \code{tidy_levels_labels}
#'   performs some basic work to quickly provide more readable 
#'   descriptors for cases where they can easily be obtained.  These
#'   descriptors are retrieved from the data, however, so the 
#'   utility is determined by the user's habits in providing 
#'   term labels and meaningful factor levels.  
#'   
#'   Due to the complexity of the terms that could be used for a model,
#'   it isn't practical to attempt to recover human-ready descriptors
#'   for every conceivable term.  This would require recovering variable
#'   names for any number of functions.  \code{pixiedust} only 
#'   goes after the easiest to obtain. Replacements no managed by 
#'   \code{tidy_levels_labels} may still be made with the \code{replace}
#'   sprinkle.
#'   
#' @param object A model object, ideally with a \code{model.frame} method.
#'   It is unclear at the moment (18 Sept. 2015) what will happen if
#'   an object is passed that does not have a \code{model.frame} method.
#' @param descriptors A character vector indicating the descriptors to
#'   be used in the table.  Acceptable inputs are \code{"term"}, 
#'   \code{"term_plain"}, \code{"label"}, \code{"level"}, and 
#'   \code{"level_detail"}.  These may be used in any combination and
#'   any order, with the descriptors appearing in the table from left
#'   to right in the order given.  The default, \code{"term"}, returns
#'   only the term descriptor and is identical to the output provided
#'   by \code{broom::tidy} methods.  See Details for a full explanation
#'   of each option and the Examples for sample output.
#' @param numeric_level A character string that determines which descriptor
#'   is used for numeric variables in the \code{"level_detail"} descriptor
#'   when a numeric has an interaction with a factor.  Acceptable inputs
#'   are \code{"term"}, \code{"term_plain"}, and \code{"label"}.
#' @param argcheck An assert collection created by \code{checkmate::makeAssertCollection}.
#'   Under normal circumstances, this is passed from \code{dust}.  If \code{NULL},
#'   as in the case it is run outside of \code{dust}, a new collection is
#'   created and the assertions are reported within \code{tidy_levels_labels}.
#'   
#' @details The user may select up to five columns of descriptors, 
#'   although doing so would certainly create some ambiguity.  See
#'   the Examples for sample output.
#'   \itemize{
#'     \item{\code{"term"} }{The term name used in the R model summary}
#'     \item{\code{"term_plain"} }{The term name used in the formula.
#'       For variables that produce multiple term names (such as factors),
#'       the plain term name may be duplicated.  For example, a factor that
#'       has term names \code{FctrB} and \code{FctrC}, indicating rows for 
#'       levels \code{B} and \code{C} of the variable \code{Fctr}, will 
#'       have two rows of \code{"term_plain"} of just \code{Fctr}.}
#'     \item{\code{"label"} }{Provides the label attached to the data using
#'       \code{labelVector::get_label}.  When a term is not associated with a label, 
#'       the value of \code{term_plain} is returned instead. Note that, variable names
#'       will disassociate with a label if they are used in a function (such
#'       as \code{factor(x)} or \code{x^2}.}
#'     \item{\code{"level"} }{Indicates the level being compared within a factor 
#'       (or an interaction involving a factor), otherwise it returns \code{NA}.
#'       It may also be said that this value is the appendix to a factor name.  
#'       For the term \code{FctrB}, this would just be \code{B}.}
#'     \item{\code{"level_detail"} }{Gives additional information to \code{level}
#'       by including the reference level of the factor.  For the term \code{FctrB},
#'       this would return \code{"B vs A"}.  When an interaction with a numeric
#'       variable is present, the \code{level} for the numeric may be either 
#'       \code{term_plain} or \code{label}, the choice being controlled by the
#'       \code{level_detail} argument.}
#'   }
#'   
#' @section Restrictions:
#' The descriptors, other than \code{"term"}, generally don't make sense for data 
#' frame objects.  The use of \code{tidy_levels_labels} is not permitted within
#' the \code{dust} function, but is allowed if you really want it by 
#' \code{pixiedust:::tidy_levels_labels}.
#' 
#' Other special cases noted in future uses will be documented here, but in 
#' general, if it isn't a model object, you probably don't really want to 
#' use this.
#'       
#' @author Benjamin Nutter
#' 
#' @examples 
#' #* Descriptors for lm output with no interactions
#' mtcars2 <- mtcars
#' mtcars2$mpg <- labelVector::set_label(mtcars2$mpg, "Gas Mileage")
#' mtcars2$qsec <-  labelVector::set_label(mtcars2$qsec, "Quarter Mile Time")
#' mtcars2$am <-  labelVector::set_label(mtcars2$am, "Transmission")
#' mtcars2$wt <-  labelVector::set_label(mtcars2$wt, "Weight")
#' mtcars2$gear <-  labelVector::set_label(mtcars2$gear, "Gears")
#' 
#' #* Basic Output for a model with no interactions
#' #* Note: numeric_level has no impact as there are no
#' #*       interactions involving numeric variables.
#' 
#' fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars2)
#' 
#' pixiedust:::tidy_levels_labels(fit, 
#'   descriptors = c("term", "term_plain", "label", "level", "level_detail"),
#'   numeric_level = "term") 
#'   
#' #* Assign factors ahead of the model. This allows 
#' #* the user to determine the levels that display.
#' #* Compare the output for 'am' with the output for 'gear'
#' 
#' mtcars2$am <- factor(mtcars2$am, 0:1, c("Automatic", "Manual"))
#' mtcars2$am <-  labelVector::set_label(mtcars2$am, "Transmission") 
#'     # Label was lost in variable conversion
#' fit <- lm(mpg ~ qsec + am + wt + factor(gear), data = mtcars2)
#' pixiedust:::tidy_levels_labels(fit, 
#'   descriptors = c("term", "term_plain", "label", "level", "level_detail"),
#'   numeric_level = "term") 
#'   
#'   
#' #* Include an interaction between a factor and numeric.
#' 
#' fit <- lm(mpg ~ qsec + am * wt + factor(gear), data = mtcars2)
#' pixiedust:::tidy_levels_labels(fit, 
#'   descriptors = c("term", "term_plain", "label", "level", "level_detail"),
#'   numeric_level = "term") 
#'   
#' #* Now observe how 'level' and 'level_detail' change 
#' #* in the interaction terms as we choose different 
#' #* values for 'numeric_level'
#' 
#' pixiedust:::tidy_levels_labels(fit, 
#'   descriptors = c("term", "term_plain", "label", "level", "level_detail"),
#'   numeric_level = "term_plain")
#'   
#' pixiedust:::tidy_levels_labels(fit, 
#'   descriptors = c("term", "term_plain", "label", "level", "level_detail"),
#'   numeric_level = "label")  

tidy_levels_labels <- function(object, 
                               descriptors = "term", 
                               numeric_level = c("term", "term_plain", "label"),
                               argcheck = NULL){
  independent_check <- is.null(argcheck)
  if (is.null(argcheck)) argcheck <- checkmate::makeAssertCollection()
  
  numeric_level <- checkmate::matchArg(x = numeric_level,
                                    choices = c("term", "term_plain", "label"),
                                    add = argcheck)
  
  descriptors <- checkmate::matchArg(x = descriptors,
                                  choices = c("term", "term_plain", "label",
                                              "level", "level_detail"),
                                  several.ok = TRUE,
                                  add = argcheck)

  if (independent_check) checkmate::reportAssertions(argcheck)
  
  lnl <- levels_and_labels(object) %>%
    level_label_interactions(as.data.frame(broom::tidy(object)), numeric_level)
  
  if (! "term" %in% descriptors)
    lnl[, c("term", descriptors), drop = FALSE]
  else 
    lnl[, descriptors, drop = FALSE]
}

levels_and_labels <- function(object, ...){
  model_data <- stats::model.frame(object)
  Labels <- labelVector::get_label(model_data, names(model_data))
  NLevels <- vapply(model_data, modelNLevels, 1)
  Levels <- 
    lapply(model_data, 
           modelFriendlyLevels) 
  Levels <- do.call(.rbind_internal, Levels) 
  
  Levels$term_plain <- rep(names(NLevels), NLevels)
  Levels$term <- paste0(Levels$term, Levels$level)
  Levels$label <- Labels[match(Levels$term_plain, names(Labels))]

  Levels <- Levels[, c("term", "term_plain", "label", "level", "level_detail")]
  rownames(Levels) <- NULL
  Levels
}



modelFriendlyLevels <- function(f){
  lev <- levels(f)
  if (is.null(lev))
    return(data.frame(level = "",
                      level_detail = "",
                      stringsAsFactors = FALSE))
  else 
    return(data.frame(level = lev[-1],
                      level_detail = paste0(lev[-1], " vs. ", lev[1]),
                      stringsAsFactors = FALSE))
}

modelNLevels <- function(f){
  nlev <- nlevels(f)
  nlev <- if (nlev == 0) 1 else (nlev-1)
  nlev
}


level_label_interactions <- function(lnl, tidy_fit, numeric_level){
  if (!any(grepl("[:]", tidy_fit$term)))
    return(lnl)
  else{
    inters <- which(grepl("[:]", tidy_fit$term))
    splits <- strsplit(tidy_fit$term[inters], "[:]")
    inters <- lapply(splits, form_interaction_labels, lnl, numeric_level) 
    inters <- do.call(".rbind_internal", inters)

    .rbind_internal(lnl, inters)
  }
}


form_interaction_labels <- function(s, lnl, numeric_level){
  m <- match(s, lnl$term)

  level <- ifelse(lnl$level[m] == "",
                  lnl[[numeric_level]][m],
                  lnl$level[m])

  level_detail <- ifelse(lnl$level_detail[m] == "",
                         lnl[[numeric_level]][m],
                         lnl$level_detail[m])

  data.frame(term = paste0(lnl$term[m], collapse = ":"),
             term_plain = paste0(lnl$term_plain[m], collapse = ":"),
             label = paste0(lnl$label[m], collapse = ":"),
             level = paste0(level, collapse = ":"),
             level_detail = paste0(level_detail, collapse = ":"),
             stringsAsFactors = FALSE)
}