#' @importFrom poorman bind_rows
#' @importFrom poorman group_by
#' @importFrom poorman mutate
#' @importFrom poorman select
#' @importFrom poorman ungroup
#' @importFrom tidyr spread

print_dust_console <- function(x, ..., return_df = FALSE, asis=TRUE) {
  if (!is.null(x$caption) & x$caption_number) increment_pixie_count()
  caption_number_prefix <-
    if (x$caption_number) sprintf("Table %s: ", get_pixie_count())
    else ""

  #* Determine the number of divisions
  #* It looks more complicated than it is, but the gist of it is
  #* total number of divisions: ceiling(total_rows / longtable_rows)
  #* The insane looking data frame is just to make a reference of what rows
  #*   go in what division.
  if (!is.numeric(x$longtable) & x$longtable) longtable_rows <- 25L
  else if (!is.numeric(x$longtable) & !x$longtable) longtable_rows <- as.integer(max(x$body$row))
  else longtable_rows <- as.integer(x$longtable)

  Divisions <- data.frame(div_num = rep(1:ceiling(max(x$body$row) / longtable_rows),
                                        each = longtable_rows)[1:max(x$body$row)],
                          row_num = 1:max(x$body$row))
  total_div <- max(Divisions$div_num)

  #* Format table parts
  head <- part_prep_console(x$head)
  body <- part_prep_console(x$body)
  foot <- if (!is.null(x$foot)) part_prep_console(x$foot) else NULL
  interfoot <- if (!is.null(x$interfoot)) part_prep_console(x$interfoot) else NULL

  names(body) <- names(head) <- as.character(head[1, ])

  if (!is.null(foot)) names(foot) <- names(head)
  if (!is.null(interfoot)) names(interfoot) <- names(head)

  if (return_df) DF <- NULL

  #* Run a loop to print all the divisions
  for (i in 1:total_div) {
    tbl <- poorman::bind_rows(if (nrow(head) > 1) head[-1, ] else NULL,
                            body[Divisions$row_num[Divisions$div_num == i], ],
                            if (i == total_div) foot else interfoot)
    if (return_df) DF <- rbind(DF, tbl)
    else {
      if (!is.null(x$caption)) cat(caption_number_prefix, x$caption, "\n\n",
                                   sep = "")
      print(as.data.frame(tbl))
      cat("\n\n")
    }
  }

  if (return_df) return(as.data.frame(DF))
}

#**** Helper functions

part_prep_console <- function(part) {

  #* values in the dust object are all stored as character strings.
  #* These classes need to be converted to numerics for rounding
  #* to have the appropriate effect.
  numeric_classes <- c("double", "numeric")

  #* If functions are assigned, perform the function.
  part <- perform_function(part)

  #* Perform any rounding
  logic <- part$round == "" & part$col_class %in% numeric_classes
  part$round[logic] <- getOption("digits")

  logic <- part$col_class %in% numeric_classes
  if (any(logic))
    part$value[logic] <-
    as.character(roundSafe(part$value[logic], as.numeric(part$round[logic])))

  #* Replacement
  logic <- !is.na(part[["replace"]])
  part[["value"]][logic] <- part[["replace"]][logic]

    #* Bold text.  In the console, bold text is denoted by "**".  In order
    #* to keep the all of the formatting lined up in columns, the data
    #* frame is grouped by column, and if any cell in the column has bold
    #* text, the unbolded text gets two spaces on either side to make the
    #* columns the same width.
    apply_bold <- function(b, v) {
      if (any(b)) ifelse(b, sprintf("**%s**", v), sprintf("  %s  ", v)) else v
    }

    apply_italic <- function(i, v) {
      if (any(i)) ifelse(i, sprintf("_%s_", v), sprintf(" %s_", v)) else v
    }

    apply_span <- function(s, v) {
      ifelse(s == 0, "", v)
    }

    apply_na <- function(n, v) {
        ifelse(is.na(v) & !is.na(n), n, v)
    }

    poorman::group_by(part, col_name) %>%
    poorman::mutate(
      value = if (any(bold)) ifelse(bold, sprintf("**%s**", value), sprintf("  %s  ", value)) else value
      ) %>%
    # poorman::mutate(value = apply_bold(bold, value)) %>%
    poorman::ungroup() %>%
    #* Italic. Follows the same process as bold text.
    poorman::group_by(col_name) %>%
    poorman::mutate(
      value = if (any(italic)) ifelse(italic, sprintf("_%s_", value), sprintf(" %s_", value)) else value
      ) %>%
    # poorman::mutate(value = apply_italic(italic, value)) %>%
    poorman::ungroup() %>%
    #* For merged cells not chosen for printing, set value to an empty character
    poorman::mutate(value = ifelse(rowspan == 0, "", value),
                    value = ifelse(colspan == 0, "", value)) %>%
    # poorman::mutate(value = apply_span(rowspan, value),
    #                 value = apply_span(colspan, value)) %>%
    #* Set NA (missing) values to na_string.
    poorman::mutate(value = ifelse(is.na(value) & !is.na(na_string), na_string, value)) %>%
    # poorman::mutate(value = apply_na(na_string, value)) %>%
    #* Spread to wide format for printing
    poorman::select(row, col, value) %>%
    tidyr::spread(col, value) %>%
    poorman::select(-row)
}
