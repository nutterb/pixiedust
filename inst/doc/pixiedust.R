## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), 
          data = mtcars)

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
broom::tidy(fit)

## ------------------------------------------------------------------------
library(pixiedust)
dust(fit)

## ---- echo=FALSE---------------------------------------------------------
dust(fit) %>%
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", "P-value") %>% 
  sprinkle(bg_pattern = c("orchid", "plum")) %>% 
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"), round = 2)

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) 

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames(term = "Term", p.value = "P-value")

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames(term = "Term", p.value = "P-value", 
                    std.error = "SE", statistic = "T-statistic",
                    estimate = "Coefficient")

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", "P-value")

## ---- error = TRUE-------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", "P-value", "Extra Column Name")

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(cols = "term", 
           replace = c("Intercept", "Quarter Mile Time", "Automatic vs. Manual",
                       "Weight", "Gears: 4 vs. 3", "Gears: 5 vs 3")) %>%
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", "P-value")

## ------------------------------------------------------------------------
dust(fit) %>% 
  sprinkle(rows = 2:3, cols = 3:4, 
           replace = c(100, 300, 200, 400),
           italic = TRUE) %>%
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames("Term", "Coefficient", "SE", "T-statistic", "P-value")

## ------------------------------------------------------------------------
basetable <- dust(fit) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames(term = "Term", estimate = "Coefficient", 
                    std.error = "SE", statistic = "T-statistic", 
                    p.value = "P-value") %>% 
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), border_color = "orchid")

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), cols = 1, 
           border = c("left", "top", "bottom"),
           border_color = "orchid") %>% 
  sprinkle(rows = c(2, 4), cols = 5,
           border = c("right", "top", "bottom"),
           border_color = "orchid") %>%
  sprinkle(rows = c(2, 4), cols = 2:4,
           border = c("top", "bottom"),
           border_color = "orchid")

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), border_color = "orchid",
           pad = 15)

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), bold = TRUE)

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), bold = TRUE, italic=TRUE)

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), bg = "orchid")

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), bg = "rgba(218,112,214,.5)")

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(bg_pattern = c("orchid", "plum"))

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = c(2, 4), 
           font_color = "orchid",
           font_size = 24,
           font_size_units = "pt")

## ------------------------------------------------------------------------
basetable %>% 
  sprinkle(rows = 1, cols = 2, halign = "left", valign = "top", height = 50, width = 50) %>% 
  sprinkle(rows = 1, cols = 3, halign = "center", valign = "top", height = 50, width = 50) %>% 
  sprinkle(rows = 1, cols = 4, halign = "right", valign = "top", height = 50, width = 50) %>% 
  sprinkle(rows = 2, cols = 2, halign = "left", valign = "middle", height = 50, width = 50) %>% 
  sprinkle(rows = 2, cols = 3, halign = "center", valign = "middle", height = 50, width = 50) %>% 
  sprinkle(rows = 2, cols = 4, halign = "right", valign = "middle", height = 50, width = 50) %>% 
  sprinkle(rows = 3, cols = 2, halign = "left", valign = "bottom", height = 50, width = 50) %>% 
  sprinkle(rows = 3, cols = 3, halign = "center", valign = "bottom", height = 50, width = 50) %>% 
  sprinkle(rows = 3, cols = 4, halign = "right", valign = "bottom", height = 50, width = 50)

## ------------------------------------------------------------------------
dust(mtcars, tidy_df = TRUE) %>% 
  sprinkle(cols = c("mean", "sd", "median", "trimmed", "mad", 
                    "min", "max", "range", "skew", "kurtosis", "se"),
           round = 2) %>% 
  sprinkle(rows = 1, rotate_degree = -90, 
           height = 60, part = "head") %>% 
  sprinkle_print_method("html")

