## ------------------------------------------------------------------------
library(dplyr)
library(pixiedust)

mtcars2 <- mtcars[1:10, ]

Hmisc::label(mtcars2$mpg) <- "Gas Mileage"
Hmisc::label(mtcars2$cyl) <- "Cylinders"
Hmisc::label(mtcars2$disp) <- "Displacement"
Hmisc::label(mtcars2$hp) <- "Horse Power"
Hmisc::label(mtcars2$drat) <- "Rear Axle Ratio"
Hmisc::label(mtcars2$wt) <- "Weight"
Hmisc::label(mtcars2$qsec) <- "1/4 mile time"
Hmisc::label(mtcars2$vs) <- "V/S"
Hmisc::label(mtcars2$am) <- "Transmission"
Hmisc::label(mtcars2$gear) <- "Forward Gears"
Hmisc::label(mtcars2$carb) <- "Carburetors"

## ---- echo=FALSE---------------------------------------------------------
mtcars <- mutate(mtcars,
                 am = factor(am, 0:1, c("Automatic", "Manual")),
                 cyl = factor(cyl),
                 gear = factor(gear))

Hmisc::label(mtcars$mpg) <- "Gas Mileage"
Hmisc::label(mtcars$cyl) <- "Cylinders"
Hmisc::label(mtcars$disp) <- "Displacement"
Hmisc::label(mtcars$hp) <- "Horse Power"
Hmisc::label(mtcars$drat) <- "Rear Axle Ratio"
Hmisc::label(mtcars$wt) <- "Weight"
Hmisc::label(mtcars$qsec) <- "1/4 mile time"
Hmisc::label(mtcars$vs) <- "V/S"
Hmisc::label(mtcars$am) <- "Transmission"
Hmisc::label(mtcars$gear) <- "Forward Gears"
Hmisc::label(mtcars$carb) <- "Carburetors"

fit <- lm(mpg ~ am + wt + qsec + gear, data = mtcars)

## ------------------------------------------------------------------------
custom_head <- rbind(names(mtcars2), Hmisc::label(mtcars2)) %>%
  as.data.frame(stringsAsFactors = FALSE)

custom_foot <- rbind(vapply(mtcars2, mean, numeric(1)),
                     vapply(mtcars2, sd, numeric(1))) %>%
  as.data.frame(stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
dust(mtcars2) %>%
  redust(custom_head, part = "head") %>%
  redust(custom_foot, part = "foot") %>%
  sprinkle_table(round = 2) %>%
  sprinkle(bg = "gray", part = "head") %>%
  sprinkle(bg = "lightgray", part = "foot") %>%
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
custom_interfoot <- data.frame("To Be Continued", 
                               "", "", "", "", "", "",
                               "", "", "", "")

(x <- dust(mtcars2) %>%
  redust(custom_head, part = "head") %>%
  redust(custom_foot, part = "foot") %>%
  redust(custom_interfoot, part = "interfoot") %>%
  sprinkle_table(round = 2, longtable = 4) %>%
  sprinkle(bg = "gray", part = "head") %>%
  sprinkle(bg = "lightgray", part = "foot") %>%
  sprinkle(bg = "lightgray", part = "interfoot") %>%
  sprinkle_print_method("html"))

## ------------------------------------------------------------------------
x %>%
  sprinkle(merge = TRUE, halign = "center", part = "interfoot")

## ------------------------------------------------------------------------
x %>%
  sprinkle(merge = TRUE, halign = "center", part = "interfoot") %>%
  sprinkle(rows = 1:3, cols = 2:4,
           merge = TRUE, merge_rowval = 2, merge_colval = 3,
           halign = "center")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), 
          data = mtcars)

dust(fit, descriptors = c("label", "level")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
dust(fit, descriptors = c("label", "level_detail")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear + factor(vs), 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear, 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail"),
     glance_foot = TRUE) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle(rows = 1, border = "top") %>%
  sprinkle(cols = c(2, 6), round = 2, na_string = "",
           part = "foot") %>%
  sprinkle(rows = 1, border = "top", part = "foot") %>%
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear, 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail"),
     glance_foot = TRUE,
     glance_stats = c("AIC", "adj.r.squared", "BIC", "df"),
     byrow = TRUE) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle(rows = 1, border = "top") %>%
  sprinkle(cols = c(2, 6), round = 2, na_string = "",
           part = "foot") %>%
  sprinkle(rows = 1, border = "top", part = "foot") %>%
  sprinkle_print_method("html")

## ------------------------------------------------------------------------
custom_head <- rbind(names(mtcars2), Hmisc::label(mtcars2)) %>%
  as.data.frame(stringsAsFactors = FALSE)

custom_foot <- rbind(vapply(mtcars2, mean, numeric(1)),
                     vapply(mtcars2, sd, numeric(1))) %>%
  as.data.frame(stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
dust(mtcars2) %>%
  redust(custom_head, part = "head") %>%
  redust(custom_foot, part = "foot") %>%
  sprinkle_table(round = 2) %>%
  sprinkle(bg = "gray", part = "head") %>%
  sprinkle(bg = "lightgray", part = "foot") %>%
  sprinkle_print_method("console")

## ------------------------------------------------------------------------
custom_interfoot <- data.frame("To Be Continued", 
                               "", "", "", "", "", "",
                               "", "", "", "")

(x <- dust(mtcars2) %>%
  redust(custom_head, part = "head") %>%
  redust(custom_foot, part = "foot") %>%
  redust(custom_interfoot, part = "interfoot") %>%
  sprinkle_table(round = 2, longtable = 4) %>%
  sprinkle(bg = "gray", part = "head") %>%
  sprinkle(bg = "lightgray", part = "foot") %>%
  sprinkle(bg = "lightgray", part = "interfoot") %>%
  sprinkle_print_method("console"))

## ------------------------------------------------------------------------
x %>%
  sprinkle(merge = TRUE, halign = "center", part = "interfoot")

## ------------------------------------------------------------------------
x %>%
  sprinkle(merge = TRUE, halign = "center", part = "interfoot") %>%
  sprinkle(rows = 1:3, cols = 2:4,
           merge = TRUE, merge_rowval = 2, merge_colval = 3,
           halign = "center")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), 
          data = mtcars)

dust(fit, descriptors = c("label", "level")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("console")

## ------------------------------------------------------------------------
dust(fit, descriptors = c("label", "level_detail")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("console")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear + factor(vs), 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("console")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear, 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail"),
     glance_foot = TRUE) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle(rows = 1, border = "top") %>%
  sprinkle(cols = c(2, 6), round = 2, na_string = "",
           part = "foot") %>%
  sprinkle(rows = 1, border = "top", part = "foot") %>%
  sprinkle_print_method("console")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear, 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail"),
     glance_foot = TRUE,
     glance_stats = c("AIC", "adj.r.squared", "BIC", "df"),
     byrow = TRUE) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle(rows = 1, border = "top") %>%
  sprinkle(cols = c(2, 6), round = 2, na_string = "",
           part = "foot") %>%
  sprinkle(rows = 1, border = "top", part = "foot") %>%
  sprinkle_print_method("console")

## ------------------------------------------------------------------------
custom_head <- rbind(names(mtcars2), Hmisc::label(mtcars2)) %>%
  as.data.frame(stringsAsFactors = FALSE)

custom_foot <- rbind(vapply(mtcars2, mean, numeric(1)),
                     vapply(mtcars2, sd, numeric(1))) %>%
  as.data.frame(stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
dust(mtcars2) %>%
  redust(custom_head, part = "head") %>%
  redust(custom_foot, part = "foot") %>%
  sprinkle_table(round = 2) %>%
  sprinkle(bg = "gray", part = "head") %>%
  sprinkle(bg = "lightgray", part = "foot") %>%
  sprinkle_print_method("markdown")

## ------------------------------------------------------------------------
custom_interfoot <- data.frame("To Be Continued", 
                               "", "", "", "", "", "",
                               "", "", "", "")

(x <- dust(mtcars2) %>%
  redust(custom_head, part = "head") %>%
  redust(custom_foot, part = "foot") %>%
  redust(custom_interfoot, part = "interfoot") %>%
  sprinkle_table(round = 2, longtable = 4) %>%
  sprinkle(bg = "gray", part = "head") %>%
  sprinkle(bg = "lightgray", part = "foot") %>%
  sprinkle(bg = "lightgray", part = "interfoot") %>%
  sprinkle_print_method("markdown"))

## ------------------------------------------------------------------------
x %>%
  sprinkle(merge = TRUE, halign = "center", part = "interfoot")

## ------------------------------------------------------------------------
x %>%
  sprinkle(merge = TRUE, halign = "center", part = "interfoot") %>%
  sprinkle(rows = 1:3, cols = 2:4,
           merge = TRUE, merge_rowval = 2, merge_colval = 3,
           halign = "center")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), 
          data = mtcars)

dust(fit, descriptors = c("label", "level")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("markdown")

## ------------------------------------------------------------------------
dust(fit, descriptors = c("label", "level_detail")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("markdown")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear + factor(vs), 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail")) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("markdown")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear, 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail"),
     glance_foot = TRUE) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle(rows = 1, border = "top") %>%
  sprinkle(cols = c(2, 6), round = 2, na_string = "",
           part = "foot") %>%
  sprinkle(rows = 1, border = "top", part = "foot") %>%
  sprinkle_print_method("markdown")

## ------------------------------------------------------------------------
fit <- lm(mpg ~ qsec + am + wt + gear, 
          data = mtcars)

dust(fit, descriptors = c("label", "level_detail"),
     glance_foot = TRUE,
     glance_stats = c("AIC", "adj.r.squared", "BIC", "df"),
     byrow = TRUE) %>%
  sprinkle(cols = 3:5, round = 2) %>%
  sprinkle(cols = 6, fn = quote(pvalString(value))) %>%
  sprinkle(rows = 1, border = "top") %>%
  sprinkle(cols = c(2, 6), round = 2, na_string = "",
           part = "foot") %>%
  sprinkle(rows = 1, border = "top", part = "foot") %>%
  sprinkle_print_method("markdown")

