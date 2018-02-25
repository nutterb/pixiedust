context("tidy_labels_levels")


mtcars2 <- mtcars
mtcars2 <- 
  labelVector::set_label(
    mtcars2,
    mpg = "Gas Mileage",
    qsec = "Quarter Mile Time",
    am = "Transmission",
    wt = "Weight",
    gear = "Gears"
  )

#* Basic Output for a model with no interactions
#* Note: numeric_level has no impact as there are no
#*       interactions involving numeric variables.

fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars2)

test_that("output for a model with no interactions",
{
  expect_silent(tidy_levels_labels(fit, 
                                 descriptors = c("term", "term_plain", "label", 
                                                 "level", "level_detail"),
                                 numeric_level = "term"))
})
 
#* Assign factors ahead of the model. This allows 
#* the user to determine the levels that display.
#* Compare the output for 'am' with the output for 'gear'

mtcars2$am <- factor(mtcars2$am, 0:1, c("Automatic", "Manual"))
mtcars2$am <- labelVector::set_label(mtcars2$am, "Transmission") # Label was lost in variable conversion
fit <- lm(mpg ~ qsec + am + wt + factor(gear), data = mtcars2)

test_that("factor labels from data are preserved",
{
  expect_equal(tidy_levels_labels(fit, 
                                 descriptors = c("term", "term_plain", "label", 
                                                 "level", "level_detail"),
                                 numeric_level = "term")$level[3],
               "Manual")
})
 
 
#* Include an interaction between a factor and numeric.

fit <- lm(mpg ~ qsec + am * wt + factor(gear), data = mtcars2)


test_that("use an interaction",
{
  expect_silent(tidy_levels_labels(fit, 
                                 descriptors = c("term", "term_plain", "label", 
                                                 "level", "level_detail"),
                                 numeric_level = "term"))
})
 
#* Now observe how 'level' and 'level_detail' change 
#* in the interaction terms as we choose different 
#* values for 'numeric_level'

test_that("use an interaction with term_plain",
{
  expect_silent(tidy_levels_labels(fit, 
                                 descriptors = c("term", "term_plain", "label", 
                                                 "level", "level_detail"),
                                 numeric_level = "term_plain"))
})
   
test_that("use an interaction with label",
{
  expect_silent(tidy_levels_labels(fit, 
                                 descriptors = c("term", "term_plain", "label", 
                                                 "level", "level_detail"),
                                 numeric_level = "label"))
})

test_that("tidy_labels_levels with bad descriptor list",
{
  expect_error(tidy_levels_labels(fit,
                                 descriptors = c("term_boring"),
                                 numeric_level = "term"))
})

test_that("tidy_labels_levels with bad numeric_level",
{
  expect_error(tidy_levels_labels(fit,
                                 descriptors = c("term"),
                                 numeric_level = "term_boring"))
})