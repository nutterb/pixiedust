context("sprinkle_gradient")

x <- dust(mtcars)

color_range <- 
  scales::gradient_n_pal(c("#132B43", "#56B1F7"))(seq(0, 1, 
                                                      length.out = 10))
color_index <- 
  as.numeric(cut(mtcars$mpg,
                 breaks = quantile(mtcars$mpg, 
                                   probs = seq(0, 1, length.out = 10),
                                   na.rm = TRUE),
                 include.lowest = TRUE))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the bg column",
  {
    expect_equal(
      sprinkle_gradient(
        x, 
        cols = "mpg", 
        gradient = "bg")[["body"]][["bg"]][1:32],
      
      color_range[color_index]
    )
  }
)

# Functional Requirement 2 ------------------------------------------

# Functional Requirement 3 ------------------------------------------

# Functional Requirement 4 ------------------------------------------

# Functional Requirement 5 ------------------------------------------

# Functional Requirement 6 ------------------------------------------

# Functional Requirement 7 ------------------------------------------

# Functional Requirement 8 ------------------------------------------

# Functional Requirement 9 ------------------------------------------

# Functional Requirement 10 -----------------------------------------

# Functional Requirement 11 -----------------------------------------

# Functional Requirement 12 -----------------------------------------