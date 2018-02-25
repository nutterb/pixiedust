context("is_valid_color")

# Functional Requirement --------------------------------------------

test_that(
  "Returns a logical vector correclty identifying valid color formats",
  {
    skip_on_cran()
    col <- expand.grid(R = seq(0, 255, by = 50),
                       G = seq(0, 255, by = 50),
                       B = seq(0, 255, by = 50))
    col <- sprintf("rgb(%s, %s, %s)",
                   col$R,
                   col$G, 
                   col$B)
    expect_equal(
      is_valid_color(c(col, "rgb(255, 255, 255)")),
      rep(TRUE, length(col) + 1)
    )
  }
)

test_that(
  "Returns a logical vector correctly identifying valid color formats",
  {
    skip_on_cran()
    expect_equal(
      is_valid_color(c("rgb(-1, 000, 0)", "rgb(5, 15, 256)", "rgb(5, 322, 100)")),
                     c(FALSE, FALSE, FALSE)
    )
  }
)

test_that(
  "Returns a logical vector correclty identifying valid color formats",
  {
    skip_on_cran()
    col <- expand.grid(R = seq(0, 255, by = 50),
                       G = seq(0, 255, by = 50),
                       B = seq(0, 255, by = 50),
                       A = seq(0, 1, by = .2))
    col <- sprintf("rgba(%s, %s, %s, %s)",
                   col$R,
                   col$G, 
                   col$B,
                   col$A)
    expect_equal(
      is_valid_color(c(col, "rgba(255, 255, 255, 0.5)")),
      rep(TRUE, length(col) + 1)
    )
  }
)

test_that(
  "Returns a logical vector correctly identifying valid color formats",
  {
    skip_on_cran()
    expect_equal(
      is_valid_color(c("rgba(-1, 000, 0, 0.5)", "rgb(5, 15, 256, 0.5)", 
                       "rgb(5, 322, 100, 0.5)",
                       "rgba(000, 000, 255, 1.1)", "rgba(256, 100, 15, -0.2)",
                       "rgba(123, 123, 123, 1.00000001)")),
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
  }
)

test_that(
  "Returns a logical vector correclty identifying valid color formats",
  {
    skip_on_cran()
    col <- expand.grid(R = sample(c(0:9, LETTERS[1:6]), 4),
                       R2 = sample(c(0:9, LETTERS[1:6]), 4),
                       G = sample(c(0:9, LETTERS[1:6]), 4),
                       G2 = sample(c(0:9, LETTERS[1:6]), 4),
                       B = sample(c(0:9, LETTERS[1:6]), 4),
                       B2 = sample(c(0:9, LETTERS[1:6]), 4))
    col <- sprintf("#%s%s%s%s%s%s",
                   col$R, col$R2,
                   col$G, col$G2,
                   col$B, col$B2)
    expect_equal(
      is_valid_color(c(col, "#000000", "#FFFFFF")),
      rep(TRUE, length(col) + 2)
    )
  }
)

test_that(
  "Returns a logical vector correctly identifying valid color formats",
  {
    skip_on_cran()
    expect_equal(
      is_valid_color(c("#00000G", "#AAAAZA", "#BBBQBB", "#CCLCCC",
                       "#DSDDDD", "#NEEEEE")),
      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
  }
)
    
test_that(
  "Returns a logical vector correclty identifying valid color formats",
  {
    skip_on_cran()
    col <- expand.grid(R = sample(c(0:9, LETTERS[1:6]), 2),
                       R2 = sample(c(0:9, LETTERS[1:6]), 2),
                       G = sample(c(0:9, LETTERS[1:6]), 2),
                       G2 = sample(c(0:9, LETTERS[1:6]), 2),
                       B = sample(c(0:9, LETTERS[1:6]), 2),
                       B2 = sample(c(0:9, LETTERS[1:6]), 2),
                       A = sample(c(0:9, LETTERS[1:6]), 4),
                       A2 = sample(c(0:9, LETTERS[1:6], 4)))
    col <- sprintf("#%s%s%s%s%s%s%s%s",
                   col$R, col$R2,
                   col$G, col$G2,
                   col$B, col$B2,
                   col$A, col$A2)
    expect_equal(
      is_valid_color(c(col, "#00000000", "#FFFFFFFF")),
      rep(TRUE, length(col) + 2)
    )
  }
)

test_that(
  "Returns a logical vector correctly identifying valid color formats",
  {
    skip_on_cran()
    expect_equal(
      is_valid_color(c("#000000AG", "#AAAAAAGA")),
      c(FALSE, FALSE)
    )
  }
)

test_that(
  "Returns a logical vector correctly identifying valid color formats",
  {
    skip_on_cran()
    expect_equal(
      is_valid_color(c(colors(), "transparent")),
      rep(TRUE, length(colors()) + 1)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if color is not a character object",
  {
    skip_on_cran()
    expect_error(is_valid_color(1:3))
  }
)