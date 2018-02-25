context("Pixiedust Colors")

x <- dust(lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars))

test_that(
  "lowercase color",
  {
    expect_silent(sprinkle(x, bg = "red"))
  }
)

test_that(
  "uppercase color",
  {
    expect_silent(sprinkle(x, bg = "RED"))
  }
)

test_that(
  "mixed-case color",
  {
    expect_silent(sprinkle(x, bg = "ReD"))
  }
)

test_that(
  "rgb color",
  {
    expect_silent(sprinkle(x, bg = "rgb(255,0,0)"))
  }
)

test_that(
  "rgba color",
  {
    expect_silent(sprinkle(x, bg = "rgba(255,0,0,0)"))
  }
)

test_that(
  "rgba color decimal alpha",
  {
    expect_silent(sprinkle(x, bg = "rgba(255,0,0,.10)"))
  }
)

test_that(
  "rgba color decimal alpha preceded by 0",
  {
    expect_silent(sprinkle(x, bg = "rgba(255,0,0,0.10)"))
  }
)

test_that(
  "rgba color 1.0 alpha",
  {
    expect_silent(sprinkle(x, bg = "rgba(255,0,0,1.0)"))
  }
)

test_that(
  "hexidecimal color",
  {
    expect_silent(sprinkle(x, bg = "#FF0000"))
  }
)

test_that(
  "hexidecimal color with transparency",
  {
    expect_silent(sprinkle(x, bg = "#FF0000F0"))
  }
)

test_that(
  "transparent color",
  {
    expect_silent(sprinkle(x, bg = "transparent"))
  }
)
