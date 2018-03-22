context("sanitize_latex.R")

test_that(
  "Sanitize basic characters.",
  {
    expect_equal(
      sanitize_latex(c("|", "%", "#", "<=", "<", ">=", ">", "_", "\\243", "&")),
      c("$|$", "\\%", "\\#", "$\\leq$", "$<$", "$\\geq$", "$>$", "\\_",
        "\\pounds", "\\&")
    )
  }
)

test_that(
  "Sanitize pb characters.",
  {
    expect_equal(
      sanitize_latex(c("[", "(", "]", ")"), pb = TRUE),
      c("$\\left[", "$\\left(", "\\right]$", "\\right)$")
    )
  }
)

test_that(
  "Sanitize greek characters.",
  {
    expect_equal(
      sanitize_latex(c("alpha", "beta", "gamma", "delta"), greek = TRUE),
      c("$\\alpha$", "$\\beta$", "$\\gamma$", "$\\delta$")
    )
  }
)

test_that(
  "Sanitize with a caret",
  {
    expect_equal(
      sanitize_latex(c("alpha^theta"), greek = TRUE),
      "$\\alpha$$^{$\\theta$}$"
    )
  }
)