context("dust_cell_halign")

test_that("halign must have length 1",
{
  expect_error(dust_cell_halign(col = 2, halign = c("l", "g")),
               paste0("1: 'halign' must have length 1\n",
                      "2: 'dust_cell_halign' argument 'halign' only accepts 'l', 'left', 'c', 'center', 'r', and 'right'."))
})




