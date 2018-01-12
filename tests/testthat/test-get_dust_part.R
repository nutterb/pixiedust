context("get_dust_part")

foot <- 
  colMeans(mtcars) %>% 
  matrix(nrow = 1) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  setNames(names(mtcars))

x <- 
  dust(mtcars) %>% 
  redust(foot, part = "foot") %>% 
  redust(foot, part = "interfoot")
  

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return, as a data frame, the table head",
  expect_equal(
    get_dust_part(x, "head"),
    names(mtcars) %>% 
      matrix(nrow = 1) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>% 
      setNames(names(mtcars))
  )
)

test_that(
  "Return, as a data frame, the table foot",
  expect_equal(
    get_dust_part(x, "foot") %>% 
      lapply(as.numeric) %>% 
      as.data.frame(),
    foot
  )
)

test_that(
  "Return, as a data frame, the table interfoot",
  expect_equal(
    get_dust_part(x, "interfoot") %>% 
      lapply(as.numeric) %>% 
      as.data.frame(),
    foot
  )
)

test_that(
  "Return, as a data frame, the table body",
  expect_equal(
    {
      a <- get_dust_part(x, "body") %>% 
        lapply(as.numeric) %>% 
        as.data.frame()
      rownames(a) <- rownames(mtcars)
      a
    },
    mtcars
  )
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      get_dust_part(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if part is not one of head, foot, interfoot, or body",
  {
    expect_error(
      get_dust_part(x, "not a part")
    )
  }
)

# Additional Tests --------------------------------------------------

test_that(
  "Return a 0 row data frame if the part is NULL",
  {
    x <- dust(head(mtcars))
    expect_equal(
      get_dust_part(x, "interfoot"),
      structure(list(mpg = logical(0), cyl = logical(0), disp = logical(0), 
                     hp = logical(0), drat = logical(0), wt = logical(0), 
                     qsec = logical(0), vs = logical(0), am = logical(0), 
                     gear = logical(0), carb = logical(0)), 
                .Names = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", 
                           "vs", "am", "gear", "carb"), 
                row.names = integer(0), 
                class = "data.frame")
    )
  }
)
