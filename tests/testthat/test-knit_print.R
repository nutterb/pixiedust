context("knit_print.dust.R")

test_that(
  "knit_print.dust with no options",
  {
    x <- dust(head(mtcars)) %>% 
      sprinkle_print_method("html")
    expect_silent(knit_print.dust(x))
  }
)

test_that(
  "knit_print.dust with options",
  {
    x <- dust(head(mtcars)) %>% 
      sprinkle_print_method("html")
    expect_silent(knit_print.dust(x,
                                  options = list(some_option = "a",
                                                 some_option2 = "b")))
  }
)



test_that(
  "knit_print.dust_list with no options",
  {
    x <- mtcars %>% 
      split(mtcars["gear"]) %>%  
      dust() %>% 
      sprinkle_print_method("html")
    expect_silent(knit_print.dust_list(x))
  }
)

test_that(
  "knit_print.dust_list with options",
  {
    x <- mtcars %>% 
      split(mtcars["gear"]) %>%  
      dust() %>% 
      sprinkle_print_method("html")
    expect_silent(knit_print.dust_list(x,
                                  options = list(some_option = "a",
                                                 some_option2 = "b")))
  }
)