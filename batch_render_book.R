library(pixiedust)
set_pixie_count(0)
bookdown::render_book("index.Rmd", 
                      output_dir = getwd())