## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
library(dplyr)

## ---- echo=FALSE---------------------------------------------------------
Sprinkles <- read.csv(system.file("sprinkles.csv", package = "pixiedust"),
                        stringsAsFactors=FALSE)
Sprinkles[,-1] <- lapply(Sprinkles[-1], trimws)

## ---- echo=FALSE---------------------------------------------------------
library(pixiedust)
red <- "#A50026"
lightgreen <- "#A6DBA0"
green <- "#006837"
yellow <- "#FFFFBF"

row_bg <- which(1:nrow(Sprinkles) %% 2 == 0)

dust(Sprinkles[, -2],
     caption = "Sprinkles and their output formats") %>%
  sprinkle(cols = 2:5,
           fn = quote(ifelse(value == "x", 	"&#10003;", ""))) %>%
  sprinkle(bg_pattern_by = "rows",
           border = "all",
           border_color = "#DCDCDC") %>%
  sprinkle(rows = row_bg,
           border_color = "#FFFFFF") %>%
  sprinkle_table(pad = 3) 


## ---- echo = FALSE-------------------------------------------------------
SprinkleDocs <- 
  read.csv(system.file("sprinkle_documentation.csv", package = "pixiedust"),
                        stringsAsFactors=FALSE,
           na = "")

SprinkleDocs[["sprinkle_name"]] <- SprinkleDocs[["sprinkle"]]

for(i in seq_along(SprinkleDocs[["sprinkle"]]))
{
  if (is.na(SprinkleDocs[["sprinkle_name"]][i])) 
    SprinkleDocs[["sprinkle_name"]][i] <- SprinkleDocs[["sprinkle_name"]][i-1]
}

SprinkleDocs <- 
  mutate(SprinkleDocs,
         bg_index = as.numeric(factor(sprinkle_name,
                                         unique(sprinkle_name))) %% 2)

rows_bg <- which(SprinkleDocs$bg_index == 0)

rows_top_border <- which(!is.na(SprinkleDocs$sprinkle))

dust(SprinkleDocs[, 1:3],
     caption = "Sprinkle Usage") %>%
  sprinkle(border = "all",
           border_color = "#DCDCDC",
           na_string = "") %>%
  sprinkle(rows = rows_bg,
           bg = "#DDDDDD",
           border = "all",
           border_color = "#FFFFFF") %>%
  sprinkle(rows = rows_top_border,
           border = "top", 
           border_color = "black",
           border_thickness = 2) 

