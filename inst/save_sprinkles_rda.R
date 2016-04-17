SprinkleRef <- 
  read.csv("inst/sprinkle_reference.csv",
           stringsAsFactors = FALSE,
           na = "")

save(SprinkleRef,
     file = "R/sysdata.rda")
