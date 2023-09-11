library(stringr)
source("parameters.R")
readLines("parameters.R") |> 
  str_replace(
    paste0("(?<=round_number \\= )", parameters$round_number), as.character(parameters$round_number + 1)
  ) |> 
  writeLines("parameters.R")
