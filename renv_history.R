# install.packages("renv")
renv::init()

# for code highlighting
renv::install("downlit")
renv::install("xml2")
library(downlit)
library(xml2)

renv::snapshot()

renv::restore()
