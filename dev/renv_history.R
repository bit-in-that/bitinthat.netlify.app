# install.packages("renv")
renv::init()

# for code highlighting
renv::install("downlit")
renv::install("xml2")
renv::install("crosstalk")
library(downlit)
library(xml2)
library(crosstalk)

renv::snapshot()

renv::restore()
