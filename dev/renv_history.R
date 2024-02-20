# install.packages("renv")
renv::init()

# for code highlighting
renv::install("downlit")
renv::install("xml2")
renv::install("crosstalk")
renv::install("tzdb")
library(downlit)
library(xml2)
library(crosstalk)
library(tzdb)

renv::snapshot()

renv::restore()
