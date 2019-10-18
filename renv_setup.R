if (!requireNamespace("remotes")) install.packages("remotes")

remotes::install_github("rstudio/renv")

options(repos = c(CRAN = "http://cran.rstudio.com"))
renv::init()

renv::snapshot()
