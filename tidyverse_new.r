

# use packrat for pkg version control -------------------------------------

# initiate the packrat
# packrat::init()

packrat::status()  # up to date

# this is my packages to begin with
library(packrat)
library(dplyr)

# current info session
sessionInfo()

# other attached packages:
# [1] dplyr_0.8.0.1        packrat_0.4.9-3      RevoUtilsMath_11.0.0
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.0       crayon_1.3.4     assertthat_0.2.0 R6_2.2.2        
# [5] magrittr_1.5     pillar_1.3.1     rlang_0.3.1      rstudioapi_0.7  
# [9] RevoUtils_11.0.1 tools_3.5.1      glue_1.3.0       purrr_0.3.1     
# [13] compiler_3.5.1   pkgconfig_2.0.2  tidyselect_0.2.5 tibble_2.0.1  

# load a new package assertthat 
library(assertthat)

# check the current session info
sessionInfo()  # note that assertthat is attached

# other attached packages:
# [1] assertthat_0.2.0     dplyr_0.8.0.1        packrat_0.4.9-3     
# [4] RevoUtilsMath_11.0.0
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.0       crayon_1.3.4     R6_2.2.2         magrittr_1.5    
# [5] pillar_1.3.1     rlang_0.3.1      rstudioapi_0.7   RevoUtils_11.0.1
# [9] tools_3.5.1      glue_1.3.0       purrr_0.3.1      compiler_3.5.1  
# [13] pkgconfig_2.0.2  tidyselect_0.2.5 tibble_2.0.1 

# update the assertthat version (0.2.0 --> 0.2.1)
install.packages("assertthat")

# check package status
packrat::status()

packrat::snapshot()  # set packrat to use the current library
# packrat::restore(overwrite.dirty = T)  # ask packrat to get back to the last snapshot

# load data ---------------------------------------------------------------

data(iris)

# mutate_at/summarise_at -----------------------------------------------------

summary(iris)

test1 <- iris %>%
  group_by(Species) %>%
  summarise_at(vars(matches("Sepal|Petal")), funs(avg = mean))

test2 <- iris %>%
  group_by(Species) %>%
  summarise_at(vars(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width), 
               list(avg = mean))

test3 <- iris %>%
  group_by(Species) %>%
  summarise_at(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
               list(avg = mean))

pandoc.table(test1)


test <- iris %>%
#  filter_(~Species %in% c("setosa", "versicolor"))
#  filter_('Species %in% c("setosa", "versicolor")')
  filter_(quote(Species %in% c("setosa", "versicolor"))) %>%
  summarise_(var_mean = ~mean(Sepal.Length))


# calculate the sum of two variables --------------------------------------

# step 0
iris %>%
  select(Sepal.Length, Sepal.Width) %>%
  mutate(var_sum = Sepal.Length + Sepal.Width)

# step 1 - wrap this up into a function
output1 <- iris %>%
  select_("Sepal.Length", "Sepal.Width") %>%
  mutate_(var_sum = paste("Sepal.Length", "Sepal.Width", sep = "+"))

fct_var_sum <- function(var1, var2){
  output <- iris %>%
    select_(var1, var2) %>%
    mutate_(var_sum = paste(var1, var2, sep = "+"))
  
  return(output)
}

output2 <- fct_var_sum("Sepal.Length", "Sepal.Width")
all.equal(output1, output2)


# step 2 - function update ------------------------------------------------

fct_var_sum2 <- function(var1, var2){
  
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  output <- iris %>%
    select(!!var1, !!var2) %>%
    mutate(var_sum = !!var1 + !!var2)
  
  return(output)
}

output2 <- fct_var_sum2(Sepal.Length, Sepal.Width)

# Reference:
# packrat workflow: https://www.r-project.org/nosvn/pandoc/packrat.html
# packrat example: https://rstudio.github.io/packrat/walkthrough.html
# r-blogger on SE: https://www.r-bloggers.com/non-standard-evaluation-and-standard-evaluation-in-dplyr/