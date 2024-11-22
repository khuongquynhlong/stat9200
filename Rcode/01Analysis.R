library(haven)
library(tidyverse)
library(survey)

#----- Read data
#===============================================================================
df <- read_dta("Clean data/SSA_25countries_pooled.dta")

glimpse(df)
