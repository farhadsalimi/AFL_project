# make a clean environment
rm(list=ls(all=TRUE))

# load packages----
library(tidyverse)
library(DataExplorer)

# read main_af file
main_df <- read_rds("afl_main.rds")

# we limit the data to the ones above 2013 as we dont have any hostorical data for 2012
main_df_20132017 <-
  main_df %>%
  filter(date >= as.Date("2013/01/01"))

# let's make a report
create_report(main_df_20132017)  

# it seems that we have quite a lot of missing value, we need to impute
# good news is that the previous performance seems to correlate well with the outcome of the match!

# save the main_df_20132017
write_rds(main_df_20132017, "main_df_for_modelling.rds")
