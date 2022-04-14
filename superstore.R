#install and load package
install.packages('tidyverse')
library(tidyverse)

#load data set 
patchfile <- '~/Work/R/Superstore/superstore.csv'
df <- read.csv(patchfile, na.strings = '..')

#review data set
View(df)
glimpse(df)

# check missing values from all data
mean(complete.cases(df)) == 1

# check missing values from each column
colSums(is.na(df))

# check the number of unique elements of each column
sapply(df, n_distinct)