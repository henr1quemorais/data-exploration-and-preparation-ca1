library(ggplot2)
library(dplyr)

census_raw <- read.csv("Adult Census Income.csv", stringsAsFactors = FALSE)

dim(census_raw)
nrow(census_raw)
ncol(census_raw)

str(census_raw)
names(census_raw)
head(census_raw, 10)
summary(census_raw)