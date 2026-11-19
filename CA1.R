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

colSums(is.na(census_raw))
colSums(census_raw == "?")
apply(census_raw == "?", 1, any)
sum(duplicated(census_raw))
table(census_raw$income)
round(prop.table(table(census_raw$income)) * 100, 1)

census <- read.csv("Adult Census Income.csv", na.strings = "?", stringsAsFactors = FALSE)
colSums(is.na(census))