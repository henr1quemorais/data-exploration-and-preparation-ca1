library(ggplot2)
library(dplyr)

census_raw <- read.csv("Adult Census Income.csv", stringsAsFactors = FALSE)

# checking the size of the data set and some information about it
dim(census_raw) # number of rows and columns 

str(census_raw) #type of columns
names(census_raw) #columns names
head(census_raw, 10)#first 10 rows
summary(census_raw) #min, max and mean

#checking for missing values
colSums(is.na(census_raw)) #not available cells
colSums(census_raw == "?") #or the ones written as ?

#looking for duplicated values
sum(duplicated(census_raw))
#checking income
round(prop.table(table(census_raw$income)) * 100, 1)

#Now ? becomes a missing value
census <- read.csv("Adult Census Income.csv", na.strings = "?", stringsAsFactors = FALSE)
colSums(is.na(census)) #missing values

rows_before <- nrow(census) #removing rows with missing value
census <- na.omit(census)

sum(duplicated(census)) #checking how many duplicates left
census <- census[!duplicated(census), ]
nrow(census) ##checking how many rows left

census <- census %>% select(-fnlwgt, -education) #removing not needed columns

text_cols <- sapply(census, is.character) #removing extra spaces
census[text_cols] <- lapply(census[text_cols], trimws)

#putting other countries besides the US to the group other
census$native.country <- ifelse(census$native.country == "United-States", "United-States", "Other")

for (col in names(census)) { #text into factors
  if (is.character(census[[col]])) {
    census[[col]] <- as.factor(census[[col]])
  }
}

str(census)
dim(census)