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

census <- na.omit(census) #removing rows with missing value

sum(duplicated(census)) #checking how many duplicates left
census <- census[!duplicated(census), ]
nrow(census) #checking how many rows left

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

#Outliers

numeric_cols <- c("age", "education.num", "capital.gain","capital.loss", "hours.per.week")
count_outliers <- function(x) { #counting outliers
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  iqr <- q3 - q1
  sum(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr)
}

outlier_table <- data.frame( #creating a table that shows the number and percentage of outliers
  variable = numeric_cols,
  outliers = sapply(census[numeric_cols], count_outliers)
)
outlier_table$percent <- round(outlier_table$outliers / nrow(census) * 100, 1)
outlier_table

long <- stack(census[c("age", "education.num", "hours.per.week")]) #stacking these columns

#boxplot 
#red dots = outliers
ggplot(long, aes(x = ind, y = values)) + geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.alpha = 0.3) + labs(title = "Age, education and hours per week",
x = NULL, y = "Value") + theme_minimal(base_size = 14)

#checking gain and loss in the capital
sum(census$capital.gain == 0) / nrow(census) * 100
sum(census$capital.loss == 0) / nrow(census) * 100
sum(census$capital.gain == 99999)