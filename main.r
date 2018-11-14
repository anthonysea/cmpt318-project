train <- read.csv("train_data.txt")
library(lubridate)
set.seed(1)

### Training Data Preprocessing

# Make sure that Date column is actually a Date object
train$Date <- as.Date(train$Date, "%d/%m/%Y")

# Add day of week attribute
train$Day_of_week <- weekdays(as.Date(train$Date, "%d/%m/%Y"))

# Remove rows that have any null attributes
train <- train[rowSums(is.na(train)) == 0,]

# Create specific time windows
christmas <- train[month(train$Date) == 12 & day(train$Date) == 25,]

weekends <- train[train$Day_of_week %in% c("Saturday", "Sunday"),]

weekdays <- train[train$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),]

winter <- subset(train, month(train$Date) %in% c(12, 1, 2))
spring <- subset(train, month(train$Date) %in% c(3, 4, 5))
summer <- subset(train, month(train$Date) %in% c(6, 7, 8))
fall <- subset(train, month(train$Date) %in% c(9, 10, 11))



