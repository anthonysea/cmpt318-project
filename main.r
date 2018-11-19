train <- read.csv("train_data.txt")
library(lubridate)
library(depmixS4)
library(ggplot2)
library(zoo)
set.seed(1)

### Training Data Preprocessing

# Make sure that Date column is actually a Date object
train$Date <- as.Date(train$Date, "%d/%m/%Y")

# Add day of week attribute
train$Day_of_week <- weekdays(as.Date(train$Date, "%d/%m/%Y"))

# Remove rows that have any null attributes
# train <- train[rowSums(is.na(train)) == 0,]

fw <- subset(train, month(train$Date) %in% c(9, 10, 11, 12, 1, 2))
ss <- subset(train, month(train$Date) %in% c(3, 4, 5, 6, 7, 8))

fw.weekdays <- subset(fw, fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
fw.weekends <- subset(fw, fw$Day_of_week %in% c("Saturday", "Sunday"))

ss.weekdays <- subset(ss, ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ss.weekends <- subset(ss, ss$Day_of_week %in% c("Saturday", "Sunday"))

fw.weekdays.nights <- fw.weekdays[hour(hms(fw.weekdays$Time)) >= 0 & hour(hms(fw.weekdays$Time)) < 8, ]
fw.weekdays.days <- fw.weekdays[hour(hms(fw.weekdays$Time)) >= 8 & hour(hms(fw.weekdays$Time)) < 17, ]
fw.weekdays.evenings <- fw.weekdays[hour(hms(fw.weekdays$Time)) >= 17 & hour(hms(fw.weekdays$Time)) < 24, ]

fw.weekends.nights <- fw.weekends[hour(hms(fw.weekends$Time)) >= 0 & hour(hms(fw.weekends$Time)) < 8, ]
fw.weekends.days <- fw.weekends[hour(hms(fw.weekends$Time)) >= 8 & hour(hms(fw.weekends$Time)) < 17, ]
fw.weekends.evenings <- fw.weekends[hour(hms(fw.weekends$Time)) >= 17 & hour(hms(fw.weekends$Time)) < 24, ]

ss.weekdays.nights <- ss.weekdays[hour(hms(ss.weekdays$Time)) >= 0 & hour(hms(ss.weekdays$Time)) < 8, ]
ss.weekdays.days <- ss.weekdays[hour(hms(ss.weekdays$Time)) >= 8 & hour(hms(ss.weekdays$Time)) < 17, ]
ss.weekdays.evenings <- ss.weekdays[hour(hms(ss.weekdays$Time)) >= 17 & hour(hms(ss.weekdays$Time)) < 24, ]

ss.weekends.nights <- ss.weekends[hour(hms(ss.weekends$Time)) >= 0 & hour(hms(ss.weekends$Time)) < 8, ]
ss.weekends.days <- ss.weekends[hour(hms(ss.weekends$Time)) >= 8 & hour(hms(ss.weekends$Time)) < 17, ]
ss.weekends.evenings <- ss.weekends[hour(hms(ss.weekends$Time)) >= 17 & hour(hms(ss.weekends$Time)) < 24, ]
