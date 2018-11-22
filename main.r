train <- read.csv("train_data.txt")
library(lubridate)
library(depmixS4)
library(ggplot2)
library(zoo)
set.seed(1)

### Training Data Preprocessing

# Fill NA Values with the Mean of that attribute
naMeanFill <- function(dataset) {
  dataset$Global_active_power[is.na(dataset$Global_active_power)] <- mean(dataset$Global_active_power, na.rm=T)
  dataset$Global_reactive_power[is.na(dataset$Global_reactive_power)] <- mean(dataset$Global_reactive_power, na.rm=T)
  dataset$Voltage[is.na(dataset$Voltage)] <- mean(dataset$Voltage, na.rm=T)
  dataset$Global_intensity[is.na(dataset$Global_intensity)] <- mean(dataset$Global_intensity, na.rm=T)
  dataset$Sub_metering_1[is.na(dataset$Sub_metering_1)] <- mean(dataset$Sub_metering_1, na.rm=T)
  dataset$Sub_metering_2[is.na(dataset$Sub_metering_2)] <- mean(dataset$Sub_metering_2, na.rm=T)
  dataset$Sub_metering_3[is.na(dataset$Sub_metering_3)] <- mean(dataset$Sub_metering_3, na.rm=T)
  return(dataset)
}

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
fw.weekdays.nights <- naMeanFill(fw.weekdays.nights)
fw.weekdays.days <- fw.weekdays[hour(hms(fw.weekdays$Time)) >= 8 & hour(hms(fw.weekdays$Time)) < 17, ]
fw.weekdays.days <- naMeanFill(fw.weekdays.days)
fw.weekdays.evenings <- fw.weekdays[hour(hms(fw.weekdays$Time)) >= 17 & hour(hms(fw.weekdays$Time)) < 24, ]
fw.weekdays.evenings <- naMeanFill(fw.weekdays.evenings)

fw.weekends.nights <- fw.weekends[hour(hms(fw.weekends$Time)) >= 0 & hour(hms(fw.weekends$Time)) < 8, ]
fw.weekends.nights <- naMeanFill(fw.weekends.nights)
fw.weekends.days <- fw.weekends[hour(hms(fw.weekends$Time)) >= 8 & hour(hms(fw.weekends$Time)) < 17, ]
fw.weekends.days <- naMeanFill(fw.weekends.days)
fw.weekends.evenings <- fw.weekends[hour(hms(fw.weekends$Time)) >= 17 & hour(hms(fw.weekends$Time)) < 24, ]
fw.weekends.evenings <- naMeanFill(fw.weekends.evenings)

ss.weekdays.nights <- ss.weekdays[hour(hms(ss.weekdays$Time)) >= 0 & hour(hms(ss.weekdays$Time)) < 8, ]
ss.weekdays.nights <- naMeanFill(ss.weekdays.nights)
ss.weekdays.days <- ss.weekdays[hour(hms(ss.weekdays$Time)) >= 8 & hour(hms(ss.weekdays$Time)) < 17, ]
ss.weekdays.days <- naMeanFill(ss.weekdays.days)
ss.weekdays.evenings <- ss.weekdays[hour(hms(ss.weekdays$Time)) >= 17 & hour(hms(ss.weekdays$Time)) < 24, ]
ss.weekdays.evenings <- naMeanFill(ss.weekdays.evenings)

ss.weekends.nights <- ss.weekends[hour(hms(ss.weekends$Time)) >= 0 & hour(hms(ss.weekends$Time)) < 8, ]
ss.weekends.nights <- naMeanFill(ss.weekends.nights)
ss.weekends.days <- ss.weekends[hour(hms(ss.weekends$Time)) >= 8 & hour(hms(ss.weekends$Time)) < 17, ]
ss.weekends.days <- naMeanFill(ss.weekends.days)
ss.weekends.evenings <- ss.weekends[hour(hms(ss.weekends$Time)) >= 17 & hour(hms(ss.weekends$Time)) < 24, ]
ss.weekends.evenings <- naMeanFill(ss.weekends.evenings)
