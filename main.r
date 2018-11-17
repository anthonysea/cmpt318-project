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
train <- train[rowSums(is.na(train)) == 0,]

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

'
winter <- subset(train, month(train$Date) %in% c(12, 1, 2))
spring <- subset(train, month(train$Date) %in% c(3, 4, 5))
summer <- subset(train, month(train$Date) %in% c(6, 7, 8))
fall <- subset(train, month(train$Date) %in% c(9, 10, 11))

# Create time windows
## Summer time windows
summer.weekdays <- subset(summer, summer$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
summer.weekends <- subset(summer, summer$Day_of_week %in% c("Saturday", "Sunday"))

summer.weekdays.evenings <- summer.weekdays[hour(hms(summer.weekdays$Time)) >= 18 & hour(hms(summer.weekdays$Time)) < 24,]
summer.weekdays.nights <- summer.weekdays[hour(hms(summer.weekdays$Time)) >= 0 & hour(hms(summer.weekdays$Time)) < 6,]
summer.weekdays.mornings <- summer.weekdays[hour(hms(summer.weekdays$Time)) >= 6 & hour(hms(summer.weekdays$Time)) < 12,]
summer.weekdays.afternoons <- summer.weekdays[hour(hms(summer.weekdays$Time)) >= 12 & hour(hms(summer.weekdays$Time)) < 18,]

summer.weekends.evenings <- summer.weekends[hour(hms(summer.weekends$Time)) >= 18 & hour(hms(summer.weekends$Time)) < 24,]
summer.weekends.nights <- summer.weekends[hour(hms(summer.weekends$Time)) >= 0 & hour(hms(summer.weekends$Time)) < 6,]
summer.weekends.mornings <- summer.weekends[hour(hms(summer.weekends$Time)) >= 6 & hour(hms(summer.weekends$Time)) < 12,]
summer.weekends.afternoons <- summer.weekends[hour(hms(summer.weekends$Time)) >= 12 & hour(hms(summer.weekends$Time)) < 18,]


## Fall time windows
fall.weekdays <- subset(fall, fall$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
fall.weekends <- subset(fall, fall$Day_of_week %in% c("Saturday", "Sunday"))

fall.weekdays.evenings <- fall.weekdays[hour(hms(fall.weekdays$Time)) >= 18 & hour(hms(fall.weekdays$Time)) < 24,]
fall.weekdays.nights <- fall.weekdays[hour(hms(fall.weekdays$Time)) >= 0 & hour(hms(fall.weekdays$Time)) < 6,]
fall.weekdays.mornings <- fall.weekdays[hour(hms(fall.weekdays$Time)) >= 6 & hour(hms(fall.weekdays$Time)) < 12,]
fall.weekdays.afternoons <- fall.weekdays[hour(hms(fall.weekdays$Time)) >= 12 & hour(hms(fall.weekdays$Time)) < 18,]

fall.weekends.evenings <- fall.weekends[hour(hms(fall.weekends$Time)) >= 18 & hour(hms(fall.weekends$Time)) < 24,]
fall.weekends.nights <- fall.weekends[hour(hms(fall.weekends$Time)) >= 0 & hour(hms(fall.weekends$Time)) < 6,]
fall.weekends.mornings <- fall.weekends[hour(hms(fall.weekends$Time)) >= 6 & hour(hms(fall.weekends$Time)) < 12,]
fall.weekends.afternoons <- fall.weekends[hour(hms(fall.weekends$Time)) >= 12 & hour(hms(fall.weekends$Time)) < 18,]

## Winter time windows
winter.weekdays <- subset(winter, winter$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
winter.weekends <- subset(winter, winter$Day_of_week %in% c("Saturday", "Sunday"))

winter.weekdays.evenings <- winter.weekdays[hour(hms(winter.weekdays$Time)) >= 18 & hour(hms(winter.weekdays$Time)) < 24,]
winter.weekdays.nights <- winter.weekdays[hour(hms(winter.weekdays$Time)) >= 0 & hour(hms(winter.weekdays$Time)) < 6,]
winter.weekdays.mornings <- winter.weekdays[hour(hms(winter.weekdays$Time)) >= 6 & hour(hms(winter.weekdays$Time)) < 12,]
winter.weekdays.afternoons <- winter.weekdays[hour(hms(winter.weekdays$Time)) >= 12 & hour(hms(winter.weekdays$Time)) < 18,]

winter.weekends.evenings <- winter.weekends[hour(hms(winter.weekends$Time)) >= 18 & hour(hms(winter.weekends$Time)) < 24,]
winter.weekends.nights <- winter.weekends[hour(hms(winter.weekends$Time)) >= 0 & hour(hms(winter.weekends$Time)) < 6,]
winter.weekends.mornings <- winter.weekends[hour(hms(winter.weekends$Time)) >= 6 & hour(hms(winter.weekends$Time)) < 12,]
winter.weekends.afternoons <- winter.weekends[hour(hms(winter.weekends$Time)) >= 12 & hour(hms(winter.weekends$Time)) < 18,]


## Spring time windows
spring.weekdays <- subset(spring, spring$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
spring.weekends <- subset(spring, spring$Day_of_week %in% c("Saturday", "Sunday"))

spring.weekdays.evenings <- spring.weekdays[hour(hms(spring.weekdays$Time)) >= 18 & hour(hms(spring.weekdays$Time)) < 24,]
spring.weekdays.nights <- spring.weekdays[hour(hms(spring.weekdays$Time)) >= 0 & hour(hms(spring.weekdays$Time)) < 6,]
spring.weekdays.mornings <- spring.weekdays[hour(hms(spring.weekdays$Time)) >= 6 & hour(hms(spring.weekdays$Time)) < 12,]
spring.weekdays.afternoons <- spring.weekdays[hour(hms(spring.weekdays$Time)) >= 12 & hour(hms(spring.weekdays$Time)) < 18,]

spring.weekends.evenings <- spring.weekends[hour(hms(spring.weekends$Time)) >= 18 & hour(hms(spring.weekends$Time)) < 24,]
spring.weekends.nights <- spring.weekends[hour(hms(spring.weekends$Time)) >= 0 & hour(hms(spring.weekends$Time)) < 6,]
spring.weekends.mornings <- spring.weekends[hour(hms(spring.weekends$Time)) >= 6 & hour(hms(spring.weekends$Time)) < 12,]
spring.weekends.afternoons <- spring.weekends[hour(hms(spring.weekends$Time)) >= 12 & hour(hms(spring.weekends$Time)) < 18,]

## Christmas time windows
christmas.evenings <- christmas[hour(hms(christmas$Time)) >= 18 & hour(hms(christmas$Time)) < 24,]
christmas.nights <- christmas[hour(hms(christmas$Time)) >= 0 & hour(hms(christmas$Time)) < 6,]
christmas.mornings <- christmas[hour(hms(christmas$Time)) >= 6 & hour(hms(christmas$Time)) < 12,]
christmas.afternoons <- christmas[hour(hms(christmas$Time)) >= 12 & hour(hms(christmas$Time)) < 18,]

## Halloween time windows
halloween.evenings <- halloween[hour(hms(halloween$Time)) >= 18 & hour(hms(halloween$Time)) < 24,]
halloween.nights <- halloween[hour(hms(halloween$Time)) >= 0 & hour(hms(halloween$Time)) < 6,]
halloween.mornings <- halloween[hour(hms(halloween$Time)) >= 6 & hour(hms(halloween$Time)) < 12,]
halloween.afternoons <- halloween[hour(hms(halloween$Time)) >= 12 & hour(hms(halloween$Time)) < 18,]
'
