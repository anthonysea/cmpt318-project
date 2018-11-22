test1 <- read.csv("test1.txt")
test2 <- read.csv("test2.txt")
test3 <- read.csv("test3.txt")
test4 <- read.csv("test4.txt")
test5 <- read.csv("test5.txt")

test1$Date <- as.Date(test1$Date, "%d/%m/%Y")
test2$Date <- as.Date(test2$Date, "%d/%m/%Y")
test3$Date <- as.Date(test3$Date, "%d/%m/%Y")
test4$Date <- as.Date(test4$Date, "%d/%m/%Y")
test5$Date <- as.Date(test5$Date, "%d/%m/%Y")

# Add day of week attribute
test1$Day_of_week <- weekdays(as.Date(test1$Date, "%d/%m/%Y"))
test2$Day_of_week <- weekdays(as.Date(test2$Date, "%d/%m/%Y"))
test3$Day_of_week <- weekdays(as.Date(test3$Date, "%d/%m/%Y"))
test4$Day_of_week <- weekdays(as.Date(test4$Date, "%d/%m/%Y"))
test5$Day_of_week <- weekdays(as.Date(test5$Date, "%d/%m/%Y"))

# Remove rows that have any null attributes
test1 <- test1[rowSums(is.na(test1)) == 0,]
test2 <- test2[rowSums(is.na(test2)) == 0,]
test3 <- test3[rowSums(is.na(test3)) == 0,]
test4 <- test4[rowSums(is.na(test4)) == 0,]
test5 <- test5[rowSums(is.na(test5)) == 0,]

t1.fw <- subset(test1, month(test1$Date) %in% c(9, 10, 11, 12, 1, 2))
t1.ss <- subset(test1, month(test1$Date) %in% c(3, 4, 5, 6, 7, 8))
t1.fw.weekdays <- subset(t1.fw, t1.fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t1.fw.weekends <- subset(t1.fw, t1.fw$Day_of_week %in% c("Saturday", "Sunday"))
t1.ss.weekdays <- subset(t1.ss, t1.ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t1.ss.weekends <- subset(t1.ss, t1.ss$Day_of_week %in% c("Saturday", "Sunday"))
t1.fw.weekdays.nights <- t1.fw.weekdays[hour(hms(t1.fw.weekdays$Time)) >= 0 & hour(hms(t1.fw.weekdays$Time)) < 8,]
t1.fw.weekdays.days <- t1.fw.weekdays[hour(hms(t1.fw.weekdays$Time)) >= 8 & hour(hms(t1.fw.weekdays$Time)) < 17,]
t1.fw.weekdays.evenings <- t1.fw.weekdays[hour(hms(t1.fw.weekdays$Time)) >= 17 & hour(hms(t1.fw.weekdays$Time)) < 24,]

t1.fw.weekends.nights <- t1.fw.weekends[hour(hms(t1.fw.weekends$Time)) >= 0 & hour(hms(t1.fw.weekends$Time)) < 8,]
t1.fw.weekends.days <- t1.fw.weekends[hour(hms(t1.fw.weekends$Time)) >= 8 & hour(hms(t1.fw.weekends$Time)) < 17,]
t1.fw.weekends.evenings <- t1.fw.weekends[hour(hms(t1.fw.weekends$Time)) >= 17 & hour(hms(t1.fw.weekends$Time)) < 24,]

t1.ss.weekdays.nights <- t1.ss.weekdays[hour(hms(t1.ss.weekdays$Time)) >= 0 & hour(hms(t1.ss.weekdays$Time)) < 8,]
t1.ss.weekdays.days <- t1.ss.weekdays[hour(hms(t1.ss.weekdays$Time)) >= 8 & hour(hms(t1.ss.weekdays$Time)) < 17,]
t1.ss.weekdays.evenings <- t1.ss.weekdays[hour(hms(t1.ss.weekdays$Time)) >= 17 & hour(hms(t1.ss.weekdays$Time)) < 24,]

t1.ss.weekends.nights <- t1.ss.weekends[hour(hms(t1.ss.weekends$Time)) >= 0 & hour(hms(t1.ss.weekends$Time)) < 8,]
t1.ss.weekends.days <- t1.ss.weekends[hour(hms(t1.ss.weekends$Time)) >= 8 & hour(hms(t1.ss.weekends$Time)) < 17,]
t1.ss.weekends.evenings <- t1.ss.weekends[hour(hms(t1.ss.weekends$Time)) >= 17 & hour(hms(t1.ss.weekends$Time)) < 24,]



t2.fw <- subset(test2, month(test2$Date) %in% c(9, 10, 11, 12, 1, 2))
t2.ss <- subset(test2, month(test2$Date) %in% c(3, 4, 5, 6, 7, 8))
t2.fw.weekdays <- subset(t2.fw, t2.fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t2.fw.weekends <- subset(t2.fw, t2.fw$Day_of_week %in% c("Saturday", "Sunday"))
t2.ss.weekdays <- subset(t2.ss, t2.ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t2.ss.weekends <- subset(t2.ss, t2.ss$Day_of_week %in% c("Saturday", "Sunday"))
t2.fw.weekdays.nights <- t2.fw.weekdays[hour(hms(t2.fw.weekdays$Time)) >= 0 & hour(hms(t2.fw.weekdays$Time)) < 8,]
t2.fw.weekdays.days <- t2.fw.weekdays[hour(hms(t2.fw.weekdays$Time)) >= 8 & hour(hms(t2.fw.weekdays$Time)) < 17,]
t2.fw.weekdays.evenings <- t2.fw.weekdays[hour(hms(t2.fw.weekdays$Time)) >= 17 & hour(hms(t2.fw.weekdays$Time)) < 24,]

t2.fw.weekends.nights <- t2.fw.weekends[hour(hms(t2.fw.weekends$Time)) >= 0 & hour(hms(t2.fw.weekends$Time)) < 8,]
t2.fw.weekends.days <- t2.fw.weekends[hour(hms(t2.fw.weekends$Time)) >= 8 & hour(hms(t2.fw.weekends$Time)) < 17,]
t2.fw.weekends.evenings <- t2.fw.weekends[hour(hms(t2.fw.weekends$Time)) >= 17 & hour(hms(t2.fw.weekends$Time)) < 24,]

t2.ss.weekdays.nights <- t2.ss.weekdays[hour(hms(t2.ss.weekdays$Time)) >= 0 & hour(hms(t2.ss.weekdays$Time)) < 8,]
t2.ss.weekdays.days <- t2.ss.weekdays[hour(hms(t2.ss.weekdays$Time)) >= 8 & hour(hms(t2.ss.weekdays$Time)) < 17,]
t2.ss.weekdays.evenings <- t2.ss.weekdays[hour(hms(t2.ss.weekdays$Time)) >= 17 & hour(hms(t2.ss.weekdays$Time)) < 24,]

t2.ss.weekends.nights <- t2.ss.weekends[hour(hms(t2.ss.weekends$Time)) >= 0 & hour(hms(t2.ss.weekends$Time)) < 8,]
t2.ss.weekends.days <- t2.ss.weekends[hour(hms(t2.ss.weekends$Time)) >= 8 & hour(hms(t2.ss.weekends$Time)) < 17,]
t2.ss.weekends.evenings <- t2.ss.weekends[hour(hms(t2.ss.weekends$Time)) >= 17 & hour(hms(t2.ss.weekends$Time)) < 24,]



t3.fw <- subset(test3, month(test3$Date) %in% c(9, 10, 11, 12, 1, 2))
t3.ss <- subset(test3, month(test3$Date) %in% c(3, 4, 5, 6, 7, 8))
t3.fw.weekdays <- subset(t3.fw, t3.fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t3.fw.weekends <- subset(t3.fw, t3.fw$Day_of_week %in% c("Saturday", "Sunday"))
t3.ss.weekdays <- subset(t3.ss, t3.ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t3.ss.weekends <- subset(t3.ss, t3.ss$Day_of_week %in% c("Saturday", "Sunday"))
t3.fw.weekdays.nights <- t3.fw.weekdays[hour(hms(t3.fw.weekdays$Time)) >= 0 & hour(hms(t3.fw.weekdays$Time)) < 8,]
t3.fw.weekdays.days <- t3.fw.weekdays[hour(hms(t3.fw.weekdays$Time)) >= 8 & hour(hms(t3.fw.weekdays$Time)) < 17,]
t3.fw.weekdays.evenings <- t3.fw.weekdays[hour(hms(t3.fw.weekdays$Time)) >= 17 & hour(hms(t3.fw.weekdays$Time)) < 24,]

t3.fw.weekends.nights <- t3.fw.weekends[hour(hms(t3.fw.weekends$Time)) >= 0 & hour(hms(t3.fw.weekends$Time)) < 8,]
t3.fw.weekends.days <- t3.fw.weekends[hour(hms(t3.fw.weekends$Time)) >= 8 & hour(hms(t3.fw.weekends$Time)) < 17,]
t3.fw.weekends.evenings <- t3.fw.weekends[hour(hms(t3.fw.weekends$Time)) >= 17 & hour(hms(t3.fw.weekends$Time)) < 24,]

t3.ss.weekdays.nights <- t3.ss.weekdays[hour(hms(t3.ss.weekdays$Time)) >= 0 & hour(hms(t3.ss.weekdays$Time)) < 8,]
t3.ss.weekdays.days <- t3.ss.weekdays[hour(hms(t3.ss.weekdays$Time)) >= 8 & hour(hms(t3.ss.weekdays$Time)) < 17,]
t3.ss.weekdays.evenings <- t3.ss.weekdays[hour(hms(t3.ss.weekdays$Time)) >= 17 & hour(hms(t3.ss.weekdays$Time)) < 24,]

t3.ss.weekends.nights <- t3.ss.weekends[hour(hms(t3.ss.weekends$Time)) >= 0 & hour(hms(t3.ss.weekends$Time)) < 8,]
t3.ss.weekends.days <- t3.ss.weekends[hour(hms(t3.ss.weekends$Time)) >= 8 & hour(hms(t3.ss.weekends$Time)) < 17,]
t3.ss.weekends.evenings <- t3.ss.weekends[hour(hms(t3.ss.weekends$Time)) >= 17 & hour(hms(t3.ss.weekends$Time)) < 24,]



t4.fw <- subset(test4, month(test4$Date) %in% c(9, 10, 11, 12, 1, 2))
t4.ss <- subset(test4, month(test4$Date) %in% c(3, 4, 5, 6, 7, 8))
t4.fw.weekdays <- subset(t4.fw, t4.fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t4.fw.weekends <- subset(t4.fw, t4.fw$Day_of_week %in% c("Saturday", "Sunday"))
t4.ss.weekdays <- subset(t4.ss, t4.ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t4.ss.weekends <- subset(t4.ss, t4.ss$Day_of_week %in% c("Saturday", "Sunday"))
t4.fw.weekdays.nights <- t4.fw.weekdays[hour(hms(t4.fw.weekdays$Time)) >= 0 & hour(hms(t4.fw.weekdays$Time)) < 8,]
t4.fw.weekdays.days <- t4.fw.weekdays[hour(hms(t4.fw.weekdays$Time)) >= 8 & hour(hms(t4.fw.weekdays$Time)) < 17,]
t4.fw.weekdays.evenings <- t4.fw.weekdays[hour(hms(t4.fw.weekdays$Time)) >= 17 & hour(hms(t4.fw.weekdays$Time)) < 24,]

t4.fw.weekends.nights <- t4.fw.weekends[hour(hms(t4.fw.weekends$Time)) >= 0 & hour(hms(t4.fw.weekends$Time)) < 8,]
t4.fw.weekends.days <- t4.fw.weekends[hour(hms(t4.fw.weekends$Time)) >= 8 & hour(hms(t4.fw.weekends$Time)) < 17,]
t4.fw.weekends.evenings <- t4.fw.weekends[hour(hms(t4.fw.weekends$Time)) >= 17 & hour(hms(t4.fw.weekends$Time)) < 24,]

t4.ss.weekdays.nights <- t4.ss.weekdays[hour(hms(t4.ss.weekdays$Time)) >= 0 & hour(hms(t4.ss.weekdays$Time)) < 8,]
t4.ss.weekdays.days <- t4.ss.weekdays[hour(hms(t4.ss.weekdays$Time)) >= 8 & hour(hms(t4.ss.weekdays$Time)) < 17,]
t4.ss.weekdays.evenings <- t4.ss.weekdays[hour(hms(t4.ss.weekdays$Time)) >= 17 & hour(hms(t4.ss.weekdays$Time)) < 24,]

t4.ss.weekends.nights <- t4.ss.weekends[hour(hms(t4.ss.weekends$Time)) >= 0 & hour(hms(t4.ss.weekends$Time)) < 8,]
t4.ss.weekends.days <- t4.ss.weekends[hour(hms(t4.ss.weekends$Time)) >= 8 & hour(hms(t4.ss.weekends$Time)) < 17,]
t4.ss.weekends.evenings <- t4.ss.weekends[hour(hms(t4.ss.weekends$Time)) >= 17 & hour(hms(t4.ss.weekends$Time)) < 24,]



t5.fw <- subset(test5, month(test5$Date) %in% c(9, 10, 11, 12, 1, 2))
t5.ss <- subset(test5, month(test5$Date) %in% c(3, 4, 5, 6, 7, 8))
t5.fw.weekdays <- subset(t5.fw, t5.fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t5.fw.weekends <- subset(t5.fw, t5.fw$Day_of_week %in% c("Saturday", "Sunday"))
t5.ss.weekdays <- subset(t5.ss, t5.ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
t5.ss.weekends <- subset(t5.ss, t5.ss$Day_of_week %in% c("Saturday", "Sunday"))
t5.fw.weekdays.nights <- t5.fw.weekdays[hour(hms(t5.fw.weekdays$Time)) >= 0 & hour(hms(t5.fw.weekdays$Time)) < 8,]
t5.fw.weekdays.days <- t5.fw.weekdays[hour(hms(t5.fw.weekdays$Time)) >= 8 & hour(hms(t5.fw.weekdays$Time)) < 17,]
t5.fw.weekdays.evenings <- t5.fw.weekdays[hour(hms(t5.fw.weekdays$Time)) >= 17 & hour(hms(t5.fw.weekdays$Time)) < 24,]

t5.fw.weekends.nights <- t5.fw.weekends[hour(hms(t5.fw.weekends$Time)) >= 0 & hour(hms(t5.fw.weekends$Time)) < 8,]
t5.fw.weekends.days <- t5.fw.weekends[hour(hms(t5.fw.weekends$Time)) >= 8 & hour(hms(t5.fw.weekends$Time)) < 17,]
t5.fw.weekends.evenings <- t5.fw.weekends[hour(hms(t5.fw.weekends$Time)) >= 17 & hour(hms(t5.fw.weekends$Time)) < 24,]

t5.ss.weekdays.nights <- t5.ss.weekdays[hour(hms(t5.ss.weekdays$Time)) >= 0 & hour(hms(t5.ss.weekdays$Time)) < 8,]
t5.ss.weekdays.days <- t5.ss.weekdays[hour(hms(t5.ss.weekdays$Time)) >= 8 & hour(hms(t5.ss.weekdays$Time)) < 17,]
t5.ss.weekdays.evenings <- t5.ss.weekdays[hour(hms(t5.ss.weekdays$Time)) >= 17 & hour(hms(t5.ss.weekdays$Time)) < 24,]

t5.ss.weekends.nights <- t5.ss.weekends[hour(hms(t5.ss.weekends$Time)) >= 0 & hour(hms(t5.ss.weekends$Time)) < 8,]
t5.ss.weekends.days <- t5.ss.weekends[hour(hms(t5.ss.weekends$Time)) >= 8 & hour(hms(t5.ss.weekends$Time)) < 17,]
t5.ss.weekends.evenings <- t5.ss.weekends[hour(hms(t5.ss.weekends$Time)) >= 17 & hour(hms(t5.ss.weekends$Time)) < 24,]

