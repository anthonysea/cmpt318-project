library('lubridate')
# LOad dataset
ft <- read.csv("./finalTest.txt")
# Normalize Date attribute
ft$Date <- as.Date(ft$Date, "%d/%m/%Y")
# Add Day_of_week attribute
ft$Day_of_week <- weekdays(as.Date(ft$Date, "%d/%d/%Y"))
# Subset dataset into F/W and S/S
ft.fw <- subset(ft, month(ft$Date) %in% c(9, 10, 11, 12, 1, 2))
ft.ss <- subset(ft, month(ft$Date) %in% c(3, 4, 5, 6, 7, 8))

ft.fw.weekdays <- subset(ft.fw, ft.fw$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ft.fw.weekends <- subset(ft.fw, ft.fw$Day_of_week %in% c("Saturday", "Sunday"))

ft.ss.weekdays <- subset(ft.ss, ft.ss$Day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ft.ss.weekends <- subset(ft.ss, ft.ss$Day_of_week %in% c("Saturday", "Sunday"))

ft.fw.weekdays.nights <- ft.fw.weekdays[hour(hms(ft.fw.weekdays$Time)) >= 0 & hour(hms(ft.fw.weekdays$Time)) < 8,]
ft.fw.weekdays.nights <- naMeanFill(ft.fw.weekdays.nights)
ft.fw.weekdays.days <- ft.fw.weekdays[hour(hms(ft.fw.weekdays$Time)) >= 8 & hour(hms(ft.fw.weekdays$Time)) < 17, ]
ft.fw.weekdays.days <- naMeanFill(ft.fw.weekdays.days)
ft.fw.weekdays.evenings <- ft.fw.weekdays[hour(hms(ft.fw.weekdays$Time)) >= 17 & hour(hms(ft.fw.weekdays$Time)) < 24, ]
ft.fw.weekdays.evenings <- naMeanFill(ft.fw.weekdays.evenings)

ft.fw.weekends.nights <- ft.fw.weekends[hour(hms(ft.fw.weekends$Time)) >= 0 & hour(hms(ft.fw.weekends$Time)) < 8, ]
ft.fw.weekends.nights <- naMeanFill(ft.fw.weekends.nights)
ft.fw.weekends.days <- ft.fw.weekends[hour(hms(ft.fw.weekends$Time)) >= 8 & hour(hms(ft.fw.weekends$Time)) < 17, ]
ft.fw.weekends.days <- naMeanFill(ft.fw.weekends.days)
ft.fw.weekends.evenings <- ft.fw.weekends[hour(hms(ft.fw.weekends$Time)) >= 17 & hour(hms(ft.fw.weekends$Time)) < 24, ]
ft.fw.weekends.evenings <- naMeanFill(ft.fw.weekends.evenings)

ft.ss.weekdays.nights <- ft.ss.weekdays[hour(hms(ft.ss.weekdays$Time)) >= 0 & hour(hms(ft.ss.weekdays$Time)) < 8, ]
ft.ss.weekdays.nights <- naMeanFill(ft.ss.weekdays.nights)
ft.ss.weekdays.days <- ft.ss.weekdays[hour(hms(ft.ss.weekdays$Time)) >= 8 & hour(hms(ft.ss.weekdays$Time)) < 17, ]
ft.ss.weekdays.days <- naMeanFill(ft.ss.weekdays.days)
ft.ss.weekdays.evenings <- ft.ss.weekdays[hour(hms(ft.ss.weekdays$Time)) >= 17 & hour(hms(ft.ss.weekdays$Time)) < 24, ]
ft.ss.weekdays.evenings <- naMeanFill(ft.ss.weekdays.evenings)

ft.ss.weekends.nights <- ft.ss.weekends[hour(hms(ft.ss.weekends$Time)) >= 0 & hour(hms(ft.ss.weekends$Time)) < 8, ]
ft.ss.weekends.nights <- naMeanFill(ft.ss.weekends.nights)
ft.ss.weekends.days <- ft.ss.weekends[hour(hms(ft.ss.weekends$Time)) >= 8 & hour(hms(ft.ss.weekends$Time)) < 17, ]
ft.ss.weekends.days <- naMeanFill(ft.ss.weekends.days)
ft.ss.weekends.evenings <- ft.ss.weekends[hour(hms(ft.ss.weekends$Time)) >= 17 & hour(hms(ft.ss.weekends$Time)) < 24, ]
ft.ss.weekends.evenings <- naMeanFill(ft.ss.weekends.evenings)

## HMM Testing



