##  Test file for testing data sets
library(lubridate)
library(depmixS4)
library(ggplot2)
library(zoo)
set.seed(1)

##  Phase 2 approach 1
##  Out of Range

# fw.weekdays
min.fw.weekdays.days <- min(fw.weekdays.days$Global_active_power, na.rm=T)
max.fw.weekdays.days <- max(fw.weekdays.days$Global_active_power, na.rm=T)

min.fw.weekdays.evenings <- min(fw.weekdays.evenings$Global_active_power, na.rm=T)
max.fw.weekdays.evenings <- max(fw.weekdays.evenings$Global_active_power, na.rm=T)

min.fw.weekdays.nights <- min(fw.weekdays.nights$Global_active_power, na.rm=T)
max.fw.weekdays.nights <- max(fw.weekdays.nights$Global_active_power, na.rm=T)

# fw.weekends
min.fw.weekends.days <- min(fw.weekends.days$Global_active_power, na.rm=T)
max.fw.weekends.days <- max(fw.weekends.days$Global_active_power, na.rm=T)

min.fw.weekends.evenings <- min(fw.weekends.evenings$Global_active_power, na.rm=T)
max.fw.weekends.evenings <- max(fw.weekends.evenings$Global_active_power, na.rm=T)

min.fw.weekends.nights <- min(fw.weekends.nights$Global_active_power, na.rm=T)
max.fw.weekends.nights <- max(fw.weekends.nights$Global_active_power, na.rm=T)

# ss.weekdays
min.ss.weekdays.days <- min(ss.weekdays.days$Global_active_power, na.rm=T)
max.ss.weekdays.days <- max(ss.weekdays.days$Global_active_power, na.rm=T)

min.ss.weekdays.evenings <- min(ss.weekdays.evenings$Global_active_power, na.rm=T)
max.ss.weekdays.evenings <- max(ss.weekdays.evenings$Global_active_power, na.rm=T)

min.ss.weekdays.nights <- min(ss.weekdays.nights$Global_active_power, na.rm=T)
max.ss.weekdays.nights <- max(ss.weekdays.nights$Global_active_power, na.rm=T)

# ss.weekends
min.ss.weekends.days <- min(ss.weekends.days$Global_active_power, na.rm=T)
max.ss.weekends.days <- max(ss.weekends.days$Global_active_power, na.rm=T)

min.ss.weekends.evenings <- min(ss.weekends.evenings$Global_active_power, na.rm=T)
max.ss.weekends.evenings <- max(ss.weekends.evenings$Global_active_power, na.rm=T)

min.ss.weekends.nights <- min(ss.weekends.nights$Global_active_power, na.rm=T)
max.ss.weekends.nights <- max(ss.weekends.nights$Global_active_power, na.rm=T)

##  Test against the data

# # plot test data sets and have a different color for outliers
# day_minutes = length(t5.fw.weekdays.days$Global_active_power)
# evening_minutes = length(t5.fw.weekdays.evenings$Global_active_power)
# night_minutes = length(t5.fw.weekdays.nights$Global_active_power)
# 
# day_time_5 <- 1:day_minutes #9 hrs 
# evening_time_5 <- 1:evening_minutes #7 hrs
# night_time_5 <- 1:night_minutes  #8 hrs

# 
# t5.fw.weekdays.days <- t5.fw.weekdays.days[order(t5.fw.weekdays.days$Time),]
plot(t5.fw.weekdays.days$Time, t5.fw.weekdays.days$Global_active_power, type="p", cex=0.3, pch=20, col=ifelse(t5.fw.weekdays.days$Global_active_power > max.fw.weekdays.days | t5.fw.weekdays.days$Global_active_power < min.fw.weekdays.days, "red", "black"))
abline(h=max.fw.weekdays.days, col="red")
abline(h=min.fw.weekdays.days, col="red")

# plot test set 1
# day_minutes = length(t1.fw.weekdays.days$Global_active_power)
# evening_minutes = length(t1.fw.weekdays.evenings$Global_active_power)
# night_minutes = length(t1.fw.weekdays.nights$Global_active_power)
# 
# day_time_1 <- 1:day_minutes #9 hrs 
# evening_time_1 <- 1:evening_minutes #7 hrs
# night_time_1 <- 1:night_minutes  #8 hrs

##############  Datasets 1,2,5  ################

# t1.fw.weekdays.days
plot(t1.fw.weekdays.days$Time, t1.fw.weekdays.days$Global_active_power, type="p", col=ifelse(t1.fw.weekdays.days$Global_active_power > max.fw.weekdays.days | t1.fw.weekdays.days$Global_active_power < min.fw.weekdays.days, "red", "black"))
abline(h=max.fw.weekdays.days, col="red")
abline(h=min.fw.weekdays.days, col="red")
title(main = "Dataset 1: 8am-5pm Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.fw.weekdays.evenings
plot(t1.fw.weekdays.evenings$Time, t1.fw.weekdays.evenings$Global_active_power, type="p", col=ifelse(t1.fw.weekdays.evenings$Global_active_power > max.fw.weekdays.evenings | t1.fw.weekdays.evenings$Global_active_power < min.fw.weekdays.evenings, "red", "black"))
abline(h=max.fw.weekdays.evenings, col="red")
abline(h=min.fw.weekdays.evenings, col="red")
title(main = "Dataset 1: 5pm-12am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.fw.weekdays.nights
plot(t1.fw.weekdays.nights$Time, t1.fw.weekdays.nights$Global_active_power, type="p", col=ifelse(t1.fw.weekdays.nights$Global_active_power > max.fw.weekdays.nights | t1.fw.weekdays.nights$Global_active_power < min.fw.weekdays.nights, "red", "black"))
abline(h=max.fw.weekdays.nights, col="red")
abline(h=min.fw.weekdays.nights, col="red")
title(main = "Dataset 1: 12am-8am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")


# t1.ss.weekdays.days
plot(t1.ss.weekdays.days$Time, t1.ss.weekdays.days$Global_active_power, type="p", col=ifelse(t1.ss.weekdays.days$Global_active_power > max.ss.weekdays.days | t1.ss.weekdays.days$Global_active_power < min.ss.weekdays.days, "red", "black"))
abline(h=max.ss.weekdays.days, col="red")
abline(h=min.ss.weekdays.days, col="red")
title(main = "Dataset 1: 8am-5pm Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.ss.weekdays.evenings
plot(t1.ss.weekdays.evenings$Time, t1.ss.weekdays.evenings$Global_active_power, type="p", col=ifelse(t1.ss.weekdays.evenings$Global_active_power > max.ss.weekdays.evenings | t1.ss.weekdays.evenings$Global_active_power < min.ss.weekdays.evenings, "red", "black"))
abline(h=max.ss.weekdays.evenings, col="red")
abline(h=min.ss.weekdays.evenings, col="red")
title(main = "Dataset 1: 5pm-12am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.ss.weekdays.nights
plot(t1.ss.weekdays.nights$Time, t1.ss.weekdays.nights$Global_active_power, type="p", col=ifelse(t1.ss.weekdays.nights$Global_active_power > max.ss.weekdays.nights | t1.ss.weekdays.nights$Global_active_power < min.ss.weekdays.nights, "red", "black"))
abline(h=max.ss.weekdays.nights, col="red")
abline(h=min.ss.weekdays.nights, col="red")
title(main = "Dataset 1: 12am-8am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

#####
# t1.fw.weekends.days
plot(t1.fw.weekends.days$Time, t1.fw.weekends.days$Global_active_power, type="p", col=ifelse(t1.fw.weekends.days$Global_active_power > max.fw.weekends.days | t1.fw.weekends.days$Global_active_power < min.fw.weekends.days, "red", "black"))
abline(h=max.fw.weekends.days, col="red")
abline(h=min.fw.weekends.days, col="red")
title(main = "Dataset 2: 8am-5pm Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.fw.weekends.evenings
plot(t1.fw.weekends.evenings$Time, t1.fw.weekends.evenings$Global_active_power, type="p", col=ifelse(t1.fw.weekends.evenings$Global_active_power > max.fw.weekends.evenings | t1.fw.weekends.evenings$Global_active_power < min.fw.weekends.evenings, "red", "black"))
abline(h=max.fw.weekends.evenings, col="red")
abline(h=min.fw.weekends.evenings, col="red")
title(main = "Dataset 2: 5pm-12am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.fw.weekends.nights
plot(t1.fw.weekends.nights$Time, t1.fw.weekends.nights$Global_active_power, type="p", col=ifelse(t1.fw.weekends.nights$Global_active_power > max.fw.weekends.nights | t1.fw.weekends.nights$Global_active_power < min.fw.weekends.nights, "red", "black"))
abline(h=max.fw.weekends.nights, col="red")
abline(h=min.fw.weekends.nights, col="red")
title(main = "Dataset 2: 12am-8am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")


# t1.ss.weekends.days
plot(t1.ss.weekends.days$Time, t1.ss.weekends.days$Global_active_power, type="p", col=ifelse(t1.ss.weekends.days$Global_active_power > max.ss.weekends.days | t1.ss.weekends.days$Global_active_power < min.ss.weekends.days, "red", "black"))
abline(h=max.ss.weekends.days, col="red")
abline(h=min.ss.weekends.days, col="red")
title(main = "Dataset 2: 8am-5pm Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.ss.weekends.evenings
plot(t1.ss.weekends.evenings$Time, t1.ss.weekends.evenings$Global_active_power, type="p", col=ifelse(t1.ss.weekends.evenings$Global_active_power > max.ss.weekends.evenings | t1.ss.weekends.evenings$Global_active_power < min.ss.weekends.evenings, "red", "black"))
abline(h=max.ss.weekends.evenings, col="red")
abline(h=min.ss.weekends.evenings, col="red")
title(main = "Dataset 2: 5pm-12am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t1.ss.weekends.nights
plot(t1.ss.weekends.nights$Time, t1.ss.weekends.nights$Global_active_power, type="p", col=ifelse(t1.ss.weekends.nights$Global_active_power > max.ss.weekends.nights | t1.ss.weekends.nights$Global_active_power < min.ss.weekends.nights, "red", "black"))
abline(h=max.ss.weekends.nights, col="red")
abline(h=min.ss.weekends.nights, col="red")
title(main = "Dataset 2: 12am-8am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

################################################################################################################

# t2.fw.weekdays.days
plot(t2.fw.weekdays.days$Time, t2.fw.weekdays.days$Global_active_power, type="p", col=ifelse(t2.fw.weekdays.days$Global_active_power > max.fw.weekdays.days | t2.fw.weekdays.days$Global_active_power < min.fw.weekdays.days, "red", "black"))
abline(h=max.fw.weekdays.days, col="red")
abline(h=min.fw.weekdays.days, col="red")
title(main = "Dataset 2: 8am-5pm Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.fw.weekdays.evenings
plot(t2.fw.weekdays.evenings$Time, t2.fw.weekdays.evenings$Global_active_power, type="p", col=ifelse(t2.fw.weekdays.evenings$Global_active_power > max.fw.weekdays.evenings | t2.fw.weekdays.evenings$Global_active_power < min.fw.weekdays.evenings, "red", "black"))
abline(h=max.fw.weekdays.evenings, col="red")
abline(h=min.fw.weekdays.evenings, col="red")
title(main = "Dataset 2: 5pm-12am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.fw.weekdays.nights
plot(t2.fw.weekdays.nights$Time, t2.fw.weekdays.nights$Global_active_power, type="p", col=ifelse(t2.fw.weekdays.nights$Global_active_power > max.fw.weekdays.nights | t2.fw.weekdays.nights$Global_active_power < min.fw.weekdays.nights, "red", "black"))
abline(h=max.fw.weekdays.nights, col="red")
abline(h=min.fw.weekdays.nights, col="red")
title(main = "Dataset 2: 12am-8am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")


# t2.ss.weekdays.days
plot(t2.ss.weekdays.days$Time, t2.ss.weekdays.days$Global_active_power, type="p", col=ifelse(t2.ss.weekdays.days$Global_active_power > max.ss.weekdays.days | t2.ss.weekdays.days$Global_active_power < min.ss.weekdays.days, "red", "black"))
abline(h=max.ss.weekdays.days, col="red")
abline(h=min.ss.weekdays.days, col="red")
title(main = "Dataset 2: 8am-5pm Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.ss.weekdays.evenings
plot(t2.ss.weekdays.evenings$Time, t2.ss.weekdays.evenings$Global_active_power, type="p", col=ifelse(t2.ss.weekdays.evenings$Global_active_power > max.ss.weekdays.evenings | t2.ss.weekdays.evenings$Global_active_power < min.ss.weekdays.evenings, "red", "black"))
abline(h=max.ss.weekdays.evenings, col="red")
abline(h=min.ss.weekdays.evenings, col="red")
title(main = "Dataset 2: 5pm-12am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.ss.weekdays.nights
plot(t2.ss.weekdays.nights$Time, t2.ss.weekdays.nights$Global_active_power, type="p", col=ifelse(t2.ss.weekdays.nights$Global_active_power > max.ss.weekdays.nights | t2.ss.weekdays.nights$Global_active_power < min.ss.weekdays.nights, "red", "black"))
abline(h=max.ss.weekdays.nights, col="red")
abline(h=min.ss.weekdays.nights, col="red")
title(main = "Dataset 2: 12am-8am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

##### weekends

# t2.fw.weekends.days
plot(t2.fw.weekends.days$Time, t2.fw.weekends.days$Global_active_power, type="p", col=ifelse(t2.fw.weekends.days$Global_active_power > max.fw.weekends.days | t2.fw.weekends.days$Global_active_power < min.fw.weekends.days, "red", "black"))
abline(h=max.fw.weekends.days, col="red")
abline(h=min.fw.weekends.days, col="red")
title(main = "Dataset 2: 8am-5pm Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.fw.weekends.evenings
plot(t2.fw.weekends.evenings$Time, t2.fw.weekends.evenings$Global_active_power, type="p", col=ifelse(t2.fw.weekends.evenings$Global_active_power > max.fw.weekends.evenings | t2.fw.weekends.evenings$Global_active_power < min.fw.weekends.evenings, "red", "black"))
abline(h=max.fw.weekends.evenings, col="red")
abline(h=min.fw.weekends.evenings, col="red")
title(main = "Dataset 2: 5pm-12am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.fw.weekends.nights
plot(t2.fw.weekends.nights$Time, t2.fw.weekends.nights$Global_active_power, type="p", col=ifelse(t2.fw.weekends.nights$Global_active_power > max.fw.weekends.nights | t2.fw.weekends.nights$Global_active_power < min.fw.weekends.nights, "red", "black"))
abline(h=max.fw.weekends.nights, col="red")
abline(h=min.fw.weekends.nights, col="red")
title(main = "Dataset 2: 12am-8am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")


# t2.ss.weekends.days
plot(t2.ss.weekends.days$Time, t2.ss.weekends.days$Global_active_power, type="p", col=ifelse(t2.ss.weekends.days$Global_active_power > max.ss.weekends.days | t2.ss.weekends.days$Global_active_power < min.ss.weekends.days, "red", "black"))
abline(h=max.ss.weekends.days, col="red")
abline(h=min.ss.weekends.days, col="red")
title(main = "Dataset 2: 8am-5pm Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.ss.weekends.evenings
plot(t2.ss.weekends.evenings$Time, t2.ss.weekends.evenings$Global_active_power, type="p", col=ifelse(t2.ss.weekends.evenings$Global_active_power > max.ss.weekends.evenings | t2.ss.weekends.evenings$Global_active_power < min.ss.weekends.evenings, "red", "black"))
abline(h=max.ss.weekends.evenings, col="red")
abline(h=min.ss.weekends.evenings, col="red")
title(main = "Dataset 2: 5pm-12am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t2.ss.weekends.nights
plot(t2.ss.weekends.nights$Time, t2.ss.weekends.nights$Global_active_power, type="p", col=ifelse(t2.ss.weekends.nights$Global_active_power > max.ss.weekends.nights | t2.ss.weekends.nights$Global_active_power < min.ss.weekends.nights, "red", "black"))
abline(h=max.ss.weekends.nights, col="red")
abline(h=min.ss.weekends.nights, col="red")
title(main = "Dataset 2: 12am-8am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

################################################################################################################

# t4.fw.weekdays.days
plot(t4.fw.weekdays.days$Time, t4.fw.weekdays.days$Global_active_power, type="p", col=ifelse(t4.fw.weekdays.days$Global_active_power > max.fw.weekdays.days | t4.fw.weekdays.days$Global_active_power < min.fw.weekdays.days, "red", "black"))
abline(h=max.fw.weekdays.days, col="red")
abline(h=min.fw.weekdays.days, col="red")
title(main = "Dataset 4: 8am-5pm Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.fw.weekdays.evenings
plot(t4.fw.weekdays.evenings$Time, t4.fw.weekdays.evenings$Global_active_power, type="p", col=ifelse(t4.fw.weekdays.evenings$Global_active_power > max.fw.weekdays.evenings | t4.fw.weekdays.evenings$Global_active_power < min.fw.weekdays.evenings, "red", "black"))
abline(h=max.fw.weekdays.evenings, col="red")
abline(h=min.fw.weekdays.evenings, col="red")
title(main = "Dataset 4: 5pm-12am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.fw.weekdays.nights
plot(t4.fw.weekdays.nights$Time, t4.fw.weekdays.nights$Global_active_power, type="p", col=ifelse(t4.fw.weekdays.nights$Global_active_power > max.fw.weekdays.nights | t4.fw.weekdays.nights$Global_active_power < min.fw.weekdays.nights, "red", "black"))
abline(h=max.fw.weekdays.nights, col="red")
abline(h=min.fw.weekdays.nights, col="red")
title(main = "Dataset 4: 12am-8am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")


# t4.ss.weekdays.days
plot(t4.ss.weekdays.days$Time, t4.ss.weekdays.days$Global_active_power, type="p", col=ifelse(t4.ss.weekdays.days$Global_active_power > max.ss.weekdays.days | t4.ss.weekdays.days$Global_active_power < min.ss.weekdays.days, "red", "black"))
abline(h=max.ss.weekdays.days, col="red")
abline(h=min.ss.weekdays.days, col="red")
title(main = "Dataset 4: 8am-5pm Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.ss.weekdays.evenings
plot(t4.ss.weekdays.evenings$Time, t4.ss.weekdays.evenings$Global_active_power, type="p", col=ifelse(t4.ss.weekdays.evenings$Global_active_power > max.ss.weekdays.evenings | t4.ss.weekdays.evenings$Global_active_power < min.ss.weekdays.evenings, "red", "black"))
abline(h=max.ss.weekdays.evenings, col="red")
abline(h=min.ss.weekdays.evenings, col="red")
title(main = "Dataset 4: 5pm-12am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.ss.weekdays.nights
plot(t4.ss.weekdays.nights$Time, t4.ss.weekdays.nights$Global_active_power, type="p", col=ifelse(t4.ss.weekdays.nights$Global_active_power > max.ss.weekdays.nights | t4.ss.weekdays.nights$Global_active_power < min.ss.weekdays.nights, "red", "black"))
abline(h=max.ss.weekdays.nights, col="red")
abline(h=min.ss.weekdays.nights, col="red")
title(main = "Dataset 4: 12am-8am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

##### weekends

# t4.fw.weekends.days
plot(t4.fw.weekends.days$Time, t4.fw.weekends.days$Global_active_power, type="p", col=ifelse(t4.fw.weekends.days$Global_active_power > max.fw.weekends.days | t4.fw.weekends.days$Global_active_power < min.fw.weekends.days, "red", "black"))
abline(h=max.fw.weekends.days, col="red")
abline(h=min.fw.weekends.days, col="red")
title(main = "Dataset 4: 8am-5pm Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.fw.weekends.evenings
plot(t4.fw.weekends.evenings$Time, t4.fw.weekends.evenings$Global_active_power, type="p", col=ifelse(t4.fw.weekends.evenings$Global_active_power > max.fw.weekends.evenings | t4.fw.weekends.evenings$Global_active_power < min.fw.weekends.evenings, "red", "black"))
abline(h=max.fw.weekends.evenings, col="red")
abline(h=min.fw.weekends.evenings, col="red")
title(main = "Dataset 4: 5pm-12am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.fw.weekends.nights
plot(t4.fw.weekends.nights$Time, t4.fw.weekends.nights$Global_active_power, type="p", col=ifelse(t4.fw.weekends.nights$Global_active_power > max.fw.weekends.nights | t4.fw.weekends.nights$Global_active_power < min.fw.weekends.nights, "red", "black"))
abline(h=max.fw.weekends.nights, col="red")
abline(h=min.fw.weekends.nights, col="red")
title(main = "Dataset 4: 12am-8am Fall + Winter", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")


# t4.ss.weekends.days
plot(t4.ss.weekends.days$Time, t4.ss.weekends.days$Global_active_power, type="p", col=ifelse(t4.ss.weekends.days$Global_active_power > max.ss.weekends.days | t4.ss.weekends.days$Global_active_power < min.ss.weekends.days, "red", "black"))
abline(h=max.ss.weekends.days, col="red")
abline(h=min.ss.weekends.days, col="red")
title(main = "Dataset 4: 8am-5pm Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.ss.weekends.evenings
plot(t4.ss.weekends.evenings$Time, t4.ss.weekends.evenings$Global_active_power, type="p", col=ifelse(t4.ss.weekends.evenings$Global_active_power > max.ss.weekends.evenings | t4.ss.weekends.evenings$Global_active_power < min.ss.weekends.evenings, "red", "black"))
abline(h=max.ss.weekends.evenings, col="red")
abline(h=min.ss.weekends.evenings, col="red")
title(main = "Dataset 4: 5pm-12am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

# t4.ss.weekends.nights
plot(t4.ss.weekends.nights$Time, t4.ss.weekends.nights$Global_active_power, type="p", col=ifelse(t4.ss.weekends.nights$Global_active_power > max.ss.weekends.nights | t4.ss.weekends.nights$Global_active_power < min.ss.weekends.nights, "red", "black"))
abline(h=max.ss.weekends.nights, col="red")
abline(h=min.ss.weekends.nights, col="red")
title(main = "Dataset 4: 12am-8am Spring + Summer", sub = NULL, 
      xlab = "Time", ylab = "Global Active Power")

