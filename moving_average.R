library(lubridate)
library(depmixS4)
library(ggplot2)
library(zoo)
set.seed(1)

window <- 30

##weekdays_fw

roll.fw.weekdays.days <- rollapplyr(fw.weekdays.days$Global_active_power, window, mean)
roll.fw.weekdays.days <- c(rep(roll.fw.weekdays.days[1],window-1),roll.fw.weekdays.days)

roll.fw.weekdays.evenings <- rollapplyr(fw.weekdays.evenings$Global_active_power, window, mean)
roll.fw.weekdays.evenings <- c(rep(roll.fw.weekdays.evenings[1],window-1),roll.fw.weekdays.evenings)

roll.fw.weekdays.nights <- rollapplyr(fw.weekdays.nights$Global_active_power, window, mean)
roll.fw.weekdays.nights <- c(rep(roll.fw.weekdays.nights[1],window-1),roll.fw.weekdays.nights)

##anomaly detection fw weekdays
thresh <- 3

dif.fw.weekdays.days <- abs(roll.fw.weekdays.days - fw.weekdays.days$Global_active_power)
anomalies.fw.weekdays.days <- fw.weekdays.days$Global_active_power[which(dif.fw.weekdays.days > thresh)]


day_time.fw.weekdays.days <- 1:length(fw.weekdays.days$Global_active_power)
x.anomalies.fw.weekdays.days <- fw.weekdays.days$Time[which(dif.fw.weekdays.days > thresh)]
plot(fw.weekdays.days$Time,fw.weekdays.days$Global_active_power,type="p", col="black", pch=20, main=paste("Fall Winter\nWeekdays Days, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.fw.weekdays.days, anomalies.fw.weekdays.days, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


dif.fw.weekdays.evenings <- abs(roll.fw.weekdays.evenings - fw.weekdays.evenings$Global_active_power)
anomalies.fw.weekdays.evenings <- fw.weekdays.evenings$Global_active_power[which(dif.fw.weekdays.evenings > thresh)]

day_time.fw.weekdays.evenings <- 1:length(fw.weekdays.evenings$Global_active_power)
x.anomalies.fw.weekdays.evenings <- fw.weekdays.evenings$Time[which(dif.fw.weekdays.evenings > thresh)]
plot(fw.weekdays.evenings$Time,fw.weekdays.evenings$Global_active_power,type="p", col="black", pch=20, main=paste("Fall Winter\nWeekdays Evenings, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.fw.weekdays.evenings, anomalies.fw.weekdays.evenings, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)

dif.fw.weekdays.nights <- abs(roll.fw.weekdays.nights - fw.weekdays.nights$Global_active_power)
anomalies.fw.weekdays.nights <- fw.weekdays.nights$Global_active_power[which(dif.fw.weekdays.nights > thresh)]

day_time.fw.weekdays.nights <- 1:length(fw.weekdays.nights$Global_active_power)
x.anomalies.fw.weekdays.nights <- fw.weekdays.nights$Time[which(dif.fw.weekdays.nights > thresh)]
plot(fw.weekdays.nights$Time,fw.weekdays.nights$Global_active_power,type="p", col="black", pch=20, main=paste("Fall Winter\nWeekdays Nights, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.fw.weekdays.nights, anomalies.fw.weekdays.nights, pch=20,col="red")
legend(20000,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)
##weekends_fw

roll.fw.weekends.days <- rollapplyr(fw.weekends.days$Global_active_power, window, mean)
roll.fw.weekends.days <- c(rep(roll.fw.weekends.days[1],window-1),roll.fw.weekends.days)

roll.fw.weekends.evenings <- rollapplyr(fw.weekends.evenings$Global_active_power, window, mean)
roll.fw.weekends.evenings <- c(rep(roll.fw.weekends.evenings[1],window-1),roll.fw.weekends.evenings)

roll.fw.weekends.nights <- rollapplyr(fw.weekends.nights$Global_active_power, window, mean)
roll.fw.weekends.nights <- c(rep(roll.fw.weekends.nights[1],window-1),roll.fw.weekends.nights)
##anomalies weekends

dif.fw.weekends.days <- abs(roll.fw.weekends.days - fw.weekends.days$Global_active_power)
anomalies.fw.weekends.days <- fw.weekends.days$Global_active_power[which(dif.fw.weekends.days > thresh)]

day_time.fw.weekends.days <- 1:length(fw.weekends.days$Global_active_power)
x.anomalies.fw.weekends.days <- fw.weekends.days$Time[which(dif.fw.weekends.days > thresh)]
plot(fw.weekends.days$Time,fw.weekends.days$Global_active_power,type="p", col="black", pch=20, main=paste("Fall Winter\nweekends Days, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.fw.weekends.days, anomalies.fw.weekends.days, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)

dif.fw.weekends.evenings <- abs(roll.fw.weekends.evenings - fw.weekends.evenings$Global_active_power)
anomalies.fw.weekends.evenings <- fw.weekends.evenings$Global_active_power[which(dif.fw.weekends.evenings > thresh)]

day_time.fw.weekends.evenings <- 1:length(fw.weekends.evenings$Global_active_power)
x.anomalies.fw.weekends.evenings <- fw.weekends.evenings$Time[which(dif.fw.weekends.evenings > thresh)]
plot(fw.weekends.evenings$Time,fw.weekends.evenings$Global_active_power,type="p", col="black", pch=20, main=paste("Fall Winter\nweekends Evenings, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.fw.weekends.evenings, anomalies.fw.weekends.evenings, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)

dif.fw.weekends.nights <- abs(roll.fw.weekends.nights - fw.weekends.nights$Global_active_power)
anomalies.fw.weekends.nights <- fw.weekends.nights$Global_active_power[which(dif.fw.weekends.nights > thresh)]

day_time.fw.weekends.nights <- 1:length(fw.weekends.nights$Global_active_power)
x.anomalies.fw.weekends.nights <- fw.weekends.nights$Time[which(dif.fw.weekends.nights > thresh)]
plot(fw.weekends.nights$Time,fw.weekends.nights$Global_active_power,type="p", col="black", pch=20, main=paste("Fall Winter\nweekends Nights, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.fw.weekends.nights, anomalies.fw.weekends.nights, pch=20,col="red")
legend(20000,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


##weekdays_ss

roll.ss.weekdays.days <- rollapplyr(ss.weekdays.days$Global_active_power, window, mean)
roll.ss.weekdays.days <- c(rep(roll.ss.weekdays.days[1],window-1),roll.ss.weekdays.days)

roll.ss.weekdays.evenings <- rollapplyr(ss.weekdays.evenings$Global_active_power, window, mean)
roll.ss.weekdays.evenings <- c(rep(roll.ss.weekdays.evenings[1],window-1),roll.ss.weekdays.evenings)

roll.ss.weekdays.nights <- rollapplyr(ss.weekdays.nights$Global_active_power, window, mean)
roll.ss.weekdays.nights <- c(rep(roll.ss.weekdays.nights[1],window-1),roll.ss.weekdays.nights)

##anomaly detection aa weekdays
dif.ss.weekdays.days <- abs(roll.ss.weekdays.days - ss.weekdays.days$Global_active_power)
anomalies.ss.weekdays.days <- ss.weekdays.days$Global_active_power[which(dif.ss.weekdays.days > thresh)]

day_time.ss.weekdays.days <- 1:length(ss.weekdays.days$Global_active_power)
x.anomalies.ss.weekdays.days <- ss.weekdays.days$Time[which(dif.ss.weekdays.days > thresh)]
plot(ss.weekdays.days$Time,ss.weekdays.days$Global_active_power,type="p", col="black", pch=20, main=paste("Spring Summer\nWeekdays Days, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.ss.weekdays.days, anomalies.ss.weekdays.days, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


dif.ss.weekdays.evenings <- abs(roll.ss.weekdays.evenings - ss.weekdays.evenings$Global_active_power)
anomalies.ss.weekdays.evenings <- ss.weekdays.evenings$Global_active_power[which(dif.ss.weekdays.evenings > thresh)]

day_time.ss.weekdays.evenings <- 1:length(ss.weekdays.evenings$Global_active_power)
x.anomalies.ss.weekdays.evenings <- ss.weekdays.evenings$Time[which(dif.ss.weekdays.evenings > thresh)]
plot(ss.weekdays.evenings$Time,ss.weekdays.evenings$Global_active_power,type="p", col="black", pch=20, main=paste("Spring Summer\nWeekdays Evenings, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.ss.weekdays.evenings, anomalies.ss.weekdays.evenings, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


dif.ss.weekdays.nights <- abs(roll.ss.weekdays.nights - ss.weekdays.nights$Global_active_power)
anomalies.ss.weekdays.nights <- ss.weekdays.nights$Global_active_power[which(dif.ss.weekdays.nights > thresh)]

day_time.ss.weekdays.nights <- 1:length(ss.weekdays.nights$Global_active_power)
x.anomalies.ss.weekdays.nights <- ss.weekdays.nights$Time[which(dif.ss.weekdays.nights > thresh)]
plot(ss.weekdays.nights$Time,ss.weekdays.nights$Global_active_power,type="p", col="black", pch=20, main=paste("Spring Summer\nWeekdays Nights, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.ss.weekdays.nights, anomalies.ss.weekdays.nights, pch=20,col="red")
legend(20000,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


##weekends_ss

roll.ss.weekends.days <- rollapplyr(ss.weekends.days$Global_active_power, window, mean)
roll.ss.weekends.days <- c(rep(roll.ss.weekends.days[1],window-1),roll.ss.weekends.days)

roll.ss.weekends.evenings <- rollapplyr(ss.weekends.evenings$Global_active_power, window, mean)
roll.ss.weekends.evenings <- c(rep(roll.ss.weekends.evenings[1],window-1),roll.ss.weekends.evenings)

roll.ss.weekends.nights <- rollapplyr(ss.weekends.nights$Global_active_power, window, mean)
roll.ss.weekends.nights <- c(rep(roll.ss.weekends.nights[1],window-1),roll.ss.weekends.nights)


##anomalies weekends ss 

dif.ss.weekends.days <- abs(roll.ss.weekends.days - ss.weekends.days$Global_active_power)
anomalies.ss.weekends.days <- ss.weekends.days$Global_active_power[which(dif.ss.weekends.days > thresh)]

day_time.ss.weekends.days <- 1:length(ss.weekends.days$Global_active_power)
x.anomalies.ss.weekends.days <- ss.weekends.days$Time[which(dif.ss.weekends.days > thresh)]
plot(ss.weekends.days$Time,ss.weekends.days$Global_active_power,type="p", col="black", pch=20, main=paste("Spring Summer\nweekends Days, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.ss.weekends.days, anomalies.ss.weekends.days, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


dif.ss.weekends.evenings <- abs(roll.ss.weekends.evenings - ss.weekends.evenings$Global_active_power)
anomalies.ss.weekends.evenings <- ss.weekends.evenings$Global_active_power[which(dif.ss.weekends.evenings > thresh)]

day_time.ss.weekends.evenings <- 1:length(ss.weekends.evenings$Global_active_power)
x.anomalies.ss.weekends.evenings <- ss.weekends.evenings$Time[which(dif.ss.weekends.evenings > thresh)]
plot(ss.weekends.evenings$Time,ss.weekends.evenings$Global_active_power,type="p", col="black", pch=20, main=paste("Spring Summer\nweekends Evenings, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.ss.weekends.evenings, anomalies.ss.weekends.evenings, pch=20,col="red")
legend(1,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)


dif.ss.weekends.nights <- abs(roll.ss.weekends.nights - ss.weekends.nights$Global_active_power)
anomalies.ss.weekends.nights <- ss.weekends.nights$Global_active_power[which(dif.ss.weekends.nights > thresh)]

day_time.ss.weekends.nights <- 1:length(ss.weekends.nights$Global_active_power)
x.anomalies.ss.weekends.nights <- ss.weekends.nights$Time[which(dif.ss.weekends.nights > thresh)]
plot(ss.weekends.nights$Time,ss.weekends.nights$Global_active_power,type="p", col="black", pch=20, main=paste("Spring Summer\nweekends Nights, Threshold=",thresh), sub="Global Active Power rolling avg anomalies", xlab="Time", ylab="Global Active Power")
points(x.anomalies.ss.weekends.nights, anomalies.ss.weekends.nights, pch=20,col="red")
legend(20000,6, legend=c("Normal","Anomalies"), col=c("black","red"), pch=c(20,20), cex= 0.8)

