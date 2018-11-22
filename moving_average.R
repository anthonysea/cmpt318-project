library(lubridate)
library(depmixS4)
library(ggplot2)
library(scales)
library(zoo)
library(dplyr)
set.seed(1)

window <- 30
thresh <- 3.5

##weekdays_fw

roll.fw.weekdays.days <- rollapplyr(fw.weekdays.days$Global_active_power, window, mean)
roll.fw.weekdays.days <- c(rep(roll.fw.weekdays.days[1],window-1),roll.fw.weekdays.days)

roll.fw.weekdays.evenings <- rollapplyr(fw.weekdays.evenings$Global_active_power, window, mean)
roll.fw.weekdays.evenings <- c(rep(roll.fw.weekdays.evenings[1],window-1),roll.fw.weekdays.evenings)

roll.fw.weekdays.nights <- rollapplyr(fw.weekdays.nights$Global_active_power, window, mean)
roll.fw.weekdays.nights <- c(rep(roll.fw.weekdays.nights[1],window-1),roll.fw.weekdays.nights)

##anomaly detection fw weekdays
dif.fw.weekdays.days <- abs(roll.fw.weekdays.days - fw.weekdays.days$Global_active_power)
anomalies.fw.weekdays.days <- fw.weekdays.days$Global_active_power[which(dif.fw.weekdays.days > thresh)]

day_time.fw.weekdays.days <- 1:length(fw.weekdays.days$Global_active_power)
x.anomalies.fw.weekdays.days <- fw.weekdays.days$Time[which(dif.fw.weekdays.days > thresh)]

normal.fw.weekdays.days <- data.frame(Time = paste(today(), fw.weekdays.days$Time) %>% as_datetime(), Global_Active_Power = fw.weekdays.days$Global_active_power)
anom.fw.weekdays.days <- data.frame(Time2 = paste(today(), x.anomalies.fw.weekdays.days) %>% as_datetime(), Global_Active_Power = anomalies.fw.weekdays.days)

ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 8am-5pm Data = Training\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.fw.weekdays.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.fw.weekdays.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.fw.weekdays.evenings <- abs(roll.fw.weekdays.evenings - fw.weekdays.evenings$Global_active_power)
anomalies.fw.weekdays.evenings <- fw.weekdays.evenings$Global_active_power[which(dif.fw.weekdays.evenings > thresh)]

day_time.fw.weekdays.evenings <- 1:length(fw.weekdays.evenings$Global_active_power)
x.anomalies.fw.weekdays.evenings <- fw.weekdays.evenings$Time[which(dif.fw.weekdays.evenings > thresh)]

normal.fw.weekdays.evenings <- data.frame(Time = paste(today(), fw.weekdays.evenings$Time) %>% as_datetime(), Global_Active_Power = fw.weekdays.evenings$Global_active_power)
anom.fw.weekdays.evenings <- data.frame(Time2 = paste(today(), x.anomalies.fw.weekdays.evenings) %>% as_datetime(), Global_Active_Power = anomalies.fw.weekdays.evenings)

ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 5pm-12am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.fw.weekdays.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.fw.weekdays.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.fw.weekdays.nights <- abs(roll.fw.weekdays.nights - fw.weekdays.nights$Global_active_power)
anomalies.fw.weekdays.nights <- fw.weekdays.nights$Global_active_power[which(dif.fw.weekdays.nights > thresh)]

day_time.fw.weekdays.nights <- 1:length(fw.weekdays.nights$Global_active_power)
x.anomalies.fw.weekdays.nights <- fw.weekdays.nights$Time[which(dif.fw.weekdays.nights > thresh)]

normal.fw.weekdays.nights <- data.frame(Time = paste(today(), fw.weekdays.nights$Time) %>% as_datetime(), Global_Active_Power = fw.weekdays.nights$Global_active_power)
anom.fw.weekdays.nights <- data.frame(Time2 = paste(today(), x.anomalies.fw.weekdays.nights) %>% as_datetime(), Global_Active_Power = anomalies.fw.weekdays.nights)


ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 12am-8am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.fw.weekdays.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.fw.weekdays.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))
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

normal.fw.weekends.days <- data.frame(Time = paste(today(), fw.weekends.days$Time) %>% as_datetime(), Global_Active_Power = fw.weekends.days$Global_active_power)

anom.fw.weekends.days <- data.frame(Time2 = paste(today(), x.anomalies.fw.weekends.days) %>% as_datetime(), Global_Active_Power = anomalies.fw.weekends.days)


ggplot() +
  ggtitle(paste("Fall and Winter Weekends 8am-5pm Data = Training\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.fw.weekends.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.fw.weekends.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))


dif.fw.weekends.evenings <- abs(roll.fw.weekends.evenings - fw.weekends.evenings$Global_active_power)
anomalies.fw.weekends.evenings <- fw.weekends.evenings$Global_active_power[which(dif.fw.weekends.evenings > thresh)]

day_time.fw.weekends.evenings <- 1:length(fw.weekends.evenings$Global_active_power)
x.anomalies.fw.weekends.evenings <- fw.weekends.evenings$Time[which(dif.fw.weekends.evenings > thresh)]

normal.fw.weekends.evenings <- data.frame(Time = paste(today(), fw.weekends.evenings$Time) %>% as_datetime(), Global_Active_Power = fw.weekends.evenings$Global_active_power)

anom.fw.weekends.evenings <- data.frame(Time2 = paste(today(), x.anomalies.fw.weekends.evenings) %>% as_datetime(), Global_Active_Power = anomalies.fw.weekends.evenings)


ggplot() +
  ggtitle(paste("Fall and Winter Weekends 5pm-12am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.fw.weekends.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.fw.weekends.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.fw.weekends.nights <- abs(roll.fw.weekends.nights - fw.weekends.nights$Global_active_power)
anomalies.fw.weekends.nights <- fw.weekends.nights$Global_active_power[which(dif.fw.weekends.nights > thresh)]

day_time.fw.weekends.nights <- 1:length(fw.weekends.nights$Global_active_power)
x.anomalies.fw.weekends.nights <- fw.weekends.nights$Time[which(dif.fw.weekends.nights > thresh)]

normal.fw.weekends.nights <- data.frame(Time = paste(today(), fw.weekends.nights$Time) %>% as_datetime(), Global_Active_Power = fw.weekends.nights$Global_active_power)

anom.fw.weekends.nights <- data.frame(Time2 = paste(today(), x.anomalies.fw.weekends.nights) %>% as_datetime(), Global_Active_Power = anomalies.fw.weekends.nights)


ggplot() +
  ggtitle(paste("Fall and Winter Weekends 12am-8am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.fw.weekends.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.fw.weekends.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

##weekdays_ss

roll.ss.weekdays.days <- rollapplyr(ss.weekdays.days$Global_active_power, window, mean)
roll.ss.weekdays.days <- c(rep(roll.ss.weekdays.days[1],window-1),roll.ss.weekdays.days)

roll.ss.weekdays.evenings <- rollapplyr(ss.weekdays.evenings$Global_active_power, window, mean)
roll.ss.weekdays.evenings <- c(rep(roll.ss.weekdays.evenings[1],window-1),roll.ss.weekdays.evenings)

roll.ss.weekdays.nights <- rollapplyr(ss.weekdays.nights$Global_active_power, window, mean)
roll.ss.weekdays.nights <- c(rep(roll.ss.weekdays.nights[1],window-1),roll.ss.weekdays.nights)

##anomaly detection ss weekdays
dif.ss.weekdays.days <- abs(roll.ss.weekdays.days - ss.weekdays.days$Global_active_power)
anomalies.ss.weekdays.days <- ss.weekdays.days$Global_active_power[which(dif.ss.weekdays.days > thresh)]

day_time.ss.weekdays.days <- 1:length(ss.weekdays.days$Global_active_power)
x.anomalies.ss.weekdays.days <- ss.weekdays.days$Time[which(dif.ss.weekdays.days > thresh)]

normal.ss.weekdays.days <- data.frame(Time = paste(today(), ss.weekdays.days$Time) %>% as_datetime(), Global_Active_Power = ss.weekdays.days$Global_active_power)

anom.ss.weekdays.days <- data.frame(Time2 = paste(today(), x.anomalies.ss.weekdays.days) %>% as_datetime(), Global_Active_Power = anomalies.ss.weekdays.days)


ggplot() +
  ggtitle(paste("Spring Summer Weekdays 8am-5pm Data = Training\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.ss.weekdays.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.ss.weekdays.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))


dif.ss.weekdays.evenings <- abs(roll.ss.weekdays.evenings - ss.weekdays.evenings$Global_active_power)
anomalies.ss.weekdays.evenings <- ss.weekdays.evenings$Global_active_power[which(dif.ss.weekdays.evenings > thresh)]

day_time.ss.weekdays.evenings <- 1:length(ss.weekdays.evenings$Global_active_power)
x.anomalies.ss.weekdays.evenings <- ss.weekdays.evenings$Time[which(dif.ss.weekdays.evenings > thresh)]

normal.ss.weekdays.evenings <- data.frame(Time = paste(today(), ss.weekdays.evenings$Time) %>% as_datetime(), Global_Active_Power = ss.weekdays.evenings$Global_active_power)

anom.ss.weekdays.evenings <- data.frame(Time2 = paste(today(), x.anomalies.ss.weekdays.evenings) %>% as_datetime(), Global_Active_Power = anomalies.ss.weekdays.evenings)


ggplot() +
  ggtitle(paste("Spring & Summer Weekdays 5pm-12am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.ss.weekdays.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.ss.weekdays.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))



dif.ss.weekdays.nights <- abs(roll.ss.weekdays.nights - ss.weekdays.nights$Global_active_power)
anomalies.ss.weekdays.nights <- ss.weekdays.nights$Global_active_power[which(dif.ss.weekdays.nights > thresh)]

day_time.ss.weekdays.nights <- 1:length(ss.weekdays.nights$Global_active_power)
x.anomalies.ss.weekdays.nights <- ss.weekdays.nights$Time[which(dif.ss.weekdays.nights > thresh)]

normal.ss.weekdays.nights <- data.frame(Time = paste(today(), ss.weekdays.nights$Time) %>% as_datetime(), Global_Active_Power = ss.weekdays.nights$Global_active_power)

anom.ss.weekdays.nights <- data.frame(Time2 = paste(today(), x.anomalies.ss.weekdays.nights) %>% as_datetime(), Global_Active_Power = anomalies.ss.weekdays.nights)


ggplot() +
  ggtitle(paste("Spring & Summer Weekdays 12am-8am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.ss.weekdays.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.ss.weekdays.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

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

normal.ss.weekends.days <- data.frame(Time = paste(today(), ss.weekends.days$Time) %>% as_datetime(), Global_Active_Power = ss.weekends.days$Global_active_power)

anom.ss.weekends.days <- data.frame(Time2 = paste(today(), x.anomalies.ss.weekends.days) %>% as_datetime(), Global_Active_Power = anomalies.ss.weekends.days)


ggplot() +
  ggtitle(paste("Spring & Summer weekends 8am-5pm Data = Training\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.ss.weekends.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.ss.weekends.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))


dif.ss.weekends.evenings <- abs(roll.ss.weekends.evenings - ss.weekends.evenings$Global_active_power)
anomalies.ss.weekends.evenings <- ss.weekends.evenings$Global_active_power[which(dif.ss.weekends.evenings > thresh)]

day_time.ss.weekends.evenings <- 1:length(ss.weekends.evenings$Global_active_power)
x.anomalies.ss.weekends.evenings <- ss.weekends.evenings$Time[which(dif.ss.weekends.evenings > thresh)]

normal.ss.weekends.evenings <- data.frame(Time = paste(today(), ss.weekends.evenings$Time) %>% as_datetime(), Global_Active_Power = ss.weekends.evenings$Global_active_power)

anom.ss.weekends.evenings <- data.frame(Time2 = paste(today(), x.anomalies.ss.weekends.evenings) %>% as_datetime(), Global_Active_Power = anomalies.ss.weekends.evenings)


ggplot() +
  ggtitle(paste("Spring & Summer Weekends 5pm-12am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.ss.weekends.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.ss.weekends.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.ss.weekends.nights <- abs(roll.ss.weekends.nights - ss.weekends.nights$Global_active_power)
anomalies.ss.weekends.nights <- ss.weekends.nights$Global_active_power[which(dif.ss.weekends.nights > thresh)]

day_time.ss.weekends.nights <- 1:length(ss.weekends.nights$Global_active_power)
x.anomalies.ss.weekends.nights <- ss.weekends.nights$Time[which(dif.ss.weekends.nights > thresh)]

normal.ss.weekends.nights <- data.frame(Time = paste(today(), ss.weekends.nights$Time) %>% as_datetime(), Global_Active_Power = ss.weekends.nights$Global_active_power)

anom.ss.weekends.nights <- data.frame(Time2 = paste(today(), x.anomalies.ss.weekends.nights) %>% as_datetime(), Global_Active_Power = anomalies.ss.weekends.nights)


ggplot() +
  ggtitle(paste("Spring & Summer Weekends 12am-8am Data = Training\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.ss.weekends.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.ss.weekends.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))
##=====================================================
## Test 3
##=====================================================
## t3.fw.weekdays

roll.t3.fw.weekdays.days <- rollapplyr(t3.fw.weekdays.days$Global_active_power, window, mean)
roll.t3.fw.weekdays.days <- c(rep(roll.t3.fw.weekdays.days[1],window-1),roll.t3.fw.weekdays.days)

roll.t3.fw.weekdays.evenings <- rollapplyr(t3.fw.weekdays.evenings$Global_active_power, window, mean)
roll.t3.fw.weekdays.evenings <- c(rep(roll.t3.fw.weekdays.evenings[1],window-1),roll.t3.fw.weekdays.evenings)

roll.t3.fw.weekdays.nights <- rollapplyr(t3.fw.weekdays.nights$Global_active_power, window, mean)
roll.t3.fw.weekdays.nights <- c(rep(roll.t3.fw.weekdays.nights[1],window-1),roll.t3.fw.weekdays.nights)

##========
## t3.fw.weekdays anomaly detection
thresh_t3 <- 3.5

dif.t3.fw.weekdays.days <- abs(roll.t3.fw.weekdays.days - t3.fw.weekdays.days$Global_active_power)
anomalies.t3.fw.weekdays.days <- t3.fw.weekdays.days$Global_active_power[which(dif.t3.fw.weekdays.days > thresh_t3)]

day_time.t3.fw.weekdays.days <- 1:length(t3.fw.weekdays.days$Global_active_power)
x.anomalies.t3.fw.weekdays.days <- t3.fw.weekdays.days$Time[which(dif.t3.fw.weekdays.days > thresh_t3)]

normal.t3.fw.weekdays.days <- data.frame(Time = paste(today(), t3.fw.weekdays.days$Time) %>% as_datetime(), Global_Active_Power = t3.fw.weekdays.days$Global_active_power)
anom.t3.fw.weekdays.days <- data.frame(Time2 = paste(today(), x.anomalies.t3.fw.weekdays.days) %>% as_datetime(), Global_Active_Power = anomalies.t3.fw.weekdays.days)

ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 8am-5pm Data = Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.fw.weekdays.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.fw.weekdays.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.fw.weekdays.evenings <- abs(roll.t3.fw.weekdays.evenings - t3.fw.weekdays.evenings$Global_active_power)
anomalies.t3.fw.weekdays.evenings <- t3.fw.weekdays.evenings$Global_active_power[which(dif.t3.fw.weekdays.evenings > thresh_t3)]

day_time.t3.fw.weekdays.evenings <- 1:length(t3.fw.weekdays.evenings$Global_active_power)
x.anomalies.t3.fw.weekdays.evenings <- t3.fw.weekdays.evenings$Time[which(dif.t3.fw.weekdays.evenings > thresh_t3)]

normal.t3.fw.weekdays.evenings <- data.frame(Time = paste(today(), t3.fw.weekdays.evenings$Time) %>% as_datetime(), Global_Active_Power = t3.fw.weekdays.evenings$Global_active_power)
anom.t3.fw.weekdays.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t3.fw.weekdays.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t3.fw.weekdays.evenings)

ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 5pm-12am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.fw.weekdays.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.fw.weekdays.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.fw.weekdays.nights <- abs(roll.t3.fw.weekdays.nights - t3.fw.weekdays.nights$Global_active_power)
anomalies.t3.fw.weekdays.nights <- t3.fw.weekdays.nights$Global_active_power[which(dif.t3.fw.weekdays.nights > thresh_t3)]

day_time.t3.fw.weekdays.nights <- 1:length(t3.fw.weekdays.nights$Global_active_power)
x.anomalies.t3.fw.weekdays.nights <- t3.fw.weekdays.nights$Time[which(dif.t3.fw.weekdays.nights > thresh_t3)]

normal.t3.fw.weekdays.nights <- data.frame(Time = paste(today(), t3.fw.weekdays.nights$Time) %>% as_datetime(), Global_Active_Power = t3.fw.weekdays.nights$Global_active_power)
anom.t3.fw.weekdays.nights <- data.frame(Time2 = paste(today(), x.anomalies.t3.fw.weekdays.nights) %>% as_datetime(), Global_Active_Power = anomalies.t3.fw.weekdays.nights)


ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 12am-8am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.fw.weekdays.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.fw.weekdays.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))
##=== weekends test3 fw

roll.t3.fw.weekends.days <- rollapplyr(t3.fw.weekends.days$Global_active_power, window, mean)
roll.t3.fw.weekends.days <- c(rep(roll.t3.fw.weekends.days[1],window-1),roll.t3.fw.weekends.days)

roll.t3.fw.weekends.evenings <- rollapplyr(t3.fw.weekends.evenings$Global_active_power, window, mean)
roll.t3.fw.weekends.evenings <- c(rep(roll.t3.fw.weekends.evenings[1],window-1),roll.t3.fw.weekends.evenings)

roll.t3.fw.weekends.nights <- rollapplyr(t3.fw.weekends.nights$Global_active_power, window, mean)
roll.t3.fw.weekends.nights <- c(rep(roll.t3.fw.weekends.nights[1],window-1),roll.t3.fw.weekends.nights)
##anomalies weekends

dif.t3.fw.weekends.days <- abs(roll.t3.fw.weekends.days - t3.fw.weekends.days$Global_active_power)
anomalies.t3.fw.weekends.days <- t3.fw.weekends.days$Global_active_power[which(dif.t3.fw.weekends.days > thresh_t3)]

day_time.t3.fw.weekends.days <- 1:length(t3.fw.weekends.days$Global_active_power)
x.anomalies.t3.fw.weekends.days <- t3.fw.weekends.days$Time[which(dif.t3.fw.weekends.days > thresh_t3)]

normal.t3.fw.weekends.days <- data.frame(Time = paste(today(), t3.fw.weekends.days$Time) %>% as_datetime(), Global_Active_Power = t3.fw.weekends.days$Global_active_power)
anom.t3.fw.weekends.days <- data.frame(Time2 = paste(today(), x.anomalies.t3.fw.weekends.days) %>% as_datetime(), Global_Active_Power = anomalies.t3.fw.weekends.days)

ggplot() +
  ggtitle(paste("Fall and Winter Weekends 8am-5pm Data = Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.fw.weekends.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.fw.weekends.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.fw.weekends.evenings <- abs(roll.t3.fw.weekends.evenings - t3.fw.weekends.evenings$Global_active_power)
anomalies.t3.fw.weekends.evenings <- t3.fw.weekends.evenings$Global_active_power[which(dif.t3.fw.weekends.evenings > thresh_t3)]

day_time.t3.fw.weekends.evenings <- 1:length(t3.fw.weekends.evenings$Global_active_power)
x.anomalies.t3.fw.weekends.evenings <- t3.fw.weekends.evenings$Time[which(dif.t3.fw.weekends.evenings > thresh_t3)]

normal.t3.fw.weekends.evenings <- data.frame(Time = paste(today(), t3.fw.weekends.evenings$Time) %>% as_datetime(), Global_Active_Power = t3.fw.weekends.evenings$Global_active_power)
anom.t3.fw.weekends.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t3.fw.weekends.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t3.fw.weekends.evenings)

ggplot() +
  ggtitle(paste("Fall and Winter Weekends 5pm-12am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.fw.weekends.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.fw.weekends.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.fw.weekends.nights <- abs(roll.t3.fw.weekends.nights - t3.fw.weekends.nights$Global_active_power)
anomalies.t3.fw.weekends.nights <- t3.fw.weekends.nights$Global_active_power[which(dif.t3.fw.weekends.nights > thresh_t3)]

day_time.t3.fw.weekends.nights <- 1:length(t3.fw.weekends.nights$Global_active_power)
x.anomalies.t3.fw.weekends.nights <- t3.fw.weekends.nights$Time[which(dif.t3.fw.weekends.nights > thresh_t3)]

normal.t3.fw.weekends.nights <- data.frame(Time = paste(today(), t3.fw.weekends.nights$Time) %>% as_datetime(), Global_Active_Power = t3.fw.weekends.nights$Global_active_power)
anom.t3.fw.weekends.nights <- data.frame(Time2 = paste(today(), x.anomalies.t3.fw.weekends.nights) %>% as_datetime(), Global_Active_Power = anomalies.t3.fw.weekends.nights)


ggplot() +
  ggtitle(paste("Fall and Winter Weekends 12am-8am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.fw.weekends.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.fw.weekends.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

##weekdays t3 ss
roll.t3.ss.weekdays.days <- rollapplyr(t3.ss.weekdays.days$Global_active_power, window, mean)
roll.t3.ss.weekdays.days <- c(rep(roll.t3.ss.weekdays.days[1],window-1),roll.t3.ss.weekdays.days)

roll.t3.ss.weekdays.evenings <- rollapplyr(t3.ss.weekdays.evenings$Global_active_power, window, mean)
roll.t3.ss.weekdays.evenings <- c(rep(roll.t3.ss.weekdays.evenings[1],window-1),roll.t3.ss.weekdays.evenings)

roll.t3.ss.weekdays.nights <- rollapplyr(t3.ss.weekdays.nights$Global_active_power, window, mean)
roll.t3.ss.weekdays.nights <- c(rep(roll.t3.ss.weekdays.nights[1],window-1),roll.t3.ss.weekdays.nights)

##anomaly detection aa weekdays
dif.t3.ss.weekdays.days <- abs(roll.t3.ss.weekdays.days - t3.ss.weekdays.days$Global_active_power)
anomalies.t3.ss.weekdays.days <- t3.ss.weekdays.days$Global_active_power[which(dif.t3.ss.weekdays.days > thresh_t3)]

day_time.t3.ss.weekdays.days <- 1:length(t3.ss.weekdays.days$Global_active_power)
x.anomalies.t3.ss.weekdays.days <- t3.ss.weekdays.days$Time[which(dif.t3.ss.weekdays.days > thresh_t3)]

normal.t3.ss.weekdays.days <- data.frame(Time = paste(today(), t3.ss.weekdays.days$Time) %>% as_datetime(), Global_Active_Power = t3.ss.weekdays.days$Global_active_power)
anom.t3.ss.weekdays.days <- data.frame(Time2 = paste(today(), x.anomalies.t3.ss.weekdays.days) %>% as_datetime(), Global_Active_Power = anomalies.t3.ss.weekdays.days)

ggplot() +
  ggtitle(paste("Spring & Summer Weekdays 8am-5pm Data = Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.ss.weekdays.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.ss.weekdays.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.ss.weekdays.evenings <- abs(roll.t3.ss.weekdays.evenings - t3.ss.weekdays.evenings$Global_active_power)
anomalies.t3.ss.weekdays.evenings <- t3.ss.weekdays.evenings$Global_active_power[which(dif.t3.ss.weekdays.evenings > thresh_t3)]

day_time.t3.ss.weekdays.evenings <- 1:length(t3.ss.weekdays.evenings$Global_active_power)
x.anomalies.t3.ss.weekdays.evenings <- t3.ss.weekdays.evenings$Time[which(dif.t3.ss.weekdays.evenings > thresh_t3)]

normal.t3.ss.weekdays.evenings <- data.frame(Time = paste(today(), t3.ss.weekdays.evenings$Time) %>% as_datetime(), Global_Active_Power = t3.ss.weekdays.evenings$Global_active_power)
anom.t3.ss.weekdays.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t3.ss.weekdays.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t3.ss.weekdays.evenings)

ggplot() +
  ggtitle(paste("Spring & Summer Weekdays 5pm-12am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.ss.weekdays.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.ss.weekdays.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.ss.weekdays.nights <- abs(roll.t3.ss.weekdays.nights - t3.ss.weekdays.nights$Global_active_power)
anomalies.t3.ss.weekdays.nights <- t3.ss.weekdays.nights$Global_active_power[which(dif.t3.ss.weekdays.nights > thresh_t3)]

day_time.t3.ss.weekdays.nights <- 1:length(t3.ss.weekdays.nights$Global_active_power)
x.anomalies.t3.ss.weekdays.nights <- t3.ss.weekdays.nights$Time[which(dif.t3.ss.weekdays.nights > thresh_t3)]

normal.t3.ss.weekdays.nights <- data.frame(Time = paste(today(), t3.ss.weekdays.nights$Time) %>% as_datetime(), Global_Active_Power = t3.ss.weekdays.nights$Global_active_power)
anom.t3.ss.weekdays.nights <- data.frame(Time2 = paste(today(), x.anomalies.t3.ss.weekdays.nights) %>% as_datetime(), Global_Active_Power = anomalies.t3.ss.weekdays.nights)


ggplot() +
  ggtitle(paste("Spring & Summer Weekdays 12am-8am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.ss.weekdays.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.ss.weekdays.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

##==weekends t3 ss 

roll.t3.ss.weekends.days <- rollapplyr(t3.ss.weekends.days$Global_active_power, window, mean)
roll.t3.ss.weekends.days <- c(rep(roll.t3.ss.weekends.days[1],window-1),roll.t3.ss.weekends.days)

roll.t3.ss.weekends.evenings <- rollapplyr(t3.ss.weekends.evenings$Global_active_power, window, mean)
roll.t3.ss.weekends.evenings <- c(rep(roll.t3.ss.weekends.evenings[1],window-1),roll.t3.ss.weekends.evenings)

roll.t3.ss.weekends.nights <- rollapplyr(t3.ss.weekends.nights$Global_active_power, window, mean)
roll.t3.ss.weekends.nights <- c(rep(roll.t3.ss.weekends.nights[1],window-1),roll.t3.ss.weekends.nights)


##anomalies weekends t3.ss 

dif.t3.ss.weekends.days <- abs(roll.t3.ss.weekends.days - t3.ss.weekends.days$Global_active_power)
anomalies.t3.ss.weekends.days <- t3.ss.weekends.days$Global_active_power[which(dif.t3.ss.weekends.days > thresh_t3)]

day_time.t3.ss.weekends.days <- 1:length(t3.ss.weekends.days$Global_active_power)
x.anomalies.t3.ss.weekends.days <- t3.ss.weekends.days$Time[which(dif.t3.ss.weekends.days > thresh_t3)]

normal.t3.ss.weekends.days <- data.frame(Time = paste(today(), t3.ss.weekends.days$Time) %>% as_datetime(), Global_Active_Power = t3.ss.weekends.days$Global_active_power)
anom.t3.ss.weekends.days <- data.frame(Time2 = paste(today(), x.anomalies.t3.ss.weekends.days) %>% as_datetime(), Global_Active_Power = anomalies.t3.ss.weekends.days)

ggplot() +
  ggtitle(paste("Spring & Summer Weekends 8am-5pm Data = Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.ss.weekends.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.ss.weekends.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.ss.weekends.evenings <- abs(roll.t3.ss.weekends.evenings - t3.ss.weekends.evenings$Global_active_power)
anomalies.t3.ss.weekends.evenings <- t3.ss.weekends.evenings$Global_active_power[which(dif.t3.ss.weekends.evenings > thresh_t3)]

day_time.t3.ss.weekends.evenings <- 1:length(t3.ss.weekends.evenings$Global_active_power)
x.anomalies.t3.ss.weekends.evenings <- t3.ss.weekends.evenings$Time[which(dif.t3.ss.weekends.evenings > thresh_t3)]

normal.t3.ss.weekends.evenings <- data.frame(Time = paste(today(), t3.ss.weekends.evenings$Time) %>% as_datetime(), Global_Active_Power = t3.ss.weekends.evenings$Global_active_power)
anom.t3.ss.weekends.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t3.ss.weekends.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t3.ss.weekends.evenings)

ggplot() +
  ggtitle(paste("Spring & Summer Weekends 5pm-12am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.ss.weekends.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.ss.weekends.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t3.ss.weekends.nights <- abs(roll.t3.ss.weekends.nights - t3.ss.weekends.nights$Global_active_power)
anomalies.t3.ss.weekends.nights <- t3.ss.weekends.nights$Global_active_power[which(dif.t3.ss.weekends.nights > thresh_t3)]

day_time.t3.ss.weekends.nights <- 1:length(t3.ss.weekends.nights$Global_active_power)
x.anomalies.t3.ss.weekends.nights <- t3.ss.weekends.nights$Time[which(dif.t3.ss.weekends.nights > thresh_t3)]

normal.t3.ss.weekends.nights <- data.frame(Time = paste(today(), t3.ss.weekends.nights$Time) %>% as_datetime(), Global_Active_Power = t3.ss.weekends.nights$Global_active_power)
anom.t3.ss.weekends.nights <- data.frame(Time2 = paste(today(), x.anomalies.t3.ss.weekends.nights) %>% as_datetime(), Global_Active_Power = anomalies.t3.ss.weekends.nights)


ggplot() +
  ggtitle(paste("Spring & Summer Weekends 12am-8am Data= Test3\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t3)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t3.ss.weekends.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t3.ss.weekends.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))
##================
## Test 5 rolling avgs 
#============

roll.t5.fw.weekdays.days <- rollapplyr(t5.fw.weekdays.days$Global_active_power, window, mean)
roll.t5.fw.weekdays.days <- c(rep(roll.t5.fw.weekdays.days[1],window-1),roll.t5.fw.weekdays.days)

roll.t5.fw.weekdays.evenings <- rollapplyr(t5.fw.weekdays.evenings$Global_active_power, window, mean)
roll.t5.fw.weekdays.evenings <- c(rep(roll.t5.fw.weekdays.evenings[1],window-1),roll.t5.fw.weekdays.evenings)

roll.t5.fw.weekdays.nights <- rollapplyr(t5.fw.weekdays.nights$Global_active_power, window, mean)
roll.t5.fw.weekdays.nights <- c(rep(roll.t5.fw.weekdays.nights[1],window-1),roll.t5.fw.weekdays.nights)

##========
## t5.fw.weekdays anomaly detection
thresh_t5 <- 10

dif.t5.fw.weekdays.days <- abs(roll.t5.fw.weekdays.days - t5.fw.weekdays.days$Global_active_power)
anomalies.t5.fw.weekdays.days <- t5.fw.weekdays.days$Global_active_power[which(dif.t5.fw.weekdays.days > thresh_t5)]

day_time.t5.fw.weekdays.days <- 1:length(t5.fw.weekdays.days$Global_active_power)
x.anomalies.t5.fw.weekdays.days <- t5.fw.weekdays.days$Time[which(dif.t5.fw.weekdays.days > thresh_t5)]

normal.t5.fw.weekdays.days <- data.frame(Time = paste(today(), t5.fw.weekdays.days$Time) %>% as_datetime(), Global_Active_Power = t5.fw.weekdays.days$Global_active_power)
anom.t5.fw.weekdays.days <- data.frame(Time2 = paste(today(), x.anomalies.t5.fw.weekdays.days) %>% as_datetime(), Global_Active_Power = anomalies.t5.fw.weekdays.days)

ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 8am-5pm Data = Test5\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.fw.weekdays.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.fw.weekdays.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.fw.weekdays.evenings <- abs(roll.t5.fw.weekdays.evenings - t5.fw.weekdays.evenings$Global_active_power)
anomalies.t5.fw.weekdays.evenings <- t5.fw.weekdays.evenings$Global_active_power[which(dif.t5.fw.weekdays.evenings > thresh_t5)]

day_time.t5.fw.weekdays.evenings <- 1:length(t5.fw.weekdays.evenings$Global_active_power)
x.anomalies.t5.fw.weekdays.evenings <- t5.fw.weekdays.evenings$Time[which(dif.t5.fw.weekdays.evenings > thresh_t5)]

normal.t5.fw.weekdays.evenings <- data.frame(Time = paste(today(), t5.fw.weekdays.evenings$Time) %>% as_datetime(), Global_Active_Power = t5.fw.weekdays.evenings$Global_active_power)
anom.t5.fw.weekdays.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t5.fw.weekdays.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t5.fw.weekdays.evenings)

ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 5pm-12am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.fw.weekdays.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.fw.weekdays.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.fw.weekdays.nights <- abs(roll.t5.fw.weekdays.nights - t5.fw.weekdays.nights$Global_active_power)
anomalies.t5.fw.weekdays.nights <- t5.fw.weekdays.nights$Global_active_power[which(dif.t5.fw.weekdays.nights > thresh_t5)]

day_time.t5.fw.weekdays.nights <- 1:length(t5.fw.weekdays.nights$Global_active_power)
x.anomalies.t5.fw.weekdays.nights <- t5.fw.weekdays.nights$Time[which(dif.t5.fw.weekdays.nights > thresh_t5)]

normal.t5.fw.weekdays.nights <- data.frame(Time = paste(today(), t5.fw.weekdays.nights$Time) %>% as_datetime(), Global_Active_Power = t5.fw.weekdays.nights$Global_active_power)
anom.t5.fw.weekdays.nights <- data.frame(Time2 = paste(today(), x.anomalies.t5.fw.weekdays.nights) %>% as_datetime(), Global_Active_Power = anomalies.t5.fw.weekdays.nights)


ggplot() +
  ggtitle(paste("Fall and Winter Weekdays 12am-8am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.fw.weekdays.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.fw.weekdays.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))
##=== weekends test5 fw

roll.t5.fw.weekends.days <- rollapplyr(t5.fw.weekends.days$Global_active_power, window, mean)
roll.t5.fw.weekends.days <- c(rep(roll.t5.fw.weekends.days[1],window-1),roll.t5.fw.weekends.days)

roll.t5.fw.weekends.evenings <- rollapplyr(t5.fw.weekends.evenings$Global_active_power, window, mean)
roll.t5.fw.weekends.evenings <- c(rep(roll.t5.fw.weekends.evenings[1],window-1),roll.t5.fw.weekends.evenings)

roll.t5.fw.weekends.nights <- rollapplyr(t5.fw.weekends.nights$Global_active_power, window, mean)
roll.t5.fw.weekends.nights <- c(rep(roll.t5.fw.weekends.nights[1],window-1),roll.t5.fw.weekends.nights)
##anomalies weekends

dif.t5.fw.weekends.days <- abs(roll.t5.fw.weekends.days - t5.fw.weekends.days$Global_active_power)
anomalies.t5.fw.weekends.days <- t5.fw.weekends.days$Global_active_power[which(dif.t5.fw.weekends.days > thresh_t5)]

day_time.t5.fw.weekends.days <- 1:length(t5.fw.weekends.days$Global_active_power)
x.anomalies.t5.fw.weekends.days <- t5.fw.weekends.days$Time[which(dif.t5.fw.weekends.days > thresh_t5)]

normal.t5.fw.weekends.days <- data.frame(Time = paste(today(), t5.fw.weekends.days$Time) %>% as_datetime(), Global_Active_Power = t5.fw.weekends.days$Global_active_power)
anom.t5.fw.weekends.days <- data.frame(Time2 = paste(today(), x.anomalies.t5.fw.weekends.days) %>% as_datetime(), Global_Active_Power = anomalies.t5.fw.weekends.days)

ggplot() +
  ggtitle(paste("Fall and Winter Weekends 8am-5pm Data = Test5\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.fw.weekends.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.fw.weekends.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.fw.weekends.evenings <- abs(roll.t5.fw.weekends.evenings - t5.fw.weekends.evenings$Global_active_power)
anomalies.t5.fw.weekends.evenings <- t5.fw.weekends.evenings$Global_active_power[which(dif.t5.fw.weekends.evenings > thresh_t5)]

day_time.t5.fw.weekends.evenings <- 1:length(t5.fw.weekends.evenings$Global_active_power)
x.anomalies.t5.fw.weekends.evenings <- t5.fw.weekends.evenings$Time[which(dif.t5.fw.weekends.evenings > thresh_t5)]

normal.t5.fw.weekends.evenings <- data.frame(Time = paste(today(), t5.fw.weekends.evenings$Time) %>% as_datetime(), Global_Active_Power = t5.fw.weekends.evenings$Global_active_power)
anom.t5.fw.weekends.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t5.fw.weekends.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t5.fw.weekends.evenings)

ggplot() +
  ggtitle(paste("Fall and Winter Weekends 5pm-12am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.fw.weekends.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.fw.weekends.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.fw.weekends.nights <- abs(roll.t5.fw.weekends.nights - t5.fw.weekends.nights$Global_active_power)
anomalies.t5.fw.weekends.nights <- t5.fw.weekends.nights$Global_active_power[which(dif.t5.fw.weekends.nights > thresh_t5)]

day_time.t5.fw.weekends.nights <- 1:length(t5.fw.weekends.nights$Global_active_power)
x.anomalies.t5.fw.weekends.nights <- t5.fw.weekends.nights$Time[which(dif.t5.fw.weekends.nights > thresh_t5)]

normal.t5.fw.weekends.nights <- data.frame(Time = paste(today(), t5.fw.weekends.nights$Time) %>% as_datetime(), Global_Active_Power = t5.fw.weekends.nights$Global_active_power)
anom.t5.fw.weekends.nights <- data.frame(Time2 = paste(today(), x.anomalies.t5.fw.weekends.nights) %>% as_datetime(), Global_Active_Power = anomalies.t5.fw.weekends.nights)


ggplot() +
  ggtitle(paste("Fall and Winter Weekends 12am-8am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.fw.weekends.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.fw.weekends.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))
##weekdays t5 ss


roll.t5.ss.weekdays.days <- rollapplyr(t5.ss.weekdays.days$Global_active_power, window, mean)
roll.t5.ss.weekdays.days <- c(rep(roll.t5.ss.weekdays.days[1],window-1),roll.t5.ss.weekdays.days)

roll.t5.ss.weekdays.evenings <- rollapplyr(t5.ss.weekdays.evenings$Global_active_power, window, mean)
roll.t5.ss.weekdays.evenings <- c(rep(roll.t5.ss.weekdays.evenings[1],window-1),roll.t5.ss.weekdays.evenings)

roll.t5.ss.weekdays.nights <- rollapplyr(t5.ss.weekdays.nights$Global_active_power, window, mean)
roll.t5.ss.weekdays.nights <- c(rep(roll.t5.ss.weekdays.nights[1],window-1),roll.t5.ss.weekdays.nights)

##anomaly detection aa weekdays
dif.t5.ss.weekdays.days <- abs(roll.t5.ss.weekdays.days - t5.ss.weekdays.days$Global_active_power)
anomalies.t5.ss.weekdays.days <- t5.ss.weekdays.days$Global_active_power[which(dif.t5.ss.weekdays.days > thresh_t5)]

day_time.t5.ss.weekdays.days <- 1:length(t5.ss.weekdays.days$Global_active_power)
x.anomalies.t5.ss.weekdays.days <- t5.ss.weekdays.days$Time[which(dif.t5.ss.weekdays.days > thresh_t5)]

normal.t5.ss.weekdays.days <- data.frame(Time = paste(today(), t5.ss.weekdays.days$Time) %>% as_datetime(), Global_Active_Power = t5.ss.weekdays.days$Global_active_power)
anom.t5.ss.weekdays.days <- data.frame(Time2 = paste(today(), x.anomalies.t5.ss.weekdays.days) %>% as_datetime(), Global_Active_Power = anomalies.t5.ss.weekdays.days)

ggplot() +
  ggtitle(paste("Summer & Spring Weekdays 8am-5pm Data = Test5\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.ss.weekdays.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.ss.weekdays.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.ss.weekdays.evenings <- abs(roll.t5.ss.weekdays.evenings - t5.ss.weekdays.evenings$Global_active_power)
anomalies.t5.ss.weekdays.evenings <- t5.ss.weekdays.evenings$Global_active_power[which(dif.t5.ss.weekdays.evenings > thresh_t5)]

day_time.t5.ss.weekdays.evenings <- 1:length(t5.ss.weekdays.evenings$Global_active_power)
x.anomalies.t5.ss.weekdays.evenings <- t5.ss.weekdays.evenings$Time[which(dif.t5.ss.weekdays.evenings > thresh_t5)]

normal.t5.ss.weekdays.evenings <- data.frame(Time = paste(today(), t5.ss.weekdays.evenings$Time) %>% as_datetime(), Global_Active_Power = t5.ss.weekdays.evenings$Global_active_power)
anom.t5.ss.weekdays.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t5.ss.weekdays.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t5.ss.weekdays.evenings)

ggplot() +
  ggtitle(paste("Summer & Spring Weekdays 5pm-12am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.ss.weekdays.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.ss.weekdays.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.ss.weekdays.nights <- abs(roll.t5.ss.weekdays.nights - t5.ss.weekdays.nights$Global_active_power)
anomalies.t5.ss.weekdays.nights <- t5.ss.weekdays.nights$Global_active_power[which(dif.t5.ss.weekdays.nights > thresh_t5)]

day_time.t5.ss.weekdays.nights <- 1:length(t5.ss.weekdays.nights$Global_active_power)
x.anomalies.t5.ss.weekdays.nights <- t5.ss.weekdays.nights$Time[which(dif.t5.ss.weekdays.nights > thresh_t5)]

normal.t5.ss.weekdays.nights <- data.frame(Time = paste(today(), t5.ss.weekdays.nights$Time) %>% as_datetime(), Global_Active_Power = t5.ss.weekdays.nights$Global_active_power)
anom.t5.ss.weekdays.nights <- data.frame(Time2 = paste(today(), x.anomalies.t5.ss.weekdays.nights) %>% as_datetime(), Global_Active_Power = anomalies.t5.ss.weekdays.nights)


ggplot() +
  ggtitle(paste("Summer & Spring Weekdays 12am-8am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.ss.weekdays.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.ss.weekdays.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

##==weekends t5 ss 

roll.t5.ss.weekends.days <- rollapplyr(t5.ss.weekends.days$Global_active_power, window, mean)
roll.t5.ss.weekends.days <- c(rep(roll.t5.ss.weekends.days[1],window-1),roll.t5.ss.weekends.days)

roll.t5.ss.weekends.evenings <- rollapplyr(t5.ss.weekends.evenings$Global_active_power, window, mean)
roll.t5.ss.weekends.evenings <- c(rep(roll.t5.ss.weekends.evenings[1],window-1),roll.t5.ss.weekends.evenings)

roll.t5.ss.weekends.nights <- rollapplyr(t5.ss.weekends.nights$Global_active_power, window, mean)
roll.t5.ss.weekends.nights <- c(rep(roll.t5.ss.weekends.nights[1],window-1),roll.t5.ss.weekends.nights)


##anomalies weekends t5.ss 

dif.t5.ss.weekends.days <- abs(roll.t5.ss.weekends.days - t5.ss.weekends.days$Global_active_power)
anomalies.t5.ss.weekends.days <- t5.ss.weekends.days$Global_active_power[which(dif.t5.ss.weekends.days > thresh_t5)]

day_time.t5.ss.weekends.days <- 1:length(t5.ss.weekends.days$Global_active_power)
x.anomalies.t5.ss.weekends.days <- t5.ss.weekends.days$Time[which(dif.t5.ss.weekends.days > thresh_t5)]

normal.t5.ss.weekends.days <- data.frame(Time = paste(today(), t5.ss.weekends.days$Time) %>% as_datetime(), Global_Active_Power = t5.ss.weekends.days$Global_active_power)
anom.t5.ss.weekends.days <- data.frame(Time2 = paste(today(), x.anomalies.t5.ss.weekends.days) %>% as_datetime(), Global_Active_Power = anomalies.t5.ss.weekends.days)

ggplot() +
  ggtitle(paste("Summer & Spring Weekends 8am-5pm Data = Test5\n Window: ",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.ss.weekends.days, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.ss.weekends.days, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.ss.weekends.evenings <- abs(roll.t5.ss.weekends.evenings - t5.ss.weekends.evenings$Global_active_power)
anomalies.t5.ss.weekends.evenings <- t5.ss.weekends.evenings$Global_active_power[which(dif.t5.ss.weekends.evenings > thresh_t5)]

day_time.t5.ss.weekends.evenings <- 1:length(t5.ss.weekends.evenings$Global_active_power)
x.anomalies.t5.ss.weekends.evenings <- t5.ss.weekends.evenings$Time[which(dif.t5.ss.weekends.evenings > thresh_t5)]

normal.t5.ss.weekends.evenings <- data.frame(Time = paste(today(), t5.ss.weekends.evenings$Time) %>% as_datetime(), Global_Active_Power = t5.ss.weekends.evenings$Global_active_power)
anom.t5.ss.weekends.evenings <- data.frame(Time2 = paste(today(), x.anomalies.t5.ss.weekends.evenings) %>% as_datetime(), Global_Active_Power = anomalies.t5.ss.weekends.evenings)

ggplot() +
  ggtitle(paste("Summer & Spring Weekends 5pm-12am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.ss.weekends.evenings, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.ss.weekends.evenings, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

dif.t5.ss.weekends.nights <- abs(roll.t5.ss.weekends.nights - t5.ss.weekends.nights$Global_active_power)
anomalies.t5.ss.weekends.nights <- t5.ss.weekends.nights$Global_active_power[which(dif.t5.ss.weekends.nights > thresh_t5)]

day_time.t5.ss.weekends.nights <- 1:length(t5.ss.weekends.nights$Global_active_power)
x.anomalies.t5.ss.weekends.nights <- t5.ss.weekends.nights$Time[which(dif.t5.ss.weekends.nights > thresh_t5)]

normal.t5.ss.weekends.nights <- data.frame(Time = paste(today(), t5.ss.weekends.nights$Time) %>% as_datetime(), Global_Active_Power = t5.ss.weekends.nights$Global_active_power)
anom.t5.ss.weekends.nights <- data.frame(Time2 = paste(today(), x.anomalies.t5.ss.weekends.nights) %>% as_datetime(), Global_Active_Power = anomalies.t5.ss.weekends.nights)


ggplot() +
  ggtitle(paste("Summer & Spring Weekends 12am-8am Data = Test5\n Window:",window,"min, Rolling Avg Abs Difference Threshold = ", thresh_t5)) +
  ylab("Global Active Power") +
  xlab("Time (Hours:Minutes)") +
  geom_point(data=normal.t5.ss.weekends.nights, aes(Time,Global_Active_Power, color="Normal")) +
  geom_point(data=anom.t5.ss.weekends.nights, aes(Time2,Global_Active_Power, color="Anomalies")) +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M") +
  scale_color_manual(name="Legend",values=c('red','green'))

