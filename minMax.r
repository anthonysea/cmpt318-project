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
