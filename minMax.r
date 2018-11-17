##Test file for testing data sets
library(lubridate)
library(depmixS4)
library(ggplot2)
library(zoo)
set.seed(1)

##Phase 2 approach 1
##Out of Range

#CHRISTMAS
min.christmas.afternoons <- min(christmas.afternoons$Global_active_power, na.rm=T)
max.christmas.afternoons <- max(christmas.afternoons$Global_active_power, na.rm=T)

min.christmas.mornings <- min(christmas.mornings$Global_active_power, na.rm=T)
max.christmas.mornings <- max(christmas.mornings$Global_active_power, na.rm=T)

min.christmas.evenings <- min(christmas.evenings$Global_active_power, na.rm=T)
max.christmas.evenings <- max(christmas.evenings$Global_active_power, na.rm=T)

min.christmas.nights <- min(christmas.nights$Global_active_power, na.rm=T)
max.christmas.nights <- max(christmas.nights$Global_active_power, na.rm=T)

#fall.weekdays
min.fall.weekdays.afternoons <- min(fall.weekdays.afternoons$Global_active_power, na.rm=T)
max.fall.weekdays.afternoons <- max(fall.weekdays.afternoons$Global_active_power, na.rm=T)

min.fall.weekdays.mornings <- min(fall.weekdays.mornings$Global_active_power, na.rm=T)
max.fall.weekdays.mornings <- max(fall.weekdays.mornings$Global_active_power, na.rm=T)

min.fall.weekdays.evenings <- min(fall.weekdays.evenings$Global_active_power, na.rm=T)
max.fall.weekdays.evenings <- max(fall.weekdays.evenings$Global_active_power, na.rm=T)

min.fall.weekdays.nights <- min(fall.weekdays.nights$Global_active_power, na.rm=T)
max.fall.weekdays.nights <- max(fall.weekdays.nights$Global_active_power, na.rm=T)

#fall.weekends
min.fall.weekends.afternoons <- min(fall.weekends.afternoons$Global_active_power, na.rm=T)
max.fall.weekends.afternoons <- max(fall.weekends.afternoons$Global_active_power, na.rm=T)

min.fall.weekends.mornings <- min(fall.weekends.mornings$Global_active_power, na.rm=T)
max.fall.weekends.mornings <- max(fall.weekends.mornings$Global_active_power, na.rm=T)

min.fall.weekends.evenings <- min(fall.weekends.evenings$Global_active_power, na.rm=T)
max.fall.weekends.evenings <- max(fall.weekends.evenings$Global_active_power, na.rm=T)

min.fall.weekends.nights <- min(fall.weekends.nights$Global_active_power, na.rm=T)
max.fall.weekends.nights <- max(fall.weekends.nights$Global_active_power, na.rm=T)

#spring.weekdays
min.spring.weekdays.afternoons <- min(spring.weekdays.afternoons$Global_active_power, na.rm=T)
max.spring.weekdays.afternoons <- max(spring.weekdays.afternoons$Global_active_power, na.rm=T)

min.spring.weekdays.mornings <- min(spring.weekdays.mornings$Global_active_power, na.rm=T)
max.spring.weekdays.mornings <- max(spring.weekdays.mornings$Global_active_power, na.rm=T)

min.spring.weekdays.evenings <- min(spring.weekdays.evenings$Global_active_power, na.rm=T)
max.spring.weekdays.evenings <- max(spring.weekdays.evenings$Global_active_power, na.rm=T)

min.spring.weekdays.nights <- min(spring.weekdays.nights$Global_active_power, na.rm=T)
max.spring.weekdays.nights <- max(spring.weekdays.nights$Global_active_power, na.rm=T)

#spring.weekends
min.spring.weekends.afternoons <- min(spring.weekends.afternoons$Global_active_power, na.rm=T)
max.spring.weekends.afternoons <- max(spring.weekends.afternoons$Global_active_power, na.rm=T)

min.spring.weekends.mornings <- min(spring.weekends.mornings$Global_active_power, na.rm=T)
max.spring.weekends.mornings <- max(spring.weekends.mornings$Global_active_power, na.rm=T)

min.spring.weekends.evenings <- min(spring.weekends.evenings$Global_active_power, na.rm=T)
max.spring.weekends.evenings <- max(spring.weekends.evenings$Global_active_power, na.rm=T)

min.spring.weekends.nights <- min(spring.weekends.nights$Global_active_power, na.rm=T)
max.spring.weekends.nights <- max(spring.weekends.nights$Global_active_power, na.rm=T)

#summer.weekdays
min.summer.weekdays.afternoons <- min(summer.weekdays.afternoons$Global_active_power, na.rm=T)
max.summer.weekdays.afternoons <- max(summer.weekdays.afternoons$Global_active_power, na.rm=T)

min.summer.weekdays.mornings <- min(summer.weekdays.mornings$Global_active_power, na.rm=T)
max.summer.weekdays.mornings <- max(summer.weekdays.mornings$Global_active_power, na.rm=T)

min.summer.weekdays.evenings <- min(summer.weekdays.evenings$Global_active_power, na.rm=T)
max.summer.weekdays.evenings <- max(summer.weekdays.evenings$Global_active_power, na.rm=T)

min.summer.weekdays.nights <- min(summer.weekdays.nights$Global_active_power, na.rm=T)
max.summer.weekdays.nights <- max(summer.weekdays.nights$Global_active_power, na.rm=T)

#summer.weekends
min.summer.weekends.afternoons <- min(summer.weekends.afternoons$Global_active_power, na.rm=T)
max.summer.weekends.afternoons <- max(summer.weekends.afternoons$Global_active_power, na.rm=T)

min.summer.weekends.mornings <- min(summer.weekends.mornings$Global_active_power, na.rm=T)
max.summer.weekends.mornings <- max(summer.weekends.mornings$Global_active_power, na.rm=T)

min.summer.weekends.evenings <- min(summer.weekends.evenings$Global_active_power, na.rm=T)
max.summer.weekends.evenings <- max(summer.weekends.evenings$Global_active_power, na.rm=T)

min.summer.weekends.nights <- min(summer.weekends.nights$Global_active_power, na.rm=T)
max.summer.weekends.nights <- max(summer.weekends.nights$Global_active_power, na.rm=T)

#winter.weekdays
min.winter.weekdays.afternoons <- min(winter.weekdays.afternoons$Global_active_power, na.rm=T)
max.winter.weekdays.afternoons <- max(winter.weekdays.afternoons$Global_active_power, na.rm=T)

min.winter.weekdays.mornings <- min(winter.weekdays.mornings$Global_active_power, na.rm=T)
max.winter.weekdays.mornings <- max(winter.weekdays.mornings$Global_active_power, na.rm=T)

min.winter.weekdays.evenings <- min(winter.weekdays.evenings$Global_active_power, na.rm=T)
max.winter.weekdays.evenings <- max(winter.weekdays.evenings$Global_active_power, na.rm=T)

min.winter.weekdays.nights <- min(winter.weekdays.nights$Global_active_power, na.rm=T)
max.winter.weekdays.nights <- max(winter.weekdays.nights$Global_active_power, na.rm=T)

#winter.weekends
min.winter.weekends.afternoons <- min(winter.weekends.afternoons$Global_active_power, na.rm=T)
max.winter.weekends.afternoons <- max(winter.weekends.afternoons$Global_active_power, na.rm=T)

min.winter.weekends.mornings <- min(winter.weekends.mornings$Global_active_power, na.rm=T)
max.winter.weekends.mornings <- max(winter.weekends.mornings$Global_active_power, na.rm=T)

min.winter.weekends.evenings <- min(winter.weekends.evenings$Global_active_power, na.rm=T)
max.winter.weekends.evenings <- max(winter.weekends.evenings$Global_active_power, na.rm=T)

min.winter.weekends.nights <- min(winter.weekends.nights$Global_active_power, na.rm=T)
max.winter.weekends.nights <- max(winter.weekends.nights$Global_active_power, na.rm=T)