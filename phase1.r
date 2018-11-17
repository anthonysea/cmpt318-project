# Phase 1 (I) - General Data Exploration
colMax <- function(data) sapply(data, max, na.rm=T)
colMin <- function(data) sapply(data, min, na.rm=T)
colSD <- function(data) sapply(data, sd, na.rm=T)

dNames <- c("Train", "Test 1", "Test 2", "Test 3", "Test 4", "Test 5")
attrs <-  c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

getMean_SD <- function(tr, sub1, sub2, sub3, sub4, sub5, attrs, dNames) {
  cat("Mean and Standard Deviation of ", deparse(substitute(tr)), "\n")
  meanTab <- rbind(
    colMeans(tr[, attrs]),
    colMeans(sub1[, attrs]),
    colMeans(sub2[, attrs]),
    colMeans(sub3[, attrs]),
    colMeans(sub4[, attrs]),
    colMeans(sub5[, attrs])
  ); 
  rownames(meanTab) <- dNames
  print("Mean")
  print(meanTab)
  
  sdTab <- rbind(
    colSD(tr[, attrs]),
    colSD(sub1[, attrs]),
    colSD(sub2[, attrs]),
    colSD(sub3[, attrs]),
    colSD(sub4[, attrs]),
    colSD(sub5[, attrs])
  ); 
  rownames(sdTab) <- dNames
  print("Standard Deviation")
  print(sdTab)
}

getMean_SD(fw.weekdays.days, t1.fw.weekdays.days, t2.fw.weekdays.days, t3.fw.weekdays.days, 
           t4.fw.weekdays.days, t5.fw.weekdays.days, attrs, dNames)
getMean_SD(fw.weekdays.evenings, t1.fw.weekdays.evenings, t2.fw.weekdays.evenings, t3.fw.weekdays.evenings,
           t4.fw.weekdays.evenings, t5.fw.weekdays.evenings, attrs, dNames)
getMean_SD(fw.weekdays.nights, t1.fw.weekdays.nights, t2.fw.weekdays.nights, t3.fw.weekdays.nights, t4.fw.weekdays.nights,
           t5.fw.weekdays.nights, attrs, dNames)

getMean_SD(fw.weekends.days, t1.fw.weekends.days, t2.fw.weekends.days, t3.fw.weekends.days,
           t4.fw.weekends.days, t5.fw.weekends.days, attrs, dNames)
getMean_SD(fw.weekends.evenings, t1.fw.weekends.evenings, t2.fw.weekends.evenings, t3.fw.weekends.evenings,
           t4.fw.weekends.evenings, t5.fw.weekends.evenings, attrs, dNames)
getMean_SD(fw.weekends.nights, t1.fw.weekends.nights, t2.fw.weekends.nights, t3.fw.weekends.nights,
           t4.fw.weekends.nights, t5.fw.weekends.nights, attrs, dNames)

getMean_SD(ss.weekdays.days, t1.ss.weekdays.days, t2.ss.weekdays.days, t3.ss.weekdays.days, 
           t4.ss.weekdays.days, t5.ss.weekdays.days, attrs, dNames)
getMean_SD(ss.weekdays.evenings, t1.ss.weekdays.evenings, t2.ss.weekdays.evenings, t3.ss.weekdays.evenings,
           t4.ss.weekdays.evenings, t5.ss.weekdays.evenings, attrs, dNames)
getMean_SD(ss.weekdays.nights, t1.ss.weekdays.nights, t2.ss.weekdays.nights, t3.ss.weekdays.nights,
           t4.ss.weekdays.nights, t5.ss.weekdays.nights, attrs, dNames)

getMean_SD(ss.weekends.days, t1.ss.weekends.days, t2.ss.weekends.days, t3.ss.weekends.days,
           t4.ss.weekends.days, t5.ss.weekends.days, attrs, dNames)
getMean_SD(ss.weekends.evenings, t1.ss.weekends.evenings, t2.ss.weekends.evenings, t3.ss.weekends.evenings, t4.ss.weekends.evenings,
           t5.ss.weekends.evenings, attrs, dNames)
getMean_SD(ss.weekends.nights, t1.ss.weekends.nights, t2.ss.weekends.nights, t3.ss.weekends.nights, t4.ss.weekends.nights,
           t5.ss.weekends.nights, attrs, dNames)