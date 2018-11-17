# Phase 1 - General Data Exploration
library(expss)

colMax <- function(data) sapply(data, max, na.rm=T)
colMin <- function(data) sapply(data, min, na.rm=T)
colSD <- function(data) sapply(data, sd, na.rm=T)

dNames <- c("Train", "Test 1", "Test 2", "Test 3", "Test 4", "Test 5")
attrs <-  c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

fw.weekdays.nights.mean_comparison <- rbind(
  colMeans(fw.weekdays.nights[,attrs]),
  colMeans(t1.fw.weekdays.nights[,attrs]),
  colMeans(t2.fw.weekdays.nights[,attrs]),
  colMeans(t3.fw.weekdays.nights[,attrs]),
  colMeans(t4.fw.weekdays.nights[,attrs]),
  colMeans(t5.fw.weekdays.nights[,attrs])
  ); rownames(fw.weekdays.nights.mean_comparison) <- dNames; fw.weekdays.nights.mean_comparison

fw.weekdays.nights.sd_comparison <- rbind(
  colSD(fw.weekdays.nights[, attrs]),
  colSD(t1.fw.weekdays.nights[,attrs]),
  colSD(t2.fw.weekdays.nights[,attrs]),
  colSD(t3.fw.weekdays.nights[,attrs]),
  colSD(t4.fw.weekdays.nights[,attrs]),
  colSD(t5.fw.weekdays.nights[,attrs])
); rownames(fw.weekdays.nights.sd_comparison) <- dNames; fw.weekdays.nights.sd_comparison

# Compare Mean value of attributes in the F/W Weekdays Days time window
fw.weekdays.days.mean_comparison <- rbind(
  colMeans(fw.weekdays.days[,attrs]),
  colMeans(t1.fw.weekdays.days[,attrs]),
  colMeans(t2.fw.weekdays.days[,attrs]),
  colMeans(t3.fw.weekdays.days[,attrs]),
  colMeans(t4.fw.weekdays.days[,attrs]),
  colMeans(t5.fw.weekdays.days[,attrs])
); rownames(fw.weekdays.days.mean_comparison) <- dNames; fw.weekdays.days.mean_comparison

# Compare SD value of attributes in the F/W Weekdays Days time window
fw.weekdays.days.sd_comparison <- rbind(
  colSD(fw.weekdays.days[, attrs]),
  colSD(t1.fw.weekdays.days[,attrs]),
  colSD(t2.fw.weekdays.days[,attrs]),
  colSD(t3.fw.weekdays.days[,attrs]),
  colSD(t4.fw.weekdays.days[,attrs]),
  colSD(t5.fw.weekdays.days[,attrs])
); rownames(fw.weekdays.days.sd_comparison) <- dNames; fw.weekdays.days.sd_comparison



fw.weekdays.evenings.mean_comparison <- rbind(
  colMeans(fw.weekdays.evenings[,attrs]),
  colMeans(t1.fw.weekdays.evenings[,attrs]),
  colMeans(t2.fw.weekdays.evenings[,attrs]),
  colMeans(t3.fw.weekdays.evenings[,attrs]),
  colMeans(t4.fw.weekdays.evenings[,attrs]),
  colMeans(t5.fw.weekdays.evenings[,attrs])
); rownames(fw.weekdays.evenings.mean_comparison) <- dNames; fw.weekdays.evenings.mean_comparison

fw.weekdays.evenings.sd_comparison <- rbind(
  colSD(fw.weekdays.evenings[,attrs]),
  colSD(t1.fw.weekdays.evenings[,attrs]),
  colSD(t2.fw.weekdays.evenings[,attrs]),
  colSD(t3.fw.weekdays.evenings[,attrs]),
  colSD(t4.fw.weekdays.evenings[,attrs]),
  colSD(t5.fw.weekdays.evenings[,attrs])
); rownames(fw.weekdays.evenings.sd_comparison) <- dNames; fw.weekdays.evenings.sd_comparison


fw.weekends.nights.mean_comparison <- rbind(
  colMeans(fw.weekends.nights[, attrs]),
  colMeans(t1.fw.weekends.nights[, attrs]),
  colMeans(t2.fw.weekends.nights[, attrs]),
  colMeans(t3.fw.weekends.nights[, attrs]),
  colMeans(t4.fw.weekends.nights[, attrs]),
  colMeans(t5.fw.weekends.nights[, attrs])
); rownames(fw.weekends.nights.mean_comparison) <- dNames; fw.weekends.nights.mean_comparison

fw.weekends.nights.sd_comparison <- rbind(
  colSD(fw.weekends.nights[, attrs]),
  colSD(t1.fw.weekends.nights[, attrs]),
  colSD(t2.fw.weekends.nights[, attrs]),
  colSD(t3.fw.weekends.nights[, attrs]),
  colSD(t4.fw.weekends.nights[, attrs]),
  colSD(t5.fw.weekends.nights[, attrs])
); rownames(fw.weekends.nights.sd_comparison) <- dNames; fw.weekends.nights.sd_comparison

fw.weekends.days.mean_comparison <- rbind(
  colMeans(fw.weekends.days[, attrs]), 
  colMeans(t1.fw.weekends.days[, attrs]), 
  colMeans(t2.fw.weekends.days[, attrs]), 
  colMeans(t3.fw.weekends.days[, attrs]), 
  colMeans(t4.fw.weekends.days[, attrs]), 
  colMeans(t5.fw.weekends.days[, attrs])
); rownames(fw.weekends.days.mean_comparison) <- dNames; fw.weekends.days.mean_comparison


fw.weekends.days.sd_comparison <- rbind(
  colSD(fw.weekends.days[, attrs]),
  colSD(t1.fw.weekends.days[, attrs]),
  colSD(t2.fw.weekends.days[, attrs]),
  colSD(t3.fw.weekends.days[, attrs]),
  colSD(t4.fw.weekends.days[, attrs]),
  colSD(t5.fw.weekends.days[, attrs])
); rownames(fw.weekends.days.sd_comparison) <- dNames; fw.weekends.days.sd_comparison


fw.weekends.evenings.mean_comparison <- rbind(
  colMeans(fw.weekends.evenings[, attrs]), 
  colMeans(t1.fw.weekends.evenings[, attrs]), 
  colMeans(t2.fw.weekends.evenings[, attrs]), 
  colMeans(t3.fw.weekends.evenings[, attrs]), 
  colMeans(t4.fw.weekends.evenings[, attrs]), 
  colMeans(t5.fw.weekends.evenings[, attrs])
); rownames(fw.weekends.evenings.mean_comparison) <- dNames; fw.weekends.days.mean_comparison


fw.weekends.evenings.sd_comparison <- rbind(
  colSD(fw.weekends.evenings[, attrs]),
  colSD(t1.fw.weekends.evenings[, attrs]),
  colSD(t2.fw.weekends.evenings[, attrs]),
  colSD(t3.fw.weekends.evenings[, attrs]),
  colSD(t4.fw.weekends.evenings[, attrs]),
  colSD(t5.fw.weekends.evenings[, attrs])
); rownames(fw.weekends.evenings.sd_comparison) <- dNames; fw.weekends.evenings.sd_comparison


### Spring/Summer
ss.weekdays.evenings.mean_comparison <- rbind(
  colMeans(ss.weekends.evenings[, attrs]), 
  colMeans(t1.ss.weekdays.evenings[, attrs]), 
  colMeans(t2.ss.weekdays.evenings[, attrs]), 
  colMeans(t3.ss.weekdays.evenings[, attrs]), 
  colMeans(t4.ss.weekdays.evenings[, attrs]), 
  colMeans(t5.ss.weekdays.evenings[, attrs])
); rownames(ss.weekdays.evenings.mean_comparison) <- dNames; ss.weekdays.evenings.mean_comparison

ss.weekdays.evenings.sd_comparison <- rbind(
  colSD(ss.weekends.evenings[, attrs]), 
  colSD(t1.ss.weekdays.evenings[, attrs]), 
  colSD(t2.ss.weekdays.evenings[, attrs]), 
  colSD(t3.ss.weekdays.evenings[, attrs]), 
  colSD(t4.ss.weekdays.evenings[, attrs]), 
  colSD(t5.ss.weekdays.evenings[, attrs])
); rownames(ss.weekdays.evenings.sd_comparison) <- dNames; ss.weekdays.evenings.sd_comparison


ss.weekdays.nights.mean_comparison <- rbind(
  colMeans(ss.weekdays.nights[, attrs]), 
  colMeans(t1.ss.weekdays.nights[, attrs]), 
  colMeans(t2.ss.weekdays.nights[, attrs]), 
  colMeans(t3.ss.weekdays.nights[, attrs]), 
  colMeans(t4.ss.weekdays.nights[, attrs]), 
  colMeans(t5.ss.weekdays.nights[, attrs])
); rownames(ss.weekdays.nights.mean_comparison) <- dNames; ss.weekdays.nights.mean_comparison

ss.weekdays.nights.sd_comparison <- rbind(
  colSD(ss.weekdays.nights[, attrs]), 
  colSD(t1.ss.weekdays.nights[, attrs]), 
  colSD(t2.ss.weekdays.nights[, attrs]), 
  colSD(t3.ss.weekdays.nights[, attrs]), 
  colSD(t4.ss.weekdays.nights[, attrs]), 
  colSD(t5.ss.weekdays.nights[, attrs])
); rownames(ss.weekdays.nights.sd_comparison) <- dNames; ss.weekdays.nights.sd_comparison


ss.weekdays.days.mean_comparison <- rbind(
  colMeans(ss.weekdays.days[, attrs]), 
  colMeans(t1.ss.weekdays.days[, attrs]), 
  colMeans(t2.ss.weekdays.days[, attrs]), 
  colMeans(t3.ss.weekdays.days[, attrs]), 
  colMeans(t4.ss.weekdays.days[, attrs]), 
  colMeans(t5.ss.weekdays.days[, attrs])
); rownames(ss.weekdays.days.mean_comparison) <- dNames; ss.weekdays.days.mean_comparison 

ss.weekdays.days.sd_comparison <- rbind(
  colSD(ss.weekdays.days[, attrs]), 
  colSD(t1.ss.weekdays.days[, attrs]), 
  colSD(t2.ss.weekdays.days[, attrs]), 
  colSD(t3.ss.weekdays.days[, attrs]), 
  colSD(t4.ss.weekdays.days[, attrs]), 
  colSD(t5.ss.weekdays.days[, attrs])
); rownames(ss.weekdays.days.sd_comparison) <- dNames; ss.weekdays.days.sd_comparison 


ss.weekends.nights.mean_comparison <- rbind(
  colMeans(ss.weekends.nights[, attrs]),
  colMeans(t1.ss.weekends.nights[, attrs]),
  colMeans(t2.ss.weekends.nights[, attrs]),
  colMeans(t3.ss.weekends.nights[, attrs]),
  colMeans(t4.ss.weekends.nights[, attrs]),
  colMeans(t5.ss.weekends.nights[, attrs])
); rownames(ss.weekends.nights.mean_comparison) <- dNames; ss.weekends.nights.mean_comparison

ss.weekends.nights.sd_comparison <- rbind(
  colSD(ss.weekends.nights[, attrs]),
  colSD(t1.ss.weekends.nights[, attrs]),
  colSD(t2.ss.weekends.nights[, attrs]),
  colSD(t3.ss.weekends.nights[, attrs]),
  colSD(t4.ss.weekends.nights[, attrs]),
  colSD(t5.ss.weekends.nights[, attrs])
); rownames(ss.weekends.nights.sd_comparison) <- dNames; ss.weekends.nights.sd_comparison


ss.weekends.days.mean_comparison <- rbind(
  colMeans(ss.weekends.days[, attrs]),
  colMeans(t1.ss.weekends.days[, attrs]),
  colMeans(t2.ss.weekends.days[, attrs]),
  colMeans(t3.ss.weekends.days[, attrs]),
  colMeans(t4.ss.weekends.days[, attrs]),
  colMeans(t5.ss.weekends.days[, attrs])
); rownames(ss.weekends.days.mean_comparison) <- dNames; ss.weekends.days.mean_comparison

ss.weekends.days.sd_comparison <- rbind(
  colSD(ss.weekends.days[, attrs]),
  colSD(t1.ss.weekends.days[, attrs]),
  colSD(t2.ss.weekends.days[, attrs]),
  colSD(t3.ss.weekends.days[, attrs]),
  colSD(t4.ss.weekends.days[, attrs]),
  colSD(t5.ss.weekends.days[, attrs])
); rownames(ss.weekends.days.sd_comparison) <- dNames; ss.weekends.days.sd_comparison


ss.weekends.evenings.mean_comparison <- rbind(
  colMeans(ss.weekends.evenings[, attrs]),
  colMeans(t1.ss.weekends.evenings[, attrs]),
  colMeans(t2.ss.weekends.evenings[, attrs]),
  colMeans(t3.ss.weekends.evenings[, attrs]),
  colMeans(t4.ss.weekends.evenings[, attrs]),
  colMeans(t5.ss.weekends.evenings[, attrs])
); rownames(ss.weekends.evenings.mean_comparison) <- dNames; ss.weekends.evenings.mean_comparison

ss.weekends.evenings.sd_comparison <- rbind(
  colSD(ss.weekends.evenings[, attrs]),
  colSD(t1.ss.weekends.evenings[, attrs]),
  colSD(t2.ss.weekends.evenings[, attrs]),
  colSD(t3.ss.weekends.evenings[, attrs]),
  colSD(t4.ss.weekends.evenings[, attrs]),
  colSD(t5.ss.weekends.evenings[, attrs])
); rownames(ss.weekends.evenings.sd_comparison) <- dNames; ss.weekends.evenings.sd_comparison
