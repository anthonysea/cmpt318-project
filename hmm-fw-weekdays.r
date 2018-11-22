

### FW Weekdays - Days
mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1, 
                               data = fw.weekdays.days, 
                               nstates = 12,
                               ntimes = c(rep(540, 378), 368))
fit(mod.fw.weekdays.days)

t1.mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1, 
                               data = t1.fw.weekdays.days, 
                               nstates = 12,
                               ntimes = c(173, rep(540, 126)))
fit(t1.mod.fw.weekdays.days)

t2.mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1,
                                  data = t2.fw.weekdays.days,
                                  nstates=12,
                                  ntimes=c(173, rep(540, 126)))
fit(t2.mod.fw.weekdays.days)

t3.mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1,
                                  data = t3.fw.weekdays.days,
                                  nstates=12,
                                  ntimes=c(173, rep(540, 126)))
fit(t3.mod.fw.weekdays.days)

t4.mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1,
                                  data = t4.fw.weekdays.days,
                                  nstates=12,
                                  ntimes=c(173, rep(540, 126)))
fit(t4.mod.fw.weekdays.days)

t5.mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1,
                                  data = t5.fw.weekdays.days,
                                  nstates=12,
                                  ntimes=c(173, rep(540, 126)))
fit(t5.mod.fw.weekdays.days)




### FW Weekdays - Evenings
mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                 data = fw.weekdays.evenings,
                                 nstates = 16,
                                 ntimes = rep(420 , 378))
fit(mod.fw.weekdays.evenings)

t1.mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = t1.fw.weekdays.evenings,
                                   nstates = 16,
                                   ntimes = c(rep(420 , 126), 243))
fit(t1.mod.fw.weekdays.evenings)

t2.mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t2.fw.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = c(rep(420 , 126), 243))
fit(t2.mod.fw.weekdays.evenings)

t3.mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t3.fw.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = c(rep(420 , 126), 243))
fit(t3.mod.fw.weekdays.evenings)

t4.mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t4.fw.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = c(rep(420 , 126), 243))
fit(t4.mod.fw.weekdays.evenings)

t5.mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t5.fw.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = c(rep(420 , 126), 243))
fit(t5.mod.fw.weekdays.evenings)


### FW Weekdays - Nights
mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                 data = fw.weekdays.nights,
                                 nstates = 12,
                                 ntimes = rep(480, 379))
fit(mod.fw.weekdays.nights)

t1.mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                 data = t1.fw.weekdays.nights,
                                 nstates = 12,
                                 ntimes = rep(480, 126))
fit(t1.mod.fw.weekdays.nights)

t2.mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                    data = t2.fw.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 126))
fit(t2.mod.fw.weekdays.nights)

t3.mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                    data = t3.fw.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 126))
fit(t3.mod.fw.weekdays.nights)

t4.mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                    data = t4.fw.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 126))
fit(t4.mod.fw.weekdays.nights)

t5.mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                    data = t5.fw.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 126))
fit(t5.mod.fw.weekdays.nights)


