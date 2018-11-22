### FW Weekends - Days
mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                               data = fw.weekends.days,
                               nstates = 16,
                               ntimes = rep(540, 180))
fit(mod.fw.weekends.days)

t1.mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                                  data = t1.fw.weekends.days,
                                  nstates=16,
                                  ntimes = rep(540, 50))
fit(t1.mod.fw.weekends.days)

t2.mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                                  data = t2.fw.weekends.days,
                                  nstates=16,
                                  ntimes = rep(540, 50))
fit(t2.mod.fw.weekends.days)

t3.mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                                  data = t3.fw.weekends.days,
                                  nstates=16,
                                  ntimes = rep(540, 50))
fit(t3.mod.fw.weekends.days)

t4.mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                                  data = t4.fw.weekends.days,
                                  nstates=16,
                                  ntimes = rep(540, 50))
fit(t4.mod.fw.weekends.days)

t5.mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                                  data = t5.fw.weekends.days,
                                  nstates=16,
                                  ntimes = rep(540, 50))
fit(t5.mod.fw.weekends.days)



### FW Weekends - Evenings
mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = fw.weekends.evenings,
                                   nstates = 16,
                                   ntimes = c(396, rep(420, 150)))
fit(mod.fw.weekends.evenings)

t1.mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                  data = t1.fw.weekends.evenings,
                                  nstates = 16,
                                  ntimes = rep(420, 50))
fit(t1.mod.fw.weekends.evenings)

t2.mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t2.fw.weekends.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 50))
fit(t2.mod.fw.weekends.evenings)

t3.mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t3.fw.weekends.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 50))
fit(t3.mod.fw.weekends.evenings)

t4.mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t4.fw.weekends.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 50))
fit(t4.mod.fw.weekends.evenings)

t5.mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t5.fw.weekends.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 50))
fit(t5.mod.fw.weekends.evenings)

### FW Weekends - Nights
mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                 data = fw.weekends.nights, 
                                 nstates = 12,
                                 ntimes = rep(480, 150))
fit(mod.fw.weekends.nights)

t1.mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t1.fw.weekends.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 50))
fit(t1.mod.fw.weekends.nights)

t2.mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t2.fw.weekends.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 50))
fit(t2.mod.fw.weekends.nights)

t3.mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t3.fw.weekends.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 50))
fit(t3.mod.fw.weekends.nights)

t4.mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t4.fw.weekends.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 50))
fit(t4.mod.fw.weekends.nights)

t5.mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t5.fw.weekends.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 50))
fit(t5.mod.fw.weekends.nights)
