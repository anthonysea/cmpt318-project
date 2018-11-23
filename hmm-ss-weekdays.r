mod.ss.weekdays.days <- depmix(response = Global_active_power ~ 1,
                               data = ss.weekdays.days,
                               nstates = 19,
                               ntimes = rep(540, 393))

### SS Weekdays - Evenings
mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = ss.weekdays.evenings,
                                   nstates = 16,
                                   ntimes = rep(420, 393))
fit(mod.ss.weekdays.evenings)

t1.mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t1.ss.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 132))
fit(t1.mod.ss.weekdays.evenings)

t2.mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t2.ss.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 132))
fit(t2.mod.ss.weekdays.evenings)

t3.mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t3.ss.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 132))
fit(t3.mod.ss.weekdays.evenings)

t4.mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t4.ss.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 132))
fit(t4.mod.ss.weekdays.evenings)

t5.mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                      data = t5.ss.weekdays.evenings,
                                      nstates = 16,
                                      ntimes = rep(420, 132))
fit(t5.mod.ss.weekdays.evenings)


### SS Weekdays - Nights
mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                 data = ss.weekdays.nights,
                                 nstates = 12,
                                 ntimes = rep(720, 262))
fit(mod.ss.weekdays.nights)

t1.mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t1.ss.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 132))
fit(t1.mod.ss.weekdays.nights)

t2.mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t2.ss.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 132))
fit(t2.mod.ss.weekdays.nights)

t3.mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t3.ss.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 132))
fit(t3.mod.ss.weekdays.nights)

t4.mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t4.ss.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 132))
fit(t4.mod.ss.weekdays.nights)

t5.mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                    data = t5.ss.weekdays.nights,
                                    nstates = 12,
                                    ntimes = rep(480, 132))
fit(t5.mod.ss.weekdays.nights)