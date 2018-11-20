mod.ss.weekdays.days <- depmix(response = Global_active_power ~ 1,
                               data = ss.weekdays.days,
                               nstates = 19,
                               ntimes = rep(540, 393))

mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = ss.weekdays.evenings,
                                   nstates = 19,
                                   ntimes = rep(420, 393))

mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                 data = ss.weekdays.nights,
                                 nstates = 19,
                                 ntimes = rep(720, 262))

