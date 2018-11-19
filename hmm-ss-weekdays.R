mod.ss.weekdays.days <- depmix(response = Global_active_power ~ 1,
                               data = ss.weekdays.days,
                               nstates = 19,
                               ntimes =)

mod.ss.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = ss.weekdays.evenings,
                                   nstates = 19,
                                   ntimes = )

mod.ss.weekdays.nights <- depmix(response = Global_active_power ~ 1,
                                 data = ss.weekdays.nights,
                                 nstates = 19,
                                 ntimes = )

