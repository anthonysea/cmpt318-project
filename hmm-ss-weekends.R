mod.ss.weekends.days <- depmix(response = Global_active_power ~ 1,
                               data = ss.weekends.days,
                               nstates = 19,
                               ntimes = )

mod.ss.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = ss.weekends.evenings,
                                   nstates = 19,
                                   ntimes = )

mod.ss.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                 data = ss.weekends.nights,
                                 nstates = 19,
                                 ntimes = )