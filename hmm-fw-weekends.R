mod.fw.weekends.days <- depmix(response = Global_active_power ~ 1, 
                               data = fw.weekends.days,
                               nstates = 19,
                               ntimes = )

mod.fw.weekends.evenings <- depmix(response = Global_active_power ~ 1,
                                   data = fw.weekends.evenings,
                                   nstates = 19,
                                   ntimes = )

mod.fw.weekends.nights <- depmix(response = Global_active_power ~ 1,
                                 data = fw.weekends.nights, 
                                 nstates = 19,
                                 ntimes = )

