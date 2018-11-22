
mod.fw.weekdays.days <- depmix(response = Global_active_power ~ 1, 
                               data = fw.weekdays.days, 
                               nstates = 19,
                               ntimes = c(rep(540, 378), 368))

mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                 data = fw.weekdays.evenings,
                                 nstates = 19,
                                 ntimes = rep(420 , 378))

mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                 data = fw.weekdays.nights,
                                 nstates = 19,
                                 ntimes = rep(480, 379))

fit(mod.fw.weekday.days)

