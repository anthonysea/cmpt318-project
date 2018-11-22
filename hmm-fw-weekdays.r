
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
                                  ntiems=c(44, rep(540, 122)))
fit(t2.mod.fw.weekdays.days)

mod.fw.weekdays.evenings <- depmix(response = Global_active_power ~ 1,
                                 data = fw.weekdays.evenings,
                                 nstates = 16,
                                 ntimes = rep(420 , 378))
fit(mod.fw.weekdays.evenings)

mod.fw.weekdays.nights <- depmix(response = Global_active_power ~ 1, 
                                 data = fw.weekdays.nights,
                                 nstates = 12,
                                 ntimes = rep(480, 379))
fit(mod.fw.weekdays.nights)


