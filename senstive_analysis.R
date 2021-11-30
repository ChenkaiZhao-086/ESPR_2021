cbt <- crossbasis(analysis3$mean_temp, 21, 
                  argvar = list(fun="ns", knots = quantile(analysis3$mean_temp,c(10,75,90)/100, na.rm=T),
                                Bound=range(analysis$mean_temp,na.rm=T)), 
                  arglag = list(fun="ns",knots=logknots(21,nk=3))) 

fun="ns", df=5

f0 <- gam(circ ~ pm2+o3 + cbt + s(rh, k=4) + s(time, k=2*5+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ pm2+o3 + cbt + s(rh, k=4) + s(time, k=2*6+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ pm2+o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ pm2+o3 + cbt + s(rh, k=3) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ pm2+o3 + cbt + s(rh, k=5) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ pm2+o3 + cbt + s(rh, k=6) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ pm2+o3 + crossbasis(analysis3$mean_temp, 21, 
                                  argvar = list(fun="ns", df=3), 
                                  arglag = list(fun="ns",knots=logknots(21,nk=3)))  + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ pm2+o3 + crossbasis(analysis3$mean_temp, 21, 
                                  argvar = list(fun="ns", df=4), 
                                  arglag = list(fun="ns",knots=logknots(21,nk=3)))  + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f8 <- gam(circ ~ pm2+o3 + crossbasis(analysis3$mean_temp, 21, 
                                  argvar = list(fun="ns", df=5), 
                                  arglag = list(fun="ns",knots=logknots(21,nk=3)))  + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7),summary(f8))
lapply(summ_list, coef.exat) 

