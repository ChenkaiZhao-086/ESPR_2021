analysis3 <- subset(analysis3, month>=4 & month<=10)
analysis3 <- subset(analysis3, month<4 | month>10)

cbt <- crossbasis(analysis3$mean_temp, 21, 
                  argvar = list(fun="ns", knots = quantile(analysis3$mean_temp,c(10,75,90)/100, na.rm=T),
                                Bound=range(analysis$mean_temp,na.rm=T)), 
                  arglag = list(fun="ns",knots=logknots(21,nk=3))) 


### I10 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I10 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I10 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I10 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I10 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I10 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I10 pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I10 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I10 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I10 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### I10 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I10 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I10 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I10 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I10 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I10 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I10 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I10 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I10 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I10 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I10 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I10 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I10 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I10 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I10 o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I10 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I10 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I10 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I10 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I10 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I10 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I10 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I10 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I10 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I10 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I10 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I10 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I10 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I10 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I10 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I10 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I20 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I20 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I20 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I20 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I20 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I20 呼吸系统pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I20 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I20 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I20 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### I20 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I20 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I20 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I20 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I20 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I20 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I20 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I20 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I20 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I20 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I20 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I20 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I20 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I20 呼吸系统o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I20 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I20 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I20 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I20 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I20 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I20 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I20 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I20 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I20 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I20 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I20 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I20 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I20 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I20 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I20 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I20 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I44 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I44 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I44 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I44 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I44 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I44 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I44 全疾病pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I44 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I44 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I44 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### I44 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I44 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I44 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I44 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I44 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I44 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I44 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I44 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I44 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I44 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I44 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I44 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I44 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I44 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I44 o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I44 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I44 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I44 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I44 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I44 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I44 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I44 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I44 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I44 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I44 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I44 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I44 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I44 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I44 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I44 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I44 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I50 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I50 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I50 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### I50 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I50 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I50 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I50 pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I50 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I50 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I50 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### I50 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I50 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I50 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I50 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I50 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I50 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I50 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I50 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I50 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I50 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I50 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I50 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I50 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I50 o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I50 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I50 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I50 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I50 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I50 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I50 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I50 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I50 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I50 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I50 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I50 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I50 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I50 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I50 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I50 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I50 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I60 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I60 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I60 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I60 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### I60 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I60 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I60 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I60 呼吸系统pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I60 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I60 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I60 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### I60 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I60 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I60 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I60 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I60 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I60 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I60 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I60 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I60 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I60 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I60 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I60 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I60 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I60 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I60 o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I60 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I60 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I60 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I60 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I60 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I60 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I60 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I60 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I60 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### I60 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(I60 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(I60 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(I60 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### I60 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(I60 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(I60 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J18 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J18 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J18 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J18 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J18 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J18 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J18 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J18 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J18 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### J18 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J18 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J18 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J18 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J18 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J18 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J18 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J18 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J18 pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J18 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J18 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J18 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J18 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J18 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J18 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J18 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J18 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### J18 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J18 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J18 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J18 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J18 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J18 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J18 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J18 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### J18 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J18 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J18 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J18 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J18 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J18 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J18 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J18 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J18 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J18 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J18 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J18 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J18 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J18 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J18 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J18 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J18 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J18 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J18 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J18 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J18 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J18 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J18 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J18 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J18 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J18 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J18 o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J18 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J18 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J18 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J18 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J18 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J18 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J18 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### J18 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J18 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J18 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J18 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J18 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J18 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J18 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J18 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J18 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J18 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J18 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J18 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J18 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J18 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J18 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J18 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J18 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J18 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J18 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J18 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J18 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J18 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J18 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J18 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J18 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J18 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J18 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J18 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J18 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J18 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J18 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J18 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J18 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J18 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### J40 pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J40 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J40 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J40 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J40 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J40 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J40 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J40 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J40 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### J40 pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J40 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J40 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J40 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J40 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J40 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J40 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J40 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J40 pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J40 ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J40 ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J40 ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J40 ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J40 ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J40 ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J40 ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J40 ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### J40 pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J40 ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J40 ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J40 ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J40 ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J40 ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J40 ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J40 ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### J40 so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J40 ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J40 ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J40 ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J40 ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J40 ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J40 ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J40 ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J40 ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J40 so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J40 ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J40 ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J40 ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J40 ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J40 ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J40 ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J40 ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J40 o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J40 ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J40 ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J40 ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J40 ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J40 ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J40 ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J40 ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J40 ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J40 o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J40 ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J40 ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J40 ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J40 ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J40 ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J40 ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J40 ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### J40 no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J40 ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J40 ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J40 ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J40 ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J40 ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J40 ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J40 ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J40 ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J40 no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J40 ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J40 ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J40 ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J40 ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J40 ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J40 ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J40 ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### J40 co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(J40 ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(J40 ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(J40 ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(J40 ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(J40 ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(J40 ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(J40 ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(J40 ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### J40 co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(J40 ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(J40 ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(J40 ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(J40 ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(J40 ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(J40 ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(J40 ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


