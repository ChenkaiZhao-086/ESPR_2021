##################################### nature cubic spline控制平滑 ################################
analysis3 <- subset(analysis3, month>=4 & month<=10)
analysis3 <- subset(analysis3, month>=6 & month<=8)

analysis3 <- subset(analysis3, month<4 | month>10)
analysis3 <- subset(analysis3, month<=2 | month==12)

### 循环系统pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 循环系统pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 循环系统pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm1,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm1,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm1,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(pm1,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(pm1,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm1,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm1,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 循环系统so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(so2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(so2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(so2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(so2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(so2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(so2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(so2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 循环系统o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(o3,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(o3,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(o3,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(o3,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(o3,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(o3,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(o3,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### 循环系统no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(no2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(no2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(no2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(no2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(no2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(no2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(no2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(no2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(no2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(no2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(no2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(no2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(no2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(no2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 循环系统co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 呼吸系统pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 呼吸系统pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(pm2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 呼吸系统pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 呼吸系统pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(pm1,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(pm1,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(pm1,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(pm1,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(pm1,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(pm1,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(pm1,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 呼吸系统so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(so2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(so2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(so2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(so2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(so2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(so2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(so2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 呼吸系统o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(o3,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(o3,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(o3,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(o3,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(o3,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(o3,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(o3,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### 呼吸系统no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(no2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(no2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(no2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(no2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(no2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(no2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(no2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(no2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(no2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(no2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(no2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(no2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(no2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(no2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 呼吸系统co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 全疾病pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(pm2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 全疾病pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(pm1,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(pm1,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(pm1,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(pm1,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(pm1,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(pm1,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(pm1,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 全疾病so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(so2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(so2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(so2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(so2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(so2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(so2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(so2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(o3,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(o3,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(o3,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(o3,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(o3,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(o3,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(o3,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### 全疾病no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(no2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(no2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(no2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(no2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(no2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(no2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(no2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(no2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(no2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(no2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(no2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(no2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(no2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(no2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(co, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(co, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(co, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(co, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(co, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(co, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(co, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(co,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(co,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(co,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(co,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(co,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(co,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(co,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


########################################### 使用crossbasis控制平滑 #####################################
## 建立不同的arvar变量
argvar <- list(fun="ns", knots = quantile(analysis3$mean_temp,c(10,75,90)/100, na.rm=T),
               Bound=range(analysis$mean_temp,na.rm=T))

argvar <- list(fun="ns", knots = quantile(analysis3$mean_temp,c(10,75,90)/100, na.rm=T))

argvar <- list(fun="ns", knots = quantile(analysis3$mean_temp,c(5,50,95)/100, na.rm=T))

argvar <- list(fun="ns", df=3)

argvar <- list(fun="ns", knots=equalknots(analysis3$mean_temp,nk=5))

argvar <- list(fun="ns", knots=equalknots(analysis3$mean_temp,nk=3))


## 建立log-scale的滞后变量
arglag <- list(fun="ns",knots=logknots(21,nk=3))


layout(matrix(1:2,ncol=2,byrow = T))

cbt <- crossbasis(analysis3$mean_temp, 21, 
                  argvar = list(fun="ns", knots = quantile(analysis3$mean_temp,c(10,75,90)/100, na.rm=T),
                                                         Bound=range(analysis$mean_temp,na.rm=T)), 
                  arglag = list(fun="ns",knots=logknots(21,nk=3))) 

fcb <- gam(circ ~ cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)

cp <- crosspred(cbt, fcb, cen=5, by=0.3) # 这里的cen随便设置一个值，在下一步可以确定风险最小的阈值
cp$predvar[which.min(cp$allRRfit)] # 接上一步，使用这一语句得到最低的温度阈值
cen <- cp$predvar[which.min(cp$allRRfit)] # 接上一步，使用这一语句得到最低的温度阈值
cp <- crosspred(cbt, fcb, cen=cen, by=0.3) # 这里的cen随便设置一个值，在下一步可以确定风险最小的阈值

plot(cp,"3d",ltheta=150,xlab="Temperature (C)",ylab="Lag",zlab="RR", col=gray(0.9), main="Exposure-lag-response")

plot(cp,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab,ylab="RR",main="Overall")

### 循环系统pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 循环系统pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 循环系统pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 循环系统so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 循环系统o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### 循环系统no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 循环系统co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(co, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(co, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(co, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(circ ~ Lag(co, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(circ ~ Lag(co, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(circ ~ Lag(co, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(co, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(co,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(co,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(co,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(circ ~ tsModel::runMean(co,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(circ ~ tsModel::runMean(co,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(co,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(co,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 呼吸系统pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 呼吸系统pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 呼吸系统pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 呼吸系统pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 呼吸系统so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 呼吸系统o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### 呼吸系统no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 呼吸系统co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(resp ~ Lag(co, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(resp ~ Lag(co, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(resp ~ Lag(co, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(resp ~ Lag(co, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(resp ~ Lag(co, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(resp ~ Lag(co, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(resp ~ Lag(co, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ tsModel::runMean(co,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(resp ~ tsModel::runMean(co,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(resp ~ tsModel::runMean(co,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(resp ~ tsModel::runMean(co,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(resp ~ tsModel::runMean(co,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(resp ~ tsModel::runMean(co,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(resp ~ tsModel::runMean(co,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病pm2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 全疾病pm2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病pm1，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm1 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(pm1, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(pm1, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(pm1, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(pm1, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(pm1, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(pm1, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(pm1, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 全疾病pm1，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(pm1,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(pm1,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(pm1,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(pm1,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(pm1,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(pm1,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(pm1,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### 全疾病so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(so2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(so2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(so2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(so2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(so2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(so2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(so2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(so2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(so2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(so2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(so2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(so2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(so2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(so2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(o3, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(o3, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(o3, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(o3, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(o3, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(o3, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(o3, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(o3,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(o3,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(o3,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(o3,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(o3,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(o3,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(o3,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### 全疾病no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(no2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(no2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(no2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(no2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(no2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(no2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(no2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(no2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(no2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(no2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(no2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(no2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(no2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(no2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



### 全疾病co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(icd ~ Lag(co, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(icd ~ Lag(co, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(icd ~ Lag(co, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f4 <- gam(icd ~ Lag(co, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f5 <- gam(icd ~ Lag(co, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f6 <- gam(icd ~ Lag(co, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(icd ~ Lag(co, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ tsModel::runMean(co,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(icd ~ tsModel::runMean(co,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(icd ~ tsModel::runMean(co,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f04 <- gam(icd ~ tsModel::runMean(co,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f05 <- gam(icd ~ tsModel::runMean(co,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(icd ~ tsModel::runMean(co,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(icd ~ tsModel::runMean(co,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 