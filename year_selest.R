### 循环系统pm2，单滞后，2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2，滑动平均滞后，2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis2)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 



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


### 循环系统pm2，单滞后，2016-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2，滑动平均滞后，2016-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=3*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis4)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 













