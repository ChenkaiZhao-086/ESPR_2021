### 循环系统pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm2, 1) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm2, 2) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm2, 3) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm2, 7) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I10 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I44 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I50 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I60 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2), summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I60 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

### 循环系统pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm2, 1) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm2, 2) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm2, 3) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm2, 7) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I10 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I44 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I50 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I60 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2), summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I60 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

### 循环系统pm2 + co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm2, 1) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm2, 2) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm2, 3) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm2, 7) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2 + co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + co + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I10 pm2 + co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm2, 1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm2, 2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm2, 3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm2, 7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2 + co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2 + co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(pm2, 1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(pm2, 2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(pm2, 3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(pm2, 7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2 + co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I44 pm2 + co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(pm2, 1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(pm2, 2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(pm2, 3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(pm2, 7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2 + co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 pm2 + co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm2 + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(pm2, 1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(pm2, 2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(pm2, 3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(pm2, 7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I50 pm2 + co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I60 pm2 + co，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ pm2 + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(pm2, 1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(pm2, 2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(pm2, 3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(pm2, 7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2), summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I60 pm2 + co，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(pm2,0:1) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(pm2,0:2) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(pm2,0:3) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(pm2,0:7) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(pm2,0:60) + co + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

### 循环系统pm2 + o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(circ ~ Lag(pm2, 1) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(circ ~ Lag(pm2, 2) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(circ ~ Lag(pm2, 3) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(circ ~ Lag(pm2, 7) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2 + o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I10 pm2 + o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm2, 1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm2, 2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm2, 3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm2, 7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2 + o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2 + o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I20 ~ Lag(pm2, 1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I20 ~ Lag(pm2, 2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I20 ~ Lag(pm2, 3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I20 ~ Lag(pm2, 7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2 + o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I44 pm2 + o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I44 ~ Lag(pm2, 1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I44 ~ Lag(pm2, 2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I44 ~ Lag(pm2, 3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I44 ~ Lag(pm2, 7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2 + o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 pm2 + o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm2 + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I50 ~ Lag(pm2, 1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I50 ~ Lag(pm2, 2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I50 ~ Lag(pm2, 3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I50 ~ Lag(pm2, 7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I50 pm2 + o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I60 pm2 + o3，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I60 ~ pm2 + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I60 ~ Lag(pm2, 1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I60 ~ Lag(pm2, 2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I60 ~ Lag(pm2, 3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I60 ~ Lag(pm2, 7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2), summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I60 pm2 + o3，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I60 ~ tsModel::runMean(pm2,0:1) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I60 ~ tsModel::runMean(pm2,0:2) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I60 ~ tsModel::runMean(pm2,0:3) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I60 ~ tsModel::runMean(pm2,0:7) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I60 ~ tsModel::runMean(pm2,0:60) + o3 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

