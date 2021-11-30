### 循环系统pm2，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f8 <- gam(circ ~ Lag(pm2, 14) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7), summary(f8))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 循环系统pm2，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=1*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


# Temp3，RH3，Time6 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ tsModel::runMean(pm2,0:4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ tsModel::runMean(pm2,0:5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 


### 循环系统pm1，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(circ ~ pm1 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm1, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm1, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm1, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm1, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm1, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm1, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm1, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(circ ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 循环系统pm1，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ SMA(pm1,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(pm1,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(pm1,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(pm1,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(pm1,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(pm1,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(circ ~ SMA(pm1,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(pm1,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(pm1,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(pm1,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(pm1,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(pm1,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(circ ~ SMA(pm1,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(pm1,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(pm1,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(pm1,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(pm1,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(pm1,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 


### 循环系统so2，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(circ ~ so2 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(so2, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(so2, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(so2, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(so2, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(so2, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(so2, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(so2, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统so2，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ SMA(so2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(so2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(so2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(so2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(so2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(so2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(circ ~ SMA(so2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(so2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(so2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(so2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(so2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(so2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(circ ~ SMA(so2,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(so2,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(so2,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(so2,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(so2,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(so2,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 



### 循环系统o3，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(circ ~ o3 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(o3, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(o3, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(o3, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(o3, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(o3, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(o3, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(o3, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(circ ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 循环系统o3，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ SMA(o3,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(o3,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(o3,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(o3,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(o3,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(o3,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(circ ~ SMA(o3,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(o3,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(o3,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(o3,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(o3,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(o3,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(circ ~ SMA(o3,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ SMA(o3,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ SMA(o3,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(circ ~ SMA(o3,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(circ ~ SMA(o3,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ SMA(o3,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 





### 呼吸系统pm2，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(resp ~ pm2 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(pm2, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(pm2, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(pm2, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(pm2, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(pm2, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(pm2, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(pm2, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(resp ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 呼吸系统pm2，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ SMA(pm2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(pm2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(pm2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(pm2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(pm2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(pm2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(resp ~ SMA(pm2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(pm2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(pm2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(pm2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(pm2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(pm2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(resp ~ SMA(pm2,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(pm2,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(pm2,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(pm2,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(pm2,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(pm2,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 


### 呼吸系统pm1，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(resp ~ pm1 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(pm1, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(pm1, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(pm1, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(pm1, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(pm1, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(pm1, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(pm1, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(resp ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 呼吸系统pm1，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ SMA(pm1,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(pm1,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(pm1,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(pm1,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(pm1,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(pm1,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(resp ~ SMA(pm1,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(pm1,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(pm1,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(pm1,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(pm1,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(pm1,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(resp ~ SMA(pm1,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(pm1,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(pm1,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(pm1,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(pm1,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(pm1,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 


### 呼吸系统so2，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(resp ~ so2 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(so2, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(so2, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(so2, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(so2, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(so2, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(so2, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(so2, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(resp ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统so2，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ SMA(so2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(so2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(so2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(so2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(so2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(so2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(resp ~ SMA(so2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(so2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(so2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(so2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(so2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(so2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(resp ~ SMA(so2,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(so2,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(so2,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(so2,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(so2,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(so2,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 



### 呼吸系统o3，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(resp ~ o3 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(o3, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(o3, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(o3, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(o3, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(o3, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(o3, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(o3, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(resp ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(resp ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(resp ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(resp ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(resp ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(resp ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(resp ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(resp ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(resp ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 呼吸系统o3，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(resp ~ SMA(o3,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(o3,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(o3,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(o3,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(o3,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(o3,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(resp ~ SMA(o3,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(o3,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(o3,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(o3,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(o3,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(o3,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(resp ~ SMA(o3,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(resp ~ SMA(o3,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(resp ~ SMA(o3,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(resp ~ SMA(o3,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(resp ~ SMA(o3,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(resp ~ SMA(o3,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 



### 全疾病pm2，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(icd ~ pm2 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(pm2, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(pm2, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(pm2, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(pm2, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(pm2, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(pm2, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(pm2, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(icd ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 全疾病pm2，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ SMA(pm2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(pm2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(pm2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(pm2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(pm2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(pm2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(icd ~ SMA(pm2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(pm2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(pm2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(pm2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(pm2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(pm2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(icd ~ SMA(pm2,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(pm2,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(pm2,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(pm2,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(pm2,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(pm2,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 


### 全疾病pm1，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(icd ~ pm1 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(pm1, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(pm1, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(pm1, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(pm1, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(pm1, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(pm1, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(pm1, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(icd ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm1 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(pm1, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(pm1, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(pm1, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(pm1, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(pm1, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(pm1, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(pm1, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 


### 全疾病pm1，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ SMA(pm1,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(pm1,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(pm1,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(pm1,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(pm1,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(pm1,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(icd ~ SMA(pm1,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(pm1,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(pm1,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(pm1,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(pm1,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(pm1,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(icd ~ SMA(pm1,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(pm1,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(pm1,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(pm1,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(pm1,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(pm1,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 


### 全疾病so2，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(icd ~ so2 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(so2, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(so2, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(so2, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(so2, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(so2, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(so2, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(so2, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(icd ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(so2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(so2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(so2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(so2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(so2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(so2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(so2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病so2，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ SMA(so2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(so2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(so2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(so2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(so2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(so2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(icd ~ SMA(so2,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(so2,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(so2,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(so2,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(so2,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(so2,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(icd ~ SMA(so2,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(so2,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(so2,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(so2,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(so2,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(so2,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 



### 全疾病o3，单滞后 ####
# Temp6，RH3，Time6 ####
f0 <- gam(icd ~ o3 + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(o3, 1) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(o3, 2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(o3, 3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(o3, 4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(o3, 5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(o3, 6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(o3, 7) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time6 ####
f0 <- gam(icd ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

# Temp3，RH3，Time7 ####
f0 <- gam(icd ~ o3 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(icd ~ Lag(o3, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(icd ~ Lag(o3, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(icd ~ Lag(o3, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(icd ~ Lag(o3, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(icd ~ Lag(o3, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(icd ~ Lag(o3, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(icd ~ Lag(o3, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 



### 全疾病o3，滑动平均滞后 ####
# Temp3，RH3，Time7 ####
f01 <- gam(icd ~ SMA(o3,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(o3,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(o3,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(o3,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(o3,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(o3,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp3，RH3，Time6 ####
f01 <- gam(icd ~ SMA(o3,n=2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(o3,n=3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(o3,n=4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(o3,n=5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(o3,n=6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(o3,n=8) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

# Temp6，RH3，Time6 ####
f01 <- gam(icd ~ SMA(o3,n=2) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(icd ~ SMA(o3,n=3) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(icd ~ SMA(o3,n=4) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(icd ~ SMA(o3,n=5) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(icd ~ SMA(o3,n=6) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(icd ~ SMA(o3,n=8) + s(mean_temp, k=7) + s(rh, k=4) + s(time, k=6*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07))
lapply(summ_list2, coef.exat) 

### 异常值识别
attach(analysis)
x <- scan(analysis$I10)  # 定义变量X读取数据
names(x) <- 1 : length(x)  # 给每个数据编号
boxplot(x)  # 绘制箱线图
out.vals = boxplot(x)$out  # 获取异常值信息
print(out.vals)  #  输出异常值信息
for( i in out.vals)  # 在图上标注异常点的值
{
  text(i, adj = -0.2, labels = i)
}
boxplot(I10)$out # 索引out可以直接得到异常值有哪些
boxplot(I20)$out
boxplot(I44)$out
boxplot(I50)$out
boxplot(I60)$out
boxplot(icd)$out
boxplot(circ)$out
hist(resp2)
hist(I60)
attach(analysis)
detach(analysis)


