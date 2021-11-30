library(readr)
library(dplyr)
newpoll17 <- read_csv("raw_data/原始污染物数据和1415气象数据/17pollutants.csv", 
                      col_types = cols(date = col_date(format = "%Y/%m/%d")))
View(newpoll17)
newpoll17 <- as.data.frame(newpoll17)

a <- c(seq(from=1, to=length(newpoll17$date), by=11)) # 建立一个每隔11行的数据序列
# newpoll17 %>% filter(row_number()==1) # dplyr包中根据行号进行筛选
newpoll17 <- newpoll17[-a,]

newpoll18 <- read_csv("raw_data/原始污染物数据和1415气象数据/18pollutants.csv", 
                      col_types = cols(date = col_date(format = "%Y/%m/%d")))
View(newpoll18)
newpoll18 <- as.data.frame(newpoll18)

b <- c(seq(from=6, to=length(newpoll18$date), by=11)) 
newpoll18 <- newpoll18[-b,]
#####
newpoll17$year <- year(newpoll17$date) 
newpoll17$month <- month(newpoll17$date)
newpoll17$day <- day(newpoll17$date)
newpoll17$YMD <- factor(paste(newpoll17$year, newpoll17$month, newpoll17$day, sep = "-" )) 

date_list <- c()
pm2_result <- c()
pm1_result <- c()
so2_result <- c()
no2_result <- c()
co_result <-c()
o31_result <- c()
o38_result <- c()

for (i in unique(newpoll17$YMD)){ 
  date_extract <- newpoll17[newpoll17$YMD == i, ] 
  pm2_mean <- mean(date_extract$pm2, na.rm = T)
  pm1_mean <- mean(date_extract$pm1, na.rm = T)
  so2_mean <- mean(date_extract$so2, na.rm =T)
  no2_mean <- mean(date_extract$no2, na.rm = T)
  co_mean <- mean(date_extract$co, na.rm = T)
  o31_mean <- mean(date_extract$o31, na.rm = T)
  o38_mean <- mean(date_extract$o38, na.rm = T)
  date_list <- c(date_list,i) 
  pm2_result <- c(pm2_result, pm2_mean)
  pm1_result <- c(pm1_result, pm1_mean)
  so2_result <- c(so2_result, so2_mean)
  no2_result <- c(no2_result, no2_mean)
  co_result <-c(co_result, co_mean)
  o31_result <- c(o31_result, o31_mean)
  o38_result <- c(o38_result, o38_mean)
}
result17 <- data.frame(date=date_list, pm2=pm2_result, pm1=pm1_result, so2=so2_result,
                       no2=no2_result, co=co_result, o31=o31_result, o38=o38_result) 
result17
write.csv(result17, "17poll.csv")
#####
newpoll18$year <- year(newpoll18$date) 
newpoll18$month <- month(newpoll18$date)
newpoll18$day <- day(newpoll18$date)
newpoll18$YMD <- factor(paste(newpoll18$year, newpoll18$month, newpoll18$day, sep = "-" )) 

date_list <- c()
pm2_result <- c()
pm1_result <- c()
so2_result <- c()
no2_result <- c()
co_result <-c()
o31_result <- c()
o38_result <- c()

for (i in unique(newpoll18$YMD)){ 
  date_extract <- newpoll18[newpoll18$YMD == i, ] 
  pm2_mean <- mean(date_extract$pm2, na.rm = T)
  pm1_mean <- mean(date_extract$pm1, na.rm = T)
  so2_mean <- mean(date_extract$so2, na.rm =T)
  no2_mean <- mean(date_extract$no2, na.rm = T)
  co_mean <- mean(date_extract$co, na.rm = T)
  o31_mean <- mean(date_extract$o31, na.rm = T)
  o38_mean <- mean(date_extract$o38, na.rm = T)
  date_list <- c(date_list,i) 
  pm2_result <- c(pm2_result, pm2_mean)
  pm1_result <- c(pm1_result, pm1_mean)
  so2_result <- c(so2_result, so2_mean)
  no2_result <- c(no2_result, no2_mean)
  co_result <-c(co_result, co_mean)
  o31_result <- c(o31_result, o31_mean)
  o38_result <- c(o38_result, o38_mean)
}
result18 <- data.frame(date=date_list, pm2=pm2_result, pm1=pm1_result, so2=so2_result,
                       no2=no2_result, co=co_result, o31=o31_result, o38=o38_result) 
result18
#####
poll <- rbind(result17, result18)
poll$date <- as.Date(poll$date, format = "%Y-%m-%d")
ED <- read_csv("disposal/筛选后病例数据/ED.csv", col_types = cols(X1 = col_skip(), date = col_date(format = "%Y-%m-%d")))
ED <- subset(ED, year(ED$date)==2017|year(ED$date)==2018)
ED <- as.data.frame(ED)
analysis <- merge(ED, poll, all.y = T)
poll_all2 <- read_csv("disposal/整理后气象及污染数据/poll_all2.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
poll_all2 <- as.data.frame(poll_all2)
poll_all2$date <- as.Date(poll_all2$date, format = "%Y-%m-%d")
analysis <- merge(analysis, poll_all2, all.x = T)
analysis <- read_csv("analysis_x.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
analysis$time <- 1:length(analysis$date) 
#####
tab <- data.frame(NAME=NA, Mean=NA, SD=NA, Minimum=NA, '10th'=NA, '25th'=NA,'50th'=NA,'75th'=NA,'90th'=NA,Maximum=NA) # 初始化数据框
for(i in 8:25){
  meaan <- round(mean(analysis[,i]),2) # round()函数用于四舍五入的保留两位小数
  sdd <- round(sd(analysis[,i]),2)
  quant <- round(quantile(analysis[,i], c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1)))
  name <- names(analysis[i])
  tab[i,] <- data.frame(NAME=name, Mean=meaan, SD=sdd, Minimum=quant[1], '10th'=quant[2], '25th'=quant[3],
                        '50th'=quant[4],'75th'=quant[5],'90th'=quant[6],Maximum=quant[7])
}
tab <- tab[-c(1:7),]
tab
#####
f0 <- gam(I10 ~ pm2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I10 ~ Lag(pm2, 1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I10 ~ Lag(pm2, 2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I10 ~ Lag(pm2, 3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(I10 ~ Lag(pm2, 4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(I10 ~ Lag(pm2, 5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(I10 ~ Lag(pm2, 6) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I10 ~ Lag(pm2, 7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
lapply(summ_list, coef.exat) 

f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(I10 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(I10 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(I20 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(I20 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(I44 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(I44 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f04 <- gam(I50 ~ tsModel::runMean(pm2,0:4) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f05 <- gam(I50 ~ tsModel::runMean(pm2,0:5) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f04), summary(f05), summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 

### 循环系统pm2 + so2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(circ ~ pm2 + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm2, 1) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm2, 2) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm2, 3) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm2, 7) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I10 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f1 <- gam(I10 ~ Lag(pm2, 1) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f2 <- gam(I10 ~ Lag(pm2, 2) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f3 <- gam(I10 ~ Lag(pm2, 3) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f7 <- gam(I10 ~ Lag(pm2, 7) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + so2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + so2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I20 ~ Lag(pm2, 1) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I20 ~ Lag(pm2, 2) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I20 ~ Lag(pm2, 3) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I20 ~ Lag(pm2, 7) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + so2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I44 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I44 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I44 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I44 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I44 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I50 pm2 + so2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I50 ~ pm2 + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I50 ~ Lag(pm2, 1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I50 ~ Lag(pm2, 2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I50 ~ Lag(pm2, 3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I50 ~ Lag(pm2, 7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I50 pm2 + so2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I50 ~ tsModel::runMean(pm2,0:1) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I50 ~ tsModel::runMean(pm2,0:2) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I50 ~ tsModel::runMean(pm2,0:3) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I50 ~ tsModel::runMean(pm2,0:7) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I50 ~ tsModel::runMean(pm2,0:60) + so2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
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
f0 <- gam(circ ~ pm2 + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ Lag(pm2, 1) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm2, 2) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm2, 3) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm2, 7) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### 循环系统pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(circ ~ tsModel::runMean(pm2,0:1) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(circ ~ tsModel::runMean(pm2,0:2) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(circ ~ tsModel::runMean(pm2,0:3) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(circ ~ tsModel::runMean(pm2,0:7) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(circ ~ tsModel::runMean(pm2,0:60) + no2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 




### I10 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I10 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I10 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I10 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I10 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I10 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I10 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I10 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I10 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I10 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I10 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I10 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 


### I20 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I20 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I20 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I20 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I20 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I20 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I20 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I20 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I20 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I20 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I20 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I20 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f01), summary(f02), summary(f03),summary(f07), summary(f060))
lapply(summ_list2, coef.exat) 





### I44 pm2 + no2，单滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f0 <- gam(I44 ~ pm2 + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f1 <- gam(I44 ~ Lag(pm2, 1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(I44 ~ Lag(pm2, 2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(I44 ~ Lag(pm2, 3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(I44 ~ Lag(pm2, 7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f7))
lapply(summ_list, coef.exat) 

### I44 pm2 + no2，滑动平均滞后，2017-2018 ####
# Temp3，RH3，Time7 ####
f01 <- gam(I44 ~ tsModel::runMean(pm2,0:1) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f02 <- gam(I44 ~ tsModel::runMean(pm2,0:2) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f03 <- gam(I44 ~ tsModel::runMean(pm2,0:3) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f07 <- gam(I44 ~ tsModel::runMean(pm2,0:7) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f060 <- gam(I44 ~ tsModel::runMean(pm2,0:60) + no2 + cbt + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
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








