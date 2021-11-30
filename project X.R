setwd("~/Documents/data")
library(readr)
library(ggplot2)
library(gridExtra) # 多个ggplot2放到一张图中
library(lubridate)
library(dplyr)
library(mgcv)
library(dlnm)
library(splines)
library(MuMIn)
library(tsModel)
library(TTR)
load("/Users/chenkaizhao/Documents/data/data.RData") # 装载工作环境
################ 数据前处理 ####
test1 <- read_csv("test1.csv", col_types = cols(date = col_date(format = "%Y/%m/%d"), diagnose = col_skip(), id = col_skip()))
test1 <- as.data.frame(test1)
summary(test1)
############# 计算每日病例数、写csv文件和转换为数据框格式
total <- table(test1$date) #统计每日的总病例数
total <- as.data.frame(total)
names(total) <- c("date", "icd")
total$icd <- as.integer(total$icd)
total$date <- as.Date(total$date, "%Y-%m-%d")
# write.csv(total, file = "total.csv") 
# classify <- table(test1$date, test1$icd) #统计每一日每个ICD下有多少病例数
# names(classify)[names(classify) == 'X1'] <- 'date' #经典的改列名方式；也可用colnames(classify) <- "new.name"
write.csv(classify, file = "classify.csv")
classify <- read_csv("classify.csv", col_types = cols(I10 = col_integer(), I20 = col_integer(), I44 = col_integer(), I50 = col_integer(), I60 = col_integer(), 
                                   J00 = col_skip(), J18 = col_integer(), J40 = col_integer(), date = col_date(format = "%Y/%m/%d")))
classify <- as.data.frame(classify)
################ 绘制病例ts图 ####
all <- ggplot(total, aes(x = date, y = icd)) + geom_line()
all
I10ts <- ggplot(classify, aes(x=date, y=I10)) + geom_line()
I10ts
I20ts <- ggplot(classify, aes(x=date, y=I20)) + geom_line()
I20ts
I44ts <- ggplot(classify, aes(x=date, y=I44)) + geom_line()
I44ts
I50ts <- ggplot(classify, aes(x=date, y=I50)) + geom_line()
I50ts
I60ts <- ggplot(classify, aes(x=date, y=I60)) + geom_line()
I60ts
J18ts <- ggplot(classify, aes(x=date, y=J18)) + geom_line()
J18ts
J40ts <- ggplot(classify, aes(x=date, y=J40)) + geom_line()
J40ts
################ 导入污染物数据,计算污染物浓度日均值 ####
library(redar)
library(lubridate)  # lubridate包用于提取时间数据重的年月日

poll13 <- read_csv("13pollutants.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
poll13 <- as.data.frame(poll13)

poll13$year <- year(poll13$date) #提取年、月、日，此处的year()\month()\day()都来源于lubridate包
poll13$month <- month(poll13$date)
poll13$day <- day(poll13$date)
# series <- seq.Date(from = as.Date("2013-01-01",format = "%Y-%m-%d"), by = "day", length.out = 365) #制作从2013-1-1到2013-12-31的日期序列；这一语句在此处没用

poll13$YMD <- factor(paste(poll13$year, poll13$month, poll13$day, sep = "-" )) #添加因子型的年月日标记
# 代码升级，之前这一系列创建日期因子的步骤可以使用poll13$date <- factor(poll13$date)实现，最终结果是相同的

date_list <- c()
pm2_result <- c()

for (i in unique(poll13$YMD)){  # unique返回一个向量、数据框或者数组，删除了重复的部分
  date_extract <- poll13[poll13$YMD == i, ] 
  # 连等号表示判断，这一语句的意思是在i等于第一天的值的时候，把对应第一天的行筛选出来；i等于第二天的值的时候把第二天的筛选出来。将这个语句拓展一下，在日常使用中将i替换为具体的数值等也可以使用
  # 比如print(poll13[poll13$day==31, ]这个语句就筛选出了说有的31号，相当于把所有的闰月筛选出来
  pm2_mean <- mean(date_extract$pm2, na.rm = T)
  date_list <- c(date_list,i) 
  # 这一代码用于制作一个包含全部日期的向量，这一语句设计的很巧妙，最开始的date_list是一个空向量，c(date_list,i)的意思是，date_list的基础上，每进行一次循环就添加一个新的值进去，
  # 添加进去的内容为i，对于这一个循环就是每一次添加本循环用到的日期。这一代码在按日期计算每日污染物浓度均值中也为必须用到，若直接使用poll13$date则会出现一天内有多个日期条目和一个污染物条目，
  # 无法形成正确的数据框。也可以使用上面创建的series，但需要加一行代码删掉多出来的Na；但用series创建也有一个弊端，如果中间有某天缺失，则很难一下判断出缺失的是哪一天。
  pm2_result <- c(pm2_result, pm2_mean)
}
pm2_result<-pm2_result[1:length(pm2_result)-1] # 这一语句用于删除最后此向量最后一个值
pm2 <- data.frame(date=series, pm2=pm2_result) 
# 另一种方法：xx <- data.frame(date=date_list, pm2=pm2_result)此时的日期是因子型变量,需要xx$date <- as.Date(xx$date, "%Y-%m-%d")转换；xx <- xx[-366,]删除最后一行

## 方法2
poll13 <- poll13[order(poll13$date), ] # 首先需要对数据进行排序，某些数据集是按监测站进行排序的，原始excel又被锁定无法更改
result13 <- aggregate(poll13[ ,c(2,3,4,5,6,7)],by = list(poll13[ ,11]),mean,na.rm =T) #前面的部分表示对2-7列的进行计算，后面的list为每11天一组
result13
names(result13)[1] <- c("date")
result13$date <- as.Date(x1$date, "%Y-%m-%d")
result13 <- result13[order(x1$date), ]
write.csv(result13, "13poll.csv")
# 统计满足条件的行数，如poll中pm2的频数 table(poll13$pm2) 计算pm2等于100的日子数  a<-poll13[poll13$pm2 == 0.1, ]
# 如果要计算poll13和poll14中pm2等于100的有多少 则b<-poll13[poll13$pm2 == 100 , ]  length(intersect(a,b)) 【intersect用于取交集】

################ 14年污染物 ########################
poll14 <- as.data.frame(poll14)
names(poll14) <- c("date", "so2", "no2", "co", "o3", "pm1", "pm2")
write.csv(poll14, "14pollutants.csv")

poll14$year <- year(poll14$date) 
poll14$month <- month(poll14$date)
poll14$day <- day(poll14$date)
poll14 <- poll14[order(poll14$date),]

poll14$YMD <- factor(paste(poll14$year, poll14$month, poll14$day, sep = "-" )) 

result14 <- aggregate(poll14[ ,c(2,3,4,5,6,7)],list(poll14[ ,11]),mean,na.rm =T) 
result14
names(result14)[1] <- c("date")
result14$date <- as.Date(result14$date, "%Y-%m-%d")
result14 <- result14[order(result14$date), ]
write.csv(result14, "14poll.csv")
write_csv(X2014气象数据资料_, "14weather.csv")
################ 15年污染物 ######################################
poll15$year <- year(poll15$date) 
poll15$month <- month(poll15$date)
poll15$day <- day(poll15$date)

poll15$YMD <- factor(paste(poll15$year, poll15$month, poll15$day, sep = "-" )) 

date_list <- c()
pm2_result <- c()
pm1_result <- c()
pm2524_result <- c()
pm1024_result <- c()
so2_result <- c()
no2_result <- c()
co_result <-c()
o3_result <- c()
o38_result <- c()

for (i in unique(poll15$YMD)){ 
  date_extract <- poll15[poll15$YMD == i, ] 
  pm2_mean <- mean(date_extract$pm2, na.rm = T)
  pm1_mean <- mean(date_extract$pm1, na.rm = T)
  pm2524_mean <- mean(date_extract$pm2524, na.rm = T)
  pm1024_mean <- mean(date_extract$pm1024, na.rm = T)
  so2_mean <- mean(date_extract$so2, na.rm =T)
  no2_mean <- mean(date_extract$no2, na.rm = T)
  co_mean <- mean(date_extract$co, na.rm = T)
  o3_mean <- mean(date_extract$o3, na.rm = T)
  o38_mean <- mean(date_extract$o38, na.rm = T)
  date_list <- c(date_list,i) 
  pm2_result <- c(pm2_result, pm2_mean)
  pm1_result <- c(pm1_result, pm1_mean)
  pm2524_result <- c(pm2524_result, pm2524_mean)
  pm1024_result <- c(pm1024_result, pm1024_mean)
  so2_result <- c(so2_result, so2_mean)
  no2_result <- c(no2_result, no2_mean)
  co_result <-c(co_result, co_mean)
  o3_result <- c(o3_result, o3_mean)
  o38_result <- c(o38_result, o38_mean)
}
result15 <- data.frame(date=date_list, pm2=pm2_result, pm1=pm1_result, pm2524=pm2524_result, pm1024=pm1024_result, so2=so2_result,
                       no2=no2_result, co=co_result, o3=o3_result, o38=o38_result) 
result15
result15$date <- as.Date(result15$date, "%Y-%m-%d")
write.csv(result15, "15poll.csv")
names(wea15) <- c("date","mean_hpa", "max_hpa", "min_hpa", "mean_temp", "max_temp", "min_temp", "rh", "max_h", "min_h", "rain", "wind", "sun")
write_csv(wea15, "15weather.csv")
x15poll <- merge(result15, wea15, by = "date", all.y = T)
write_csv(x15poll, "15poll.csv")
################ 16年污染物 ########################
poll16$year <- year(poll16$date) 
poll16$month <- month(poll16$date)
poll16$day <- day(poll16$date)
poll16$YMD <- factor(paste(poll16$year, poll16$month, poll16$day, sep = "-" )) 

date_list <- c()
pm2_result <- c()
pm1_result <- c()
so2_result <- c()
no2_result <- c()
co_result <-c()
o31_result <- c()
o38_result <- c()

for (i in unique(poll16$YMD)){ 
  date_extract <- poll16[poll16$YMD == i, ] 
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
result16 <- data.frame(date=date_list, pm2=pm2_result, pm1=pm1_result, so2=so2_result,
                       no2=no2_result, co=co_result, o31=o31_result, o38=o38_result) 
result16
write.csv(result16, "16poll.csv")
################ 17年污染物 #####################
poll17$year <- year(poll17$date) 
poll17$month <- month(poll17$date)
poll17$day <- day(poll17$date)
poll17$YMD <- factor(paste(poll17$year, poll17$month, poll17$day, sep = "-" )) 

date_list <- c()
pm2_result <- c()
pm1_result <- c()
so2_result <- c()
no2_result <- c()
co_result <-c()
o31_result <- c()
o38_result <- c()

for (i in unique(poll17$YMD)){ 
  date_extract <- poll17[poll17$YMD == i, ] 
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
################ 18年污染物 ######################
poll18$year <- year(poll18$date) 
poll18$month <- month(poll18$date)
poll18$day <- day(poll18$date)
poll18$YMD <- factor(paste(poll18$year, poll18$month, poll18$day, sep = "-" )) 

date_list <- c()
pm2_result <- c()
pm1_result <- c()
so2_result <- c()
no2_result <- c()
co_result <-c()
o31_result <- c()
o38_result <- c()

for (i in unique(poll18$YMD)){ 
  date_extract <- poll18[poll18$YMD == i, ] 
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
write.csv(result18, "18poll.csv")

################ 合并病例和污染物数据并绘制ts图 ####
classify <- read_csv("classify.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
total <- read_csv("total.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
classify <- as.data.frame(classify)
total <- as.data.frame(total)
ED <- merge(total, classify)
write.csv(ED, "ED.csv")
poll <- read_csv("poll_all.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
analysis <- merge(ED, poll, all.x = T)
write.csv(analysis, "analysis.csv")
pm2ts <- ggplot(analysis, na.rm = T, aes(x = date, y = pm2)) + geom_line()
pm2ts
pm1ts <- ggplot(analysis, na.rm = T, aes(x = date, y = pm1)) + geom_line()
pm1ts
so2ts <- ggplot(analysis, na.rm = T, aes(x = date, y = so2))+ geom_line()
so2ts
no2ts <- ggplot(analysis, na.rm = T, aes(x = date, y = no2)) + geom_line()
no2ts
cots <- ggplot(analysis, na.rm = T, aes(x = date, y = co)) + geom_line()
cots
o3ts <- ggplot(analysis, na.rm = T, aes(x = date, y = o3)) + geom_line()
o3ts
meanhpats <- ggplot(analysis, na.rm = T, aes(x = date, y = mean_hpa)) + geom_line()
meanhpats
meantempts <- ggplot(analysis, na.rm = T, aes(x = date, y = mean_temp)) + geom_line()
meantempts
rhts <- ggplot(analysis, na.rm = T, aes(x = date, y = rh)) + geom_line()
rhts
######################### 添加年月日 ####
attach(analysis)
analysis$year <- year(date)
analysis$month <- month(date)
analysis$day <- day(date)
analysis$dow <- wday(date, label = T, abbr = T) # 用于生成dow，直接将对应日期进行转化为星期
analysis$time <- seq(1, length(date))
detach(analysis)
write.csv(analysis, "analysis.csv")
analysis <- read_csv("analysis.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
analysis <- as.data.frame(analysis)

################ 计算基础表格 ####
dat <- read_excel("combine2.xlsx", sheet = "Sheet2", 
                  col_types = c("numeric", "text", "text", 
                                "numeric", "numeric", "text", "text"))
dat <- as.data.frame(dat)

mean(analysis3[,11])

dat$age <- as.numeric(dat$age, na.rm=T)
mean(dat$age, na.rm = T)
sd(dat$age,na.rm = T)
table(dat$sex)
26391/71740*100
37786/71740*100
7563/71740*100 
length(subset(dat,age <= 18, select = id:icd)$age)/70741*100
length(subset(dat,age >= 19 & age <= 60, select = id:icd)$age)/71740*100
length(subset(dat,age >= 61 & age <= 80, select = id:icd)$age)/71740*100
length(subset(dat,age > 80 , select = id:icd)$age)/71740*100
10117/71740*100 
4922/71740*100 
17650/71740*100 
4222/71740*100 
8350/71740*100 
36569/71740*100 
# Table 1完成


analysis32 <- subset(analysis3,select = -c(resp,J00,mean_hpa,min_hpa,max_hpa,min_temp,max_temp,min_h,max_h,rain,wind))
# analysis3[,-which(names(analysis3)%in%c("resp","J00")] # 与上一语句等价

## 全年
tab <- data.frame(NAME=NA, Mean=NA, SD=NA, Minimum=NA, '10th'=NA, '25th'=NA,'50th'=NA,'75th'=NA,'90th'=NA,Maximum=NA) # 初始化数据框
for(i in 8:25){
  meaan <- round(mean(analysis32[,i]),2) # round()函数用于四舍五入的保留两位小数
  sdd <- round(sd(analysis32[,i]),2)
  quant <- round(quantile(analysis32[,i], c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1)))
  name <- names(analysis32[i])
  tab[i,] <- data.frame(NAME=name, Mean=meaan, SD=sdd, Minimum=quant[1], '10th'=quant[2], '25th'=quant[3],
                        '50th'=quant[4],'75th'=quant[5],'90th'=quant[6],Maximum=quant[7])
}
tab <- tab[-c(1:7),]
tab

## 温暖
ana_warm <- subset(analysis3, month >= 4 & month<=10)
tab_warm <- data.frame(NAME=NA, Mean=NA, SD=NA, Minimum=NA, '10th'=NA, '25th'=NA,'50th'=NA,'75th'=NA,'90th'=NA,Maximum=NA) # 初始化数据框
for(i in 8:25){
  meaan <- round(mean(ana_warm[,i]),2) 
  sdd <- round(sd(ana_warm[,i]),2)
  quant <- round(quantile(ana_warm[,i], c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1)))
  name <- names(ana_warm[i])
  tab_warm[i,] <- data.frame(NAME=name, Mean=meaan, SD=sdd, Minimum=quant[1], '10th'=quant[2], '25th'=quant[3],
                             '50th'=quant[4],'75th'=quant[5],'90th'=quant[6],Maximum=quant[7])
}
tab_warm <- tab_warm[-c(1:7),]
tab_warm

## 寒冷
ana_cold <- subset(analysis3, month < 4 | month>10)
tab_cold <- data.frame(NAME=NA, Mean=NA, SD=NA, Minimum=NA, '10th'=NA, '25th'=NA,'50th'=NA,'75th'=NA,'90th'=NA,Maximum=NA) # 初始化数据框
for(i in 8:25){
  meaan <- round(mean(ana_cold[,i]),2) 
  sdd <- round(sd(ana_cold[,i]),2)
  quant <- round(quantile(ana_cold[,i], c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1)))
  name <- names(ana_cold[i])
  tab_cold[i,] <- data.frame(NAME=name, Mean=meaan, SD=sdd, Minimum=quant[1], '10th'=quant[2], '25th'=quant[3],
                             '50th'=quant[4],'75th'=quant[5],'90th'=quant[6],Maximum=quant[7])
}
tab_cold <- tab_cold[-c(1:7),]
tab_cold

################ 污染物与气象因素的相关性矩阵
library(Hmisc)
ana_corr <- subset(analysis32, select = pm2:rh)
rcorr(as.matrix(ana_corr[, c(1:8)]), type = "spearman")  #回归，所有分类两两回归
## 方法2
library(PerformanceAnalytics)
chart.Correlation(as.matrix(ana_corr[, c(1:8)]), histogram = F, method = "spearman", pch=19)


################ 快速读取analysis数据集####
analysis <- read_csv("analysis2.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
analysis <- as.data.frame(analysis)

analysis <- read_csv("analysis3res3.csv", col_types = cols(date = col_date(format = "%Y/%m/%d")))
analysis <- as.data.frame(analysis)

analysis <- read_excel("analysis3.xlsx")
analysis <- as.data.frame(analysis)
 
## 提取指定年份数据集
analysis2 <- subset(analysis, year==2017, select = date:wind)
analysis3 <- subset(analysis, year==2018|year==2017, select = date:wind)
analysis4 <- subset(analysis, year==2018|year==2017|year==2016, select = date:wind)
analysis5 <- subset(analysis, year==2013|year==2014, select = date:wind)

write.csv(analysis2, "good_dataset_2018.csv")

################ 异常值的诊断 ####
influence.gam(f0) # 得到hat values，hat value大于2倍均值或3倍均值可以认为是强影响点
a <- as.data.frame(influence.gam(f0)) # 形成诊断用数据框
a$num <- 1:length(a$`influence.gam(f0)`)
a$cook <- cooks.distance(f0) # 得到cook's distance，大于4/(n-k-1) 表示为强影响点，n为样本量；k为预测变量数目
a$residuals <- residuals(f0) # 得到残差
head(a)
write.csv(a, "check.csv")


################ 基础模型构建 ps.这一小节内容基于《Environmental Epidicology》构建，类似分布式滞后模型 ####
library(stats) 
par(mfrow = c(2, 1))
acf(analysis$circ, lag.max = 50, main = "(a) ED admitted", ci.col = "black")
library(splines)
fit <- lm(analysis$circ ~ ns(1:2191, 2 * 6))
xr <- resid(fit)
acf(xr, lag.max = 50, main = "(b) ED admitted (seasonality removed)", ci.col = "black") # 计算acf自相关函数

library(mgcv)
fit <- glm(death ~ l1pm10tmean + ns(date,4 * 14) + ns(tmpd, 6) + dow, data = chic,
           na.action = na.exclude, family = quasipoisson)
pr <- predict(fit, type = "terms")

f0 <- glm(circ ~ pm2 + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f1 <- glm(circ ~ Lag(pm2, 1) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f2 <- glm(circ ~ Lag(pm2, 2) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f3 <- glm(circ ~ Lag(pm2, 3) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f4 <- glm(circ ~ Lag(pm2, 4) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
ss <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4))
models <- lapply(ss, function(x) x$coefficients[2,c("Estimate", "Std. Error")]) # lappy用于返回一个与X相同长度的列表，其中的每个元素都是将语句内的function应用到X的相应元素的结果
aaaaaa <- function(x) {x$coefficients[2,c("Estimate", "Std. Error")]}
aaaaaa(ss)
ss$coefficients
### test gam and glm
library(tsModel)
library(quantmod) # 经过测试，tsModel和quantmod中的Lag函数功能一样，任选其一即可
# b <- Lag(analysis$pm2,0:2) 测试Lag函数用
f0 <- glm(circ ~ Lag(pm2, 1) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f00 <- gam(circ ~ Lag(pm2, 1) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
a <- summary(f0)
b <- summary(f00)
coeff <- function(x){
  coeff1 <- data.frame(Estimate = x$p.coeff, Std.Error = x$se, row.names = names(x$p.coeff))
  coeff1[2,]
} # 这一函数可用于提取gam产生的β系数，中间data.frame部分可以直接用于提取b系数.注意function内也可以定义变量名，即使同名不与外部的变量名冲突
coeff(summary(f00))
##
f0 <- gam(circ ~ pm2 + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f1 <- gam(circ ~ Lag(pm2, 1) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f2 <- gam(circ ~ Lag(pm2, 2) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f3 <- gam(circ ~ Lag(pm2, 3) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
f4 <- gam(circ ~ Lag(pm2, 4) + mean_temp + holiday + dow, data = analysis, family = quasipoisson)
ss <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4))
models <- lapply(ss, coeff)



################ 模型的拓展，这一小节基于《Environmental Epidicology》构建 ####
## 通过拟合预测PM10时间序列的模型来选择平滑的自由度
## 为了评估预测PM10的自由度，使用mda包中的bruto函数，通过广义交叉验证评估自由度
library(mda)
pm10 <- analysis$pm2
x <- unclass(analysis$date)
use <- complete.cases(pm10, x)
br.fit <- bruto(x[use], pm10[use]) # 通过自适应反拟合拟合一个可加样条？？？
optimal.df <- br.fit$df
# 估计的自由度是81.03884，对于6年的数据，相当于每年13.5自由度
## 使用gam包的gam函数来拟合健康影响模型。平滑方式选择smooth spline
library(gam)
fit <- gam(circ ~ pm2 + s(time, optimal.df), data = analysis, family = quasipoisson)
v <- gam.exact(fit)
print(v$coefficients["pm2", "Estimate"]) # 估计的对数相对风险
print(v$coefficients["pm2", "A-exact SE"]) # 提取标准误？
## 比较基于因变量和基于自变量的df确定方法，首先是基于自变量(污染物)的确定方法
pm10 <- data$l1pm10tmean
x <- unclass(data$date)
use <- complete.cases(pm10, x)
br.fit <- bruto(x[use], pm10[use]) 
df.pm10 <- br.fit$df
## 接下来是基于因变量(死亡率)
death <- data$death
use <- complete.cases(death, x)
br.fit <- bruto(x[use], death[use]) 
df.death <- br.fit$df
fit1 <- gam(death ~ l1pm10tmean + s(date, df.pm10), data = data, family = quasipoisson)
fit2 <- gam(death ~ l1pm10tmean + s(date, df.death), data = data, family = quasipoisson)
v1 <- gam.exact(fit1) 
v2 <- gam.exact(fit2)
################ 最新的模型，方案I：stage1单污染物,stage2多污染物,stage3温度分层? ####

### 一些常用函数的制作以及一些基础原理的阐述 

## 1、制作可以计算QAIC的quasipoisson分布模式
## 如果不自制x.quasipoisson函数的话QAIC会出现NA，因为原始gam函数没有提供quasipoisson的log-likelihood，因此需要自制函数欺骗一下gam提供log-likelihood
x.quasipoisson <- function(...) { # This is needed to get the likelihood and calculate QAICc
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
}

## 2、
## 自制函数用于提取coefficient，SE和P值
coef.exat <- function(x){ 
  a <- exp(as.data.frame(x$p.coeff)[2,])^10
  b <- exp(as.data.frame(x$se)[2,])^10
  c <- exp(as.data.frame(x$p.coeff)[2,] - 1.96*as.data.frame(x$se)[2,])^10
  d <- exp(as.data.frame(x$p.coeff)[2,] + 1.96*as.data.frame(x$se)[2,])^10
  e <- (exp(as.data.frame(x$p.coeff)[2,]*10)-1)*100
  f <- (exp((as.data.frame(x$p.coeff)[2,] - 1.96*as.data.frame(x$se)[2,])*10) -1) * 100
  g <- (exp((as.data.frame(x$p.coeff)[2,] + 1.96*as.data.frame(x$se)[2,])*10) -1) * 100
  h<- as.data.frame(x$p.pv)[2,]
  i<- data.frame(Estimate=a, Std.Error=b, RR.low=c, RR.high=d,ER=e,ER.low=f,ER.high=g,P=h)
  i
}# 注意function内也可以定义变量名，即使同名不与外部的变量名冲突

## 3、判断是否存在过度离散
## 以下的chat用来计算是否存在过度离散(over-dispersion)，当chat>1时则存在过度离散，因此需要使用quasipoission
chat<-sum(residuals(basemodel2,"pearson")^2)/basemodel$df.residual
chat
# chat <- deviance(basemodel) / df.residual(basemodel) # 另一种计算chat的方法，上一条语句去掉"pearson"后与这一条结果相同

### 选择AIC最小的模型或者用来做敏感性分析 ####
basemodel <- gam(circ ~ s(mean_temp, k = 7,bs = "cr") + s(rh, k = 4,bs = "cr") + s(time, k = 43,bs = "cr") + dow + holiday, 
                 family = "x.quasipoisson", analysis, na.action = na.omit) # 注意这里使用的"x.quasipoisson"原因如上
# basemodel
basemodel1 <- gam(circ ~ s(mean_temp, k = 6) + s(rh, k = 2) + s(time, k = 30) + dow + holiday, family = "x.quasipoisson", analysis)
basemodel2 <- gam(circ ~ s(mean_temp, k = 6) + s(rh, k = 2) + s(time, k = 36) + dow + holiday, family = "x.quasipoisson", analysis)
basemodel3 <- gam(circ ~ s(mean_temp, k = 6) + s(rh, k = 2) + s(time, k = 42) + dow + holiday, family = "x.quasipoisson", analysis)
basemodel4 <- gam(circ ~ s(mean_temp, k = 6) + s(rh, k = 2) + s(time, k = 48) + dow + holiday, family = "x.quasipoisson", analysis)

basemodel1$gcv.ubre
basemodel2$gcv.ubre
basemodel3$gcv.ubre
basemodel4$gcv.ubre
# AIC(basemodel, basemodel2) 计算AIC需要分布类型为poisson，如果为quasipoisson则无法计算
# 因为数据存在过拟合，所以要使用quasipoisson，因此要计算QAIC进行判断

## 对模型进行选择，确定使用rank.args提供模型的chat
quasi.MS<-model.sel(basemodel, basemodel2, rank = QAIC, rank.args = alist(chat = chat))
as.data.frame(quasi.MS)
## 直接得到QAIC，注意要有chat才可以
QAIC(basemodel1,basemodel2, basemodel3, basemodel4, chat = chat)

### 单滞后模型 ####

# pr <- predict(fit, type = "terms") 

f0 <- gam(circ ~ pm2 + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
# 在平滑函数s()中，自由度通过k设定之后自动选择，具体的真实自由度即edf可以使用summary获得。
# 如果要把自由度固定为指定的数值则需要在参数中添加fx=T如s(rh,k=4,fx=T)具体内容见mgcv文档p20choose.k部分和p50gam部分
# 两个操作Tips：
# 1、summary.gam可以查看回归的结果，效果与单独的summary函数相同；
# 2、gam.check()可以做回归诊断，用于判断k值设定的大小是否足够，具体的内容见gam.check的结果，同时也有回归诊断的图
f1 <- gam(circ ~ Lag(pm2, 1) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ Lag(pm2, 2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ Lag(pm2, 3) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ Lag(pm2, 4) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f5 <- gam(circ ~ Lag(pm2, 5) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f6 <- gam(circ ~ Lag(pm2, 6) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
f7 <- gam(circ ~ Lag(pm2, 7) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=7*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis)
summ_list <- list(summary(f0), summary(f1), ssummary(f2),summary(f3), summary(f4),summary(f5), summary(f6), summary(f7))
## lappy用于返回一个与summ_list相同长度的列表，其中的每个元素都是coef.exact提取出的结果
lapply(summ_list, coef.exat) 

## 关于mgcv模型的检查：
layout(matrix(1:4,ncol=2,byrow = T)) # 绘图参数，可以使多个图绘制于一个图层中
# layout(matrix(1)) 改回一个图层一幅图
gam.check() # 首先使用这一函数，可以判断k值以及查看4幅诊断图，上一条语句使4副诊断图出现在一起
plot(fitted(f0), residuals(f0))
plot(analysis$pm2, residuals(f0)) # 之后使用上述两幅残差图对模型进行诊断
# 检查k的另一种方法是使用deviance与平滑项进行拟合
rsd <- residuals(f0,type="deviance")
gam(rsd~s(time,k=20)-1,analysis, select=TRUE)

par(mfrow=c(1,2))
plot(f0,all.terms=TRUE)

### 滑动平均滞后

library(TTR)
# SMA(analysis$pm2, n=3) # 计算lag0-3的滑动平均，计算方法为(x1+x2+x3)/3
# tsModel::runMean(pm2, 0:2)函数与TTR::SMA函数类似，后面的0:2代表了0-3天的滑动平均，与TTR::SMA(pm2,n=3)结果一致
# 需要注意的一点是，SMA要求数据必须是连续的，而runMean可以允许数据有间断.runMean的计算模式为去掉间断日期之后的滑动平均，如1月29日缺失，则1月30日的计算方式为(27+28+30)/3
f0 <- gam(circ ~ SMA(analysis$o3, n=2) + ns(mean_temp, df = 5) + ns(rh, df = 3) + ns(time, df = 42) + dow + holiday, family = "x.quasipoisson", analysis)
f1 <- gam(circ ~ SMA(analysis$o3, n=3) + ns(mean_temp, df = 5) + ns(rh, df = 3) + ns(time, df = 42) + dow + holiday, family = "x.quasipoisson", analysis)
f2 <- gam(circ ~ SMA(analysis$o3, n=4) + ns(mean_temp, df = 5) + ns(rh, df = 3) + ns(time, df = 42) + dow + holiday, family = "x.quasipoisson", analysis)
f3 <- gam(circ ~ SMA(analysis$o3, n=6) + ns(mean_temp, df = 5) + ns(rh, df = 3) + ns(time, df = 42) + dow + holiday, family = "x.quasipoisson", analysis)
f4 <- gam(circ ~ SMA(analysis$o3, n=8) + ns(mean_temp, df = 5) + ns(rh, df = 3) + ns(time, df = 42) + dow + holiday, family = "x.quasipoisson", analysis)
summ_list2 <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4))
lapply(summ_list2, coef.exat) 

sum <- summary(f0)


#plot( data $ death) #绘制时间序列图，以死亡人数为例
#acf( resid( fit1) ) #绘制残差自相关图，以 fit1模型为例
#pacf( resid( fit1) ) #绘制偏自相关图，以 fit1模型为例
#model < -gam( death ~ s( pm10lag01，k = 4，fx = T) + ns( temp，3) + ns( rhum，3) + ns( time，7* 14)+ dow ，family = quasipoisson( ) ，data)
#plot ( model) - 反应关系图


################################### 低温分层
quantile(analysis3$mean_temp, c(0.05,0.1,0.9,0.95))
ana_lowT <- subset(analysis3, mean_temp <= quantile(analysis3$mean_temp, probs=c(0.1)))
ana_highT <- subset(analysis3, mean_temp >= quantile(analysis3$mean_temp, probs=c(0.9)))

####################### 倒入滑动平均后的analysis3数据
library(readxl)
analysis <- read_excel("analysis3.xlsx")
analysis <- as.data.frame(analysis)

cbt <- crossbasis(analysis$mean_temp, lag = 7, 
                  argvar = list(fun="ns", df=6), 
                  arglag = list(knots=logknots(7,fun = "ns", df=3))) # 另一种滞后参数list(fun="ns", df=2)
ft <- gam(circ ~ cbt + ns(rh, df = 3) + ns(time, df = 6*6) + dow + holiday, family = "x.quasipoisson", analysis)
coef.exat(summary(ft))
# knots = logknots() fun="ns", df=2,knots = logknots(6,fun = "ns", df=2))

##按温度分层——低温
cbt2 <- crossbasis(ana_lowT$mean_temp, lag = 7, 
                  argvar = list(fun="ns", df=6), 
                  arglag = list(knots=logknots(7,fun = "ns", df=3)))
ft2 <- gam(circ ~ cbt2 + ns(rh, df = 3) + ns(time, df = 6*6) + dow + holiday, family = "x.quasipoisson", ana_lowT)
coef.exat(summary(ft2))

##按温度分层——高温
cbt3 <- crossbasis(ana_highT$mean_temp, lag = 7, 
                  argvar = list(fun="ns", df=6), 
                  arglag = list(knots=logknots(7,fun = "ns", df=4)))
ft3 <- gam(circ ~ cbt3 + ns(rh, df = 3) + ns(time, df = 5*6) + dow + holiday, family = "x.quasipoisson", ana_highT)
coef.exat(summary(ft3))


############### 绘制RR图
library(readxl)
## 疾病别
rr_plot <- read_excel("RR_plot.xlsx", col_types = c("numeric", "numeric", "text", "text"))
rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","7","01","02","03","07")) #,"060"
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE"))

rrplot <- ggplot(rr_plot,aes(x=lagdays, y=(exp(b)^10))) +
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10))) + #,position = position_dodge(0.5)
  labs(x="Lag(days)", y="RR(95%CI)", title = expression()) +
  theme_classic() +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), # rel用于指定字体相对于其他元素的大小，hjust=0.5用于将文字居中
        axis.title=element_text(size=rel(2)), 
        axis.text=element_text(size=rel(2)),
        strip.text=element_text(size=rel(1.5))) +
  facet_wrap(vars(icd_group), nrow = 3,scales="free") # facet_wrap 用于单变量的分面，facet_grid用于两个变量的分面，scales="free"可以使标度自由
rrplot


## 收获效应
rr_plot2 <- read_excel("RR_plot.xlsx", sheet="Sheet4",col_types = c("numeric", "numeric", "text", "text"))
rr_plot2 <- as.data.frame(rr_plot2)
rr_plot2$lagdays <- as.factor(rr_plot2$lagdays)
rr_plot2$icd_group <- as.factor(rr_plot2$icd_group)
rr_plot2$icd_group <- ordered(rr_plot2$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE"))

rrplot2 <- ggplot(rr_plot2,aes(x=icd_group, y=(exp(b)^10))) +
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10))) + #,position = position_dodge(0.5)
  labs(y="RR(95%CI)", x="Diseases",title = expression()) +
  theme_classic() +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(2)), 
        axis.text=element_text(size=rel(2)))
rrplot2

## 双变量分面备用代码
#b <- year$b
#se <- year$se
#age <- factor(year$var)
#lag <- factor(year$lag)
#pollutant <- factor(year$pollutant)
#lagday <- factor(year$lagday)
#ggplot(year,aes(x=lagday, y=(exp(b)^10),color=Age))+
#  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10),position = position_dodge(0.5)+
#                        facet_grid(pollutants~lag, scales = "free")+
#                        labs(x="Lag(days)", y="RR(95%CI)", title = expression())


## 绘制暴露反应曲线           
expo_resp <- gam(circ ~ s(pm2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp1 <- gam(I10 ~ s(pm2) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp2 <- gam(I20 ~ s(pm2,k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp3 <- gam(I44 ~ s(pm2,k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp4 <- gam(I50 ~ s(pm2,k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp5 <- gam(I60 ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)

# 使用滑动平均lag02计算
expo_resp <- gam(circ ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp1 <- gam(I10 ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp2 <- gam(I20 ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp3 <- gam(I44 ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp4 <- gam(I50 ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)
expo_resp5 <- gam(I60 ~ s(tsModel::runMean(pm2,0:2),k=4,fx=T) + s(mean_temp, k=4) + s(rh, k=4) + s(time, k=2*7+1) + as.factor(dow) + as.factor(holiday), family = "x.quasipoisson", analysis3)

# summary(expo_resp)
par(oma=c(1,2,1,1))
plot(expo_resp,xlab=expression('P'*'M'[2.5]~'('*'µ'*'g'*'/'*'m'^3*')'), 
     ylab="Log relative risk", main="CVDED", cex.axis=rel(2), cex.lab=rel(2), cex.main=rel(2), lwd=1.5)
abline(v=35)
plot(expo_resp1,xlab=expression('P'*'M'[2.5]~'('*'µ'*'g'*'/'*'m'^3*')'), 
     ylab="Log relative risk",main="Hypertension", cex.axis=rel(2), cex.lab=rel(2), cex.main=rel(2), lwd=1.5)
abline(v=35)
plot(expo_resp2,xlab=expression('P'*'M'[2.5]~'('*'µ'*'g'*'/'*'m'^3*')'), 
     ylab="Log relative risk",main="IHD", cex.axis=rel(2), cex.lab=rel(2), cex.main=rel(2), lwd=1.5)
abline(v=35)
plot(expo_resp3,xlab=expression('P'*'M'[2.5]~'('*'µ'*'g'*'/'*'m'^3*')'), 
     ylab="Log relative risk",main="Arrhythmia", cex.axis=rel(2), cex.lab=rel(2), cex.main=rel(2), lwd=1.5)
abline(v=35)
plot(expo_resp4,xlab=expression('P'*'M'[2.5]~'('*'µ'*'g'*'/'*'m'^3*')'), 
     ylab="Log relative risk",main="HF", cex.axis=rel(2), cex.lab=rel(2), cex.main=rel(2), lwd=1.5)
abline(v=35)
plot(expo_resp5,xlab=expression('P'*'M'[2.5]~'('*'µ'*'g'*'/'*'m'^3*')'), 
     ylab="Log relative risk",main="CVE", cex.axis=rel(2), cex.lab=rel(2), cex.main=rel(2), lwd=1.5)
abline(v=35)

gam.check(expo_resp4)
                      