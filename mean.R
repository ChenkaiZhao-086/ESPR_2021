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