library(readxl)
rr_plot <- read_excel("RR_plot.xlsx", col_types = c("numeric", "numeric", "text", "text"))
rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","4","5","6","7","01","02","03","04","05","07"))
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE","RESP","pneumonia","Chronic lower respiratory disease"))

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



rr_plot2 <- read_excel("RR_plot.xlsx", sheet="Sheet4",col_types = c("numeric", "numeric", "text", "text"))
rr_plot2 <- as.data.frame(rr_plot2)
rr_plot2$lagdays <- as.factor(rr_plot2$lagdays)
rr_plot2$icd_group <- as.factor(rr_plot2$icd_group)
rr_plot2$icd_group <- ordered(rr_plot2$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE"))

rrplot2 <- ggplot(rr_plot2,aes(x=icd_group, y=(exp(b)^10))) +
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10))) + #,position = position_dodge(0.5)
  labs(y="RR(95%CI)", title = expression()) +
  theme_classic() +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(2)), 
        axis.text=element_text(size=rel(2)))
rrplot2

## 双变量分面

library(readxl)
rr_plot <- read_excel("RR_plot.xlsx", sheet = "crossbasis")
#View(rr_plot)

rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","4","5","6","7","01","02","03","04","05","07"))
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE"))
rr_plot$lagtype <- as.factor(rr_plot$lagtype)
rr_plot$lagtype <- ordered(rr_plot$lagtype,levels=c("Single","Moving_Average"))


ggplot(rr_plot,aes(x=lagdays, y=(exp(b)^10)))+
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10)))+#,position = position_dodge(0.5)
  facet_grid(icd_group~lagtype, scales = "free")+
  theme_classic() +
  labs(x="Lag(days)", y="RR(95%CI)", title = expression()) +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(1.5)), 
        axis.text=element_text(size=rel(1.5)),
        strip.text=element_text(size=rel(1.2))) 

rr_plot <- read_excel("RR_plot.xlsx", sheet = "cb_resp")
#View(rr_plot)

rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","4","5","6","7","01","02","03","04","05","07"))
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("RESP","pneumonia","CLRD"))
rr_plot$lagtype <- as.factor(rr_plot$lagtype)
rr_plot$lagtype <- ordered(rr_plot$lagtype,levels=c("Single","Moving_Average"))


ggplot(rr_plot,aes(x=lagdays, y=(exp(b)^10)))+
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10)))+#,position = position_dodge(0.5)
  facet_grid(icd_group~lagtype, scales = "free")+
  theme_classic() +
  labs(x="Lag(days)", y="RR(95%CI)", title = expression()) +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(1.5)), 
        axis.text=element_text(size=rel(1.5)),
        strip.text=element_text(size=rel(1.2))) 
  
## 冷暖
library(readxl)
rr_plot <- read_excel("RR_plot.xlsx", sheet = "wc")
#View(rr_plot)

rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","4","5","6","7","01","02","03","04","05","07"))
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE"))
rr_plot$lagtype <- as.factor(rr_plot$lagtype)
rr_plot$lagtype <- ordered(rr_plot$lagtype,levels=c("Single","Moving_Average"))
rr_plot$temp <- as.factor(rr_plot$temp)
rr_plot$temp <- ordered(rr_plot$temp,levels=c("warm","cold"))


ggplot(rr_plot,aes(x=lagdays, y=(exp(b)^10),color=temp))+
  scale_color_manual(values=c("#B7141F","#242B8D"))+
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10)),position = position_dodge(0.5))+
  facet_grid(icd_group~lagtype, scales = "free")+
  theme_classic() +
  labs(x="Lag(days)", y="RR(95%CI)", title = expression()) +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(1.5)), 
        axis.text=element_text(size=rel(1.5)),
        strip.text=element_text(size=rel(0.9))) 

library(readxl)
rr_plot <- read_excel("RR_plot.xlsx", sheet = "wc_resp")
#View(rr_plot)

rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","4","5","6","7","01","02","03","04","05","07"))
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("RESP","pneumonia","CLRD"))
rr_plot$lagtype <- as.factor(rr_plot$lagtype)
rr_plot$lagtype <- ordered(rr_plot$lagtype,levels=c("Single","Moving_Average"))
rr_plot$temp <- as.factor(rr_plot$temp)
rr_plot$temp <- ordered(rr_plot$temp,levels=c("warm","cold"))


ggplot(rr_plot,aes(x=lagdays, y=(exp(b)^10),color=temp))+
  scale_color_manual(values=c("#B7141F","#242B8D"))+
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10)),position = position_dodge(0.5))+
  facet_grid(icd_group~lagtype, scales = "free")+
  theme_classic() +
  labs(x="Lag(days)", y="RR(95%CI)", title = expression()) +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(1.5)), 
        axis.text=element_text(size=rel(1.5)),
        strip.text=element_text(size=rel(0.9))) 

## 调整污染物
library(readxl)
rr_plot <- read_excel("RR_plot.xlsx", sheet = "adj")
#View(rr_plot)
rr_plot <- as.data.frame(rr_plot)
rr_plot$lagdays <- as.factor(rr_plot$lagdays)
rr_plot$lagdays <- ordered(rr_plot$lagdays, levels=c("0","1","2","3","7","01","02","03","07"))
rr_plot$icd_group <- as.factor(rr_plot$icd_group)
rr_plot$icd_group <- ordered(rr_plot$icd_group,levels=c("CVDED","Hypertension","IHD","Arrhythmia","HF","CVE","RESP","pneumonia","CLRD"))
rr_plot$lagtype <- as.factor(rr_plot$lagtype)
rr_plot$lagtype <- ordered(rr_plot$lagtype,levels=c("Single","Moving_Average"))
rr_plot$poll <- as.factor(rr_plot$poll)
rr_plot$poll <- ordered(rr_plot$poll,levels=c("PM2.5","+SO2","+NO2","+CO","+O3"))


ggplot(rr_plot,aes(x=lagdays, y=(exp(b)^10),color=poll))+
  scale_color_manual(values=c("#DC5C60","#303AA6","#26BBD1","#56B157","#FEC020"))+
  geom_hline(aes(yintercept=1),colour="#999999") +
  geom_pointrange(aes(ymin=(exp(b-1.96*se)^10), ymax=(exp(b+1.96*se)^10)),position = position_dodge(0.8))+
  facet_grid(icd_group~lagtype, scales = "free")+
  theme_classic() +
  labs(x="Lag(days)", y="RR(95%CI)", title = expression()) +
  theme(plot.title = element_text(size=rel(2.5), hjust=0.5), 
        axis.title=element_text(size=rel(1.5)), 
        axis.text=element_text(size=rel(1.5)),
        strip.text=element_text(size=rel(0.9))) 
